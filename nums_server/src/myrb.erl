%%% RabbitMQ interaction
-module(myrb).

-include_lib("amqp_client.hrl").
-include("rabbit_session.hrl").

-export([start/1]).
-export([teardown/3,send_reply/4]).
-export([send_ack/2]).

-define(SETUP_CONSUMER_TIMEOUT, 10000).

%---------------------------------------------------------------------
-spec start(#rses{}) -> {ok, #rses{}, #conn{}}.

start(Rses) ->
	Host = Rses#rses.host,
	User = Rses#rses.user,
	Password = Rses#rses.password,
	Vhost = Rses#rses.vhost,
	{ok, Connection} = amqp_connection:start(network, #amqp_params{
		username = User,
		password = Password,
		host = Host,
		virtual_host = Vhost
		}),
	{ok, Channel} = amqp_connection:open_channel(Connection),
	Access = #'access.request'{realm = Vhost,
		exclusive = false,
		passive = true,
		active = true,
		write = true,
		read = true},
	#'access.request_ok'{ticket = Ticket} = amqp_channel:call(Channel, Access),

	Q = Rses#rses.queue,
	X = Rses#rses.exchange,
	Xtype = Rses#rses.exchange_type,
	BindKey = Rses#rses.routing_key,

	QueueDeclare = #'queue.declare'{ticket = Ticket, queue = Q,
		passive = false, durable = false,
		exclusive = false, auto_delete = false,
		nowait = false, arguments = []},
	#'queue.declare_ok'{queue = Q} = amqp_channel:call(Channel, QueueDeclare),
	ExchangeDeclare = #'exchange.declare'{ticket = Ticket,
		exchange = X, type= Xtype,
		passive = false, durable = false,
		auto_delete=false, internal = false,
		nowait = false, arguments = []},
	#'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),
	QueueBind = #'queue.bind'{ticket = Ticket,
		queue = Q,
		exchange = X,
		routing_key = BindKey,
		nowait = false, arguments = []},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),
	{ok, ConsumerTag} = setup_consumer(Channel, Q),
	{ok, Rses, #conn{channel=Channel,
		connection=Connection,
		consumer_tag=ConsumerTag}
		}
.
%---------------------------------------------------------------------
send_message(Channel, X, RoutingKey, Payload) ->
	Publish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
	%io:format("send_message:~n~p~n", [Publish]),
	amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload})
.
%---------------------------------------------------------------------
setup_consumer(Channel, Q) ->
	BasicConsume = #'basic.consume'{queue = Q, no_ack = false },
	#'basic.consume_ok'{consumer_tag = ConsumerTag}
		= amqp_channel:subscribe(Channel, BasicConsume, self()),
	receive
		#'basic.consume_ok'{consumer_tag = ConsumerTag} ->
			{ok, ConsumerTag}
	after ?SETUP_CONSUMER_TIMEOUT ->
		{error, setup_consumer_timeout}
	end
.
%---------------------------------------------------------------------
cancel_consumer(Channel, ConsumerTag) ->
	% After the consumer is finished interacting with the queue,
	% it can deregister itself
	BasicCancel = #'basic.cancel'{consumer_tag = ConsumerTag,
		nowait = false},
	#'basic.cancel_ok'{consumer_tag = ConsumerTag} =
		amqp_channel:call(Channel,BasicCancel)
.
%---------------------------------------------------------------------
% Dur - receive duration in seconds.
rcv_msg(Channel, X, Sep, Dur) ->
	Start = misc:get_cur_timer(),
	rcv_msg(Channel, X, Sep, Dur, Start)
.
rcv_msg(Channel, X, Sep, Dur, Start) ->
	receive
		Msg ->
			io:format("rcv_msg Msg:~n~p~n", [Msg])
	after 0 ->
		Msg = false
	end,
	Can_work = check_dur(Dur, Start),
	case Msg of
	    {#'basic.deliver'{delivery_tag = _DeliveryTag}, Content} ->
			%amqp_channel:cast(Channel, #'basic.ack'{delivery_tag=DeliveryTag}),
			case Content of
	        	#content{payload_fragments_rev = [Payload]} ->
	    		    io:format("rcv_msg: ~p~n", [Payload]),
					rcv_msg(Channel, X, Sep, Dur, Start);
				#'amqp_msg'{payload = Payload} ->
					io:format("rcv_msg amqp_msg: ~p~n", [Payload]),
					send_reply(Channel, X, Sep, Payload),
					rcv_msg(Channel, X, Sep, Dur, Start);
				C_other ->
	    		    io:format("rcv_msg other content:~n~p~n", [C_other]),
					rcv_msg(Channel, X, Sep, Dur, Start)
			end;
		false when Can_work == true ->
			rcv_msg(Channel, X, Sep, Dur, Start);
		Other when Can_work == true ->
	        io:format("rcv_msg other:~n~p~n", [Other]),
			rcv_msg(Channel, X, Sep, Dur, Start);
		Other ->
	        io:format("rcv_msg, timeout, other:~n~p~n", [Other]),
			exit(finished_receive_messages)
	end
.
%---------------------------------------------------------------------
check_dur(Dur, Start) ->
	Now = misc:get_cur_timer(),
	if	abs(Now - Start) > Dur -> false;
		true -> true
	end
.
%---------------------------------------------------------------------
teardown(Connection, Channel, ConsumerTag) ->
	cancel_consumer(Channel, ConsumerTag),
	amqp_channel:close(Channel),
	amqp_connection:close(Connection)
.
%---------------------------------------------------------------------
send_reply(Channel, X, Rt_key, Payload) ->
	io:format("send_reply rt, payload:~n~p~n~p~n", [Rt_key, Payload]),
	send_message(Channel, X, Rt_key, Payload)
.
%---------------------------------------------------------------------
make_reply(Payload, Sep) when is_binary(Payload) ->
	Payload_str = binary_to_list(Payload),
	make_reply_list(Payload_str, Sep)
;
make_reply(Payload, Sep) when is_list(Payload) ->
	make_reply_list(Payload, Sep)
;
make_reply(Payload, Sep) ->
	{error, unknown_payload}
.
%---------------------------------------------------------------------
make_reply_list(Str, Sep) ->
	Sep_str = binary_to_list(Sep),
	io:format("make_reply_list str=~p~n", [Str]),
	case regexp:split(Str, Sep_str) of
		{ok, [Rt_str,Cmd,Timeout|_]} ->
			true;
		{ok, [Rt_str,Cmd|_]} ->
			Timeout = 15;
		_ ->
			Rt_str = "",
			Cmd = "no_cmd",
			Timeout = 15
	end,
	io:format("make_reply_list sep=~p~nrt=~p~ncmd=~p~nt=~p~n", [Sep, Rt_str, Cmd, Timeout]),
	T = misc:get_time_str(),
	New = T ++ " reply " ++ Sep_str ++ Cmd,
	New_bin = list_to_binary(New),
	Rt_bin = list_to_binary(Rt_str),
	{Rt_bin, New_bin}
.
%---------------------------------------------------------------------
send_ack(Conn, Tag) ->
	% do ack unless consumer used 'no_ack' flag for queue setup
	Channel = Conn#conn.channel,
	amqp_channel:call(Channel, #'basic.ack'{delivery_tag = Tag})
.
%-------------------------------------------------------------------
