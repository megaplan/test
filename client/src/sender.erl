%%% RabbitMQ interaction
-module(sender).

-include_lib("amqp_client.hrl").
-include("rabbit_session.hrl").
-include("nums.hrl").

-export([start/1]).
-export([teardown/3,send_reply/4]).
-export([send_data/3]).

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
	{ok, Rses, #conn{channel=Channel, connection=Connection}}
.
%---------------------------------------------------------------------
send_message(Channel, X, RoutingKey, Payload) ->
	Publish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
	amqp_channel:call(Channel, Publish, #amqp_msg{payload = Payload})
.
%---------------------------------------------------------------------
cancel_consumer(Channel, ConsumerTag) ->
	% After the consumer is finished interacting with the queue,
	% it can deregister itself
	BasicCancel = #'basic.cancel'{consumer_tag = ConsumerTag, nowait = false},
	#'basic.cancel_ok'{consumer_tag = ConsumerTag} =
		amqp_channel:call(Channel,BasicCancel)
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
send_data(C, V1, V2) ->
	Conn = C#nums.conn,
	Channel = Conn#conn.channel,
	R = C#nums.rabbit,
	X = R#rses.exchange,
	RoutingKey = R#rses.routing_key,
	Bin1 = list_to_binary(V1),
	Bin2 = list_to_binary(V2),
	Payload = <<Bin1/binary, Bin2/binary>>,
	send_message(Channel, X, RoutingKey, Payload)
.
%---------------------------------------------------------------------
