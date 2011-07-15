%%% 
%%% nums_server: Nums server's main module
%%% 
%%% Copyright (c) 2011 Megaplan Ltd. (Russia)
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom
%%% the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% @author arkdro <arkdro@gmail.com>
%%% @sinse 2011-07-15 10:00
%%% @doc Receive messages from RabbitMQ queue and make a REST call to the URL from the message
%%% 

-module(nums_server).
-behaviour(gen_server).

%%%-----------------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------------

% gen_server callbacks
-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([terminate/2, code_change/3]).

%%%-----------------------------------------------------------------------------
%%% Includes
%%%-----------------------------------------------------------------------------

-include("nums.hrl").
-include("rabbit_session.hrl").
-include("amqp_client.hrl").

-define(HTTP_TIMEOUT, 15000).


%%%-----------------------------------------------------------------------------
%%% gen_server callbacks
%%%-----------------------------------------------------------------------------

start() ->
	start_link().
%%------------------------------------------------------------------------------

start_link() ->
	start_link(?CONF).
start_link(Config) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).
%%------------------------------------------------------------------------------

stop() ->
	gen_server:call(?MODULE, stop).
%%------------------------------------------------------------------------------

init([Config]) ->
	application:start(inets),
	C = nums_conf:get_config(Config),
	New = prepare_all(C),
	p_debug:p("~p:init:~p done~n", [?MODULE, ?LINE], New#nums.debug, run, 1),
	{ok, New, ?T}.
%%------------------------------------------------------------------------------

-spec handle_call(atom() | tuple(), pid(), #nums{}) -> tuple().
%%
%% Handling call messages
%% @since 2011-07-15 11:00
%%
handle_call(stop, _From, St) ->
	{stop, normal, ok, St};
handle_call(status, _From, St) ->
	{reply, St, St, ?T};
handle_call(get, _From, St) ->
	p_debug:p("~p::~p get~n", [?MODULE, ?LINE], St#nums.debug, run, 3),
	Res = St,
	{reply, Res, St, ?T};
handle_call({put, N}, _From, St) when is_integer(N), N > 0 ->
	p_debug:p("~p::~p put ~p~n", [?MODULE, ?LINE, N], St#nums.debug, run, 3),
	New = store_num:store_num(St, N),
	{reply, ok, New, ?T};
handle_call(_N, _From, St) ->
	p_debug:p("~p::~p other:~n~p~n",
		[?MODULE, ?LINE, _N], St#nums.debug, run, 4),
	{reply, {error, unknown_request}, St, ?T}.
%%------------------------------------------------------------------------------

-spec handle_cast(atom(), #nums{}) -> tuple().
%%
%% Handling cast messages
%% @since 2011-07-15 11:00
%%
handle_cast(stop, St) ->
	{stop, normal, St};
handle_cast(rotate, St) ->
	prepare_all(St),
	{noreply, St, ?T};
handle_cast(st0p, St) ->
	St;
handle_cast(_, St) ->
	{noreply, St, ?T}.
%%------------------------------------------------------------------------------

terminate(_, #nums{conn=Conn} = _State) ->
	myrb:teardown(Conn#conn.connection,
		Conn#conn.channel,
		Conn#conn.consumer_tag),
	ok.
%%------------------------------------------------------------------------------

%%
%% Handling all non call/cast messages
%%
handle_info(timeout, State) ->
	{noreply, State, ?T};
handle_info({#'basic.deliver'{delivery_tag = _Tag}, Content} = _Req, State) ->
	p_debug:p("~p::~p basic.deliver:~n~p~n",
		[?MODULE, ?LINE, _Req], State#nums.debug, run, 5),
	Payload = Content#amqp_msg.payload,
	myrb:send_ack(State#nums.conn, _Tag),
	New = store_rabbit_cmd(State, Payload),
	{noreply, New, ?T};
handle_info(_Req, State) ->
	p_debug:p("~p::~p other:~n~p~n",
		[?MODULE, ?LINE, _Req], State#nums.debug, run, 3),
	{noreply, State, ?T}.
%%------------------------------------------------------------------------------

code_change(_Old_vsn, State, _Extra) ->
	{ok, State}.
%%------------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

% @doc does all necessary preparations: [re]opens log file.
-spec prepare_all(#nums{}) -> any().

prepare_all(C) ->
	misc_log:prepare_log(C#nums.log),
	prepare_q(C).
%%------------------------------------------------------------------------------

prepare_q(C) ->
	{ok, Rses, Conn} = myrb:start(C#nums.rabbit),
	C#nums{rses=Rses, conn=Conn}.
%%------------------------------------------------------------------------------

%%
%% Make http REST-call
%% @since 2011-07-15
%%
store_rabbit_cmd(State, Payload) ->
	p_debug:p("~p:store_rabbit_cmd:~p payload:~n~p~n",
		[?MODULE, ?LINE, Payload], State#nums.debug, run, 4),
	Url = binary_to_list(Payload),
	Res = http:request(head, {Url, []},
		[{timeout, ?HTTP_TIMEOUT}, {connect_timeout, ?HTTP_TIMEOUT}],
		[]),
	p_debug:p("~p:store_rabbit_cmd:~p http result:~n~p~n",
		[?MODULE, ?LINE, Res], State#nums.debug, run, 3),
	State.
%%------------------------------------------------------------------------------
