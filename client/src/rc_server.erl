%%%-----------------------------------------------------------------
%%% nums server
%%%-----------------------------------------------------------------
-module(rc_server).
-behaviour(gen_server).
-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).
-include("nums.hrl").
%-------------------------------------------------------------------
start() ->
	start_link()
.
%-------------------------------------------------------------------
start_link() ->
	start_link(?CONF)
.
start_link(Config) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], [])
.
%-------------------------------------------------------------------
stop() ->
	gen_server:call(?MODULE, stop)
.
%-------------------------------------------------------------------
init([Config]) ->
	application:start(inets),
	C = client_conf:get_config(Config),
	Rconf = prepare_all(C),
	p_debug:p("~p:init:~p prepared~n", [?MODULE, ?LINE], C#nums.debug, run, 1),
	{ok, Pid} = misultin:start_link(
		[{port, C#nums.port},
		{loop, fun(Req) -> web_reqs:handle_http(Rconf, Req) end}]),
	State = Rconf#nums{mis = Pid},
	%unlink(Pid), % only for manual start with console
	p_debug:p("~p:init:~p done~n", [?MODULE, ?LINE], C#nums.debug, run, 1),
	init_storage(),
	{ok, State, ?T}
.
%-------------------------------------------------------------------
handle_call(stop, _From, St) ->
	{stop, normal, ok, St}
;
handle_call(status, _From, St) ->
	{reply, St, St, ?T}
;
handle_call(get, _From, St) ->
	p_debug:p("~p::~p get~n", [?MODULE, ?LINE], St#nums.debug, run, 3),
	Res = get_numbers(St),
	{reply, Res, St, ?T}
;
handle_call({put, N}, _From, St) when is_integer(N), N > 0 ->
	p_debug:p("~p::~p put ~p~n", [?MODULE, ?LINE, N], St#nums.debug, run, 3),
	New = store_num:store_num(St, N),
	{reply, ok, New, ?T}
;
handle_call(_N, _From, St) ->
	p_debug:p("~p::~p other:~n~p~n", [?MODULE, ?LINE, _N], St#nums.debug, run, 4),
	{reply, {error, unknown_request}, St, ?T}
.
%-------------------------------------------------------------------
handle_cast(stop, St) ->
	{stop, normal, St}
;
handle_cast(rotate, St) ->
	prepare_all(St),
	{noreply, St, ?T}
;
handle_cast(st0p, St) ->
	St
;
handle_cast(_, St) ->
	{noreply, St, ?T}
.
%-------------------------------------------------------------------
terminate(_, _State) ->
	misultin:stop(),
	ets:delete(?TAB),
	ok
.
%-------------------------------------------------------------------
handle_info(timeout, State) ->
	{noreply, State, ?T}
;
handle_info(_, State) ->
	{noreply, State, ?T}
.
%-------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
	{ok, State}
.
%-------------------------------------------------------------------
% @doc does all necessary preparations: [re]opens log file.
prepare_all(C) ->
	misc_log:prepare_log(C#nums.log),
	prepare_rabbit(C)
.
%-------------------------------------------------------------------
init_storage() ->
	ets:new(?TAB, [ordered_set, named_table, {keypos, 2}, protected])
.
%-------------------------------------------------------------------
% @doc extracts all sequences from storage and returns N sequences
% sorted by length
-spec get_numbers(#nums{}) -> [#cur{}].

get_numbers(St) ->
	Cur = St#nums.cur,
	List = ets:tab2list(?TAB),
	Sorted = get_sorted_list([Cur | List]),
	Delta = St#nums.amount - St#nums.n,
	if	Delta > 0 ->
			lists:nthtail(Delta, Sorted);
		true ->
			Sorted
	end
.
%-------------------------------------------------------------------
% @doc gets list of sequences, returns list of sequences sorted by length
-spec get_sorted_list(list()) -> [#cur{}].

get_sorted_list(List) ->
	List2 = lists:map(fun get_data_item/1, List),
	List3 = lists:flatten(List2),
	Sorted = lists:sort(fun compare_items/2, List3),
	Sorted
.
%-------------------------------------------------------------------
% @doc gets input item, returns either list of sequences or just sequence
-spec get_data_item(#data{} | #cur{}) -> list() | #cur{}.

get_data_item(#data{data = Data}) ->
	Data
;
get_data_item(#cur{} = Item) ->
	Item
.
%-------------------------------------------------------------------
% @doc compare two sequences using sequence length
-spec compare_items(#cur{}, #cur{}) -> boolean().

compare_items(#cur{len=A}, #cur{len=B}) when A =< B ->
	true
;
compare_items(_, _) ->
	false
.
%-------------------------------------------------------------------
prepare_rabbit(C) ->
	{ok, Rses, Conn} = sender:start(C#nums.rabbit),
	C#nums{rses=Rses, conn=Conn}
.
%-------------------------------------------------------------------
