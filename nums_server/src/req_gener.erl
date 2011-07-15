%%%-----------------------------------------------------------------
%%% api call generator functions
%%%-----------------------------------------------------------------
-module(req_gener).
%-export([t/0]).
-compile(export_all).
-define(URL, "http://localhost:8182/stat").
-define(API_TIMEOUT, 5000).
-define(GENS, 1).
-define(AMOUNT, 10).
-define(MAX_DELAY, 1000).
-define(MAX_LEN, 4096).
%------------------------------------------------------------
% @doc spawns some generators and waits for them to finish
t_sync() ->
	t_sync(?GENS, ?AMOUNT, ?MAX_DELAY, ?MAX_LEN)
.
%------------------------------------------------------------
t_sync(Gens, N, Max_delay, Max_len) ->
	List = t(Gens, N, Max_delay, Max_len),
	Refs = lists:map(fun(X) -> {erlang:monitor(process, X), X} end, List),
	t_sync_wait(Refs)
.
%------------------------------------------------------------
t_sync_wait([]) ->
	ok
;
t_sync_wait(List) ->
	receive
		{'DOWN', MonitorRef, _Type, _Object, _Info} ->
			New = proplists:delete(MonitorRef, List),
			t_sync_wait(New);
		_ ->
			t_sync_wait(List)
	after 100 ->
		t_sync_wait(List)
	end	
.
%------------------------------------------------------------
-spec t() -> [any()].
t() ->
	t(?GENS, ?AMOUNT, ?MAX_DELAY, ?MAX_LEN)
.
%------------------------------------------------------------
% @doc spawns Gens generators with individual
% parameters (Amount, Delay, Max_len) and leaves them in background
-spec t(pos_integer(), pos_integer(), non_neg_integer(), pos_integer()) ->
	[pid()].

t(Gens, N, Max_delay, Max_len) ->
	crypto:start(),
	inets:start(),
	F = fun(_) ->
		Delay = crypto:rand_uniform(1, Max_delay),
		spawn(?MODULE, req_loop, [N, Delay, Max_len])
	end,
	lists:map(F, lists:duplicate(Gens, true))
.
%------------------------------------------------------------
% @doc sends one http put request
-spec api_req(string()) -> {ok, any()} | {error, any()}.

api_req(Req) ->
	Pid = self(),
	error_logger:info_msg("pid=~p, now=~p, req=~p~n", [Pid, now(), Req]),
	httpc:request(put, {?URL, [], "",  Req}, [{timeout, ?API_TIMEOUT}, {connect_timeout, ?API_TIMEOUT}], [])
.
%------------------------------------------------------------
% @doc sends one http get request
get_state() ->
	httpc:request(get, {?URL++"/short", []}, [{timeout, ?API_TIMEOUT}, {connect_timeout, ?API_TIMEOUT}], [])
.
%------------------------------------------------------------
% @doc waits Delay, creates new value, sends it via http
-spec one_request(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().

one_request(Delay, Max_len, Old) ->
	timer:sleep(Delay),
	New = make_new_value(Max_len, Old),
	Str = integer_to_list(New),
	api_req(Str),
	New
.
%------------------------------------------------------------
% @doc The generator. Performs N requests.
-spec req_loop(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().

req_loop(N, Delay, Max_len) ->
	Pid = self(),
	error_logger:info_msg("pid=~p, delay=~p, max len=~p~n", [Pid, Delay, Max_len]),
	F = fun(_, Ain) ->
		one_request(Delay, Max_len, Ain)
	end,
	Acc = make_new_random(Max_len),
	lists:foldl(F, Acc, lists:duplicate(N, true))
.
%------------------------------------------------------------
% @doc does either of:
% 90% - increases old value with 1,
% 10% - creates new random value
-spec make_new_value(non_neg_integer(), non_neg_integer()) -> non_neg_integer().

make_new_value(Max_len, Old) ->
	N = crypto:rand_uniform(0, 100),
	if	N < 10 ->
			make_new_random(Max_len);
		true ->
			Old + 1
	end
.
%------------------------------------------------------------
% @doc creates one random value size of 1..Max_len+1
-spec make_new_random(non_neg_integer()) -> non_neg_integer().

make_new_random(Max_len) ->
	Len = crypto:rand_uniform(1, Max_len + 1),
	Bin = crypto:rand_bytes(Len),
	<<Int:Len/unit:8>> = Bin, % from asn1rt_ber_bin_v2.erl
	Int
.
%------------------------------------------------------------
