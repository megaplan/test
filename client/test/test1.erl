-module(test1).
-compile(export_all).
-include("nums.hrl").

t1() ->
	req_gener:t(1, 1, 0, 1)
.

t2() ->
	req_gener:t(10, 1000, 1000, 1)
.

t2s() ->
	req_gener:t_sync(5, 500, 500, 1)
.

t3() ->
	req_gener:t(1, 5, 0, 1)
.

t4() ->
	req_gener:t(10, 5, 0, 1)
.

t5a() ->
	application:start(rc_server_app),
	t2s(),
	S = req_gener:get_state(),
	error_logger:info_msg("s:~n~p~n", [S]),
	application:stop(rc_server_app)
.

t5() ->
	t5(10)
.

t5(N) ->
	% N - number of application's iterations
	lists:foreach(fun(_) -> t5a() end, lists:duplicate(N, true))
.
