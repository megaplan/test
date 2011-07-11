-module(test2).
-compile(export_all).
-include("nums.hrl").

t() ->
	L = get_list(),
	test(L)
.

t(File) ->
	L = get_file(File),
	test(L)
.

test(List) ->
	inets:start(),
	F = fun(X) ->
		S = integer_to_list(X),
		req_gener:api_req(S),
		log_state()
	end,
	lists:map(F, List)
.

log_state() ->
	S = req_gener:get_state(),
	error_logger:info_msg("cur state:~n~p~n", [S])
.

get_list() ->
	[
165,
166,
167,
168,
169,
121,
122,
123,
64,
108,
109,
110,
111,
65,
10,
112,
33,
154,
34,
66,
35,
36,
40,
85,
37,
38,
39,
67,
11
	]
.

get_file(File) ->
	case file:consult(File) of
		{ok, [H|_]} ->
			H;
		{error, Reason} ->
			error_logger:info_msg("get_file error: ~p~n", [Reason]),
			[]
	end
.

