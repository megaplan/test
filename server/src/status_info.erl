%%%-----------------------------------------------------------------
%%% functions to work with nums_server's output
%%%-----------------------------------------------------------------
-module(status_info).
-export([get_status/0, get_status_long/0, get_status_short/0]).
-include("nums.hrl").
%-include("p_debug.hrl").
%-------------------------------------------------------------------
get_status() ->
	St = gen_server:call(?SRV, status),
	Dat = gen_server:call(?SRV, get),
	io_lib:format("~p~n~p~n", [St, Dat])
.
%-------------------------------------------------------------------
% @doc returns short info on current sequences
get_status_short() ->
	Sorted = gen_server:call(?SRV, get),
	make_short_text(Sorted)
.
%-------------------------------------------------------------------
% @doc converts list of sequences into short text
-spec make_short_text(list()) -> string().

make_short_text(List) ->
	F = fun(#cur{beg=Beg, len=Len}) ->
		io_lib:format("~b - ~b~n", [Len, Beg])
	end,
	L_text = lists:map(F, List),
	lists:flatten(L_text)
.
%-------------------------------------------------------------------
% @doc returns long info on current sequences
get_status_long() ->
	Sorted = gen_server:call(?SRV, get),
	make_long_text(Sorted)
.
%-------------------------------------------------------------------
% @doc gets list of sequences, creates text
% containing all these numbers
-spec make_long_text([#cur{}]) -> string().

make_long_text(List) ->
	F = fun(#cur{beg=Beg, len=Len}) ->
		Nums = get_full_seq(Beg, Len),
		make_text_seq(Nums)
	end,
	L_text = lists:map(F, List),
	L_text
	%lists:flatten(L_text)
.
%-------------------------------------------------------------------
% @doc gets Begin and Length, makes list of numbers for sequence
-spec get_full_seq(non_neg_integer(), non_neg_integer()) -> [non_neg_integer()].

get_full_seq(Beg, Len) ->
	get_full_seq(Beg, [], Len)
.
%-------------------------------------------------------------------
get_full_seq(_, List, 0) ->
	lists:reverse(List)
;
get_full_seq(N, List, Len) ->
	get_full_seq(N + 1, [N | List], Len-1)	
.
%-------------------------------------------------------------------
% @doc gets list of numbers, makes string with these numbers
-spec make_text_seq([non_neg_integer()]) -> string().

make_text_seq(List) ->
	F = fun(N) ->
		integer_to_list(N)
	end,
	L_text = lists:map(F, List),
	S_text = string:join(L_text, ", "),
	lists:flatten(S_text ++ "\n")
.
%-------------------------------------------------------------------
