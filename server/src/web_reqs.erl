%%%-----------------------------------------------------------------
%%% functions to process http requests
%%%-----------------------------------------------------------------
-module(web_reqs).
-export([handle_http/2]).
-include("nums.hrl").
%-------------------------------------------------------------------
% @doc processes incoming http request
handle_http(C, Req) ->
	handle(C, Req:get(method), Req:resource([lowercase, urldecode]), Req)
.
%-------------------------------------------------------------------
% @doc URI handler
% GET /stat
handle(C, 'GET', ["stat"], Req) ->
	Stat = status_info:get_status_long(),
	p_debug:p("~p:handle:~p get stat long~n~p~n", [?MODULE, ?LINE, Stat], C#nums.debug, get, 3),
	Req:ok([{"Content-Type", "text/plain"}], Stat)
;
% GET /stat/short
handle(C, 'GET', ["stat", "short"], Req) ->
	Stat = status_info:get_status_short(),
	p_debug:p("~p:handle:~p get stat short~n~p~n", [?MODULE, ?LINE, Stat], C#nums.debug, get, 3),
	Req:ok([{"Content-Type", "text/plain"}], Stat)
;
% GET /stat/raw
handle(_, 'GET', ["stat", "raw"], Req) ->
	Stat = status_info:get_status(),
	Req:ok([{"Content-Type", "text/plain"}], Stat)
;
% PUT /stat
handle(C, 'PUT', ["stat"], Req) ->
	p_debug:p("~p:handle:~p put req:~n~p~n", [?MODULE, ?LINE, Req], C#nums.debug, http, 5),
	store_number(C, Req),
	Req:ok([{"Content-Type", "text/plain"}], "put done~n")
;
% GET /rotate
handle(_, 'GET', ["rotate"], Req) ->
	rotate(),
	Req:ok([{"Content-Type", "text/plain"}], "ok")
;
% 404
handle(_, _, _, Req) ->
	Req:ok([{"Content-Type", "text/plain"}], "Page not found.")
.
%-------------------------------------------------------------------
% @doc sends input number to nums_server
-spec store_number(#nums{}, any()) -> ok.

store_number(C, Req) ->
	Number = get_req_number(Req),
	p_debug:p("~p:handle:~p put number: ~p~n", [?MODULE, ?LINE, Number], C#nums.debug, put, 3),
	gen_server:call(?SRV, {put, Number})
.
%-------------------------------------------------------------------
% @doc extracts number from arrived request
get_req_number(Req) ->
	%U = Req:get(uri),
	H = Req:get(headers),
	Len_str = proplists:get_value('Content-Length', H, "0"),
	Len = get_integer(Len_str),
	B = Req:get(body),
	case catch split_binary(B, Len) of
		{'EXIT', _} ->
			Num = -1;
		{B1, _} ->
			List = binary_to_list(B1),
			Num = get_integer(List)
	end,
	Num
.
%-------------------------------------------------------------------
get_integer(List) ->
	catch list_to_integer(List)
.
%-------------------------------------------------------------------
% @doc sends request to nums_server to rotate logs
rotate() ->
	gen_server:cast(?SRV, rotate)
.
%-------------------------------------------------------------------
