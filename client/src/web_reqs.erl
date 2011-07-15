%%%-----------------------------------------------------------------
%%% functions to process http requests
%%%-----------------------------------------------------------------
-module(web_reqs).
-export([handle_http/2]).
-include("nums.hrl").
-define(T1, "fir1").
-define(T2, "fir2").
%-------------------------------------------------------------------
% @doc processes incoming http request
handle_http(C, Req) ->
	handle(C, Req:get(method), Req:resource([lowercase, urldecode]), Req)
.
%-------------------------------------------------------------------
% @doc URI handler
handle(_C, 'GET', ["page1"], Req) ->
	Txt = make_page1(),
	Req:ok([{"Content-Type", "text/html"}], Txt)
;
handle(C, 'POST', ["page2"], Req) ->
	Res = send_page2(C, Req),
	Req:ok([{"Content-Type", "text/plain"}], Res)
;
% 404
handle(_, _, _, Req) ->
	Req:ok([{"Content-Type", "text/plain"}], "Page not found.")
.
%-------------------------------------------------------------------
make_page1() ->
	"<html>
	<body>
	<form method=\"post\" action=\"/page2\">
	Message:
	<input size=\"80\" type=\"text\" name=\""
	?T1
	%"\"/> <br/>
	%Add:
	%<INPUT type=\"text\" name=\""
	%?T2
	"\"> <br/>
	<INPUT type=\"submit\" value=\"Send\"> <INPUT type=\"reset\"> <br/>
	</form>
	</body>
	</html>
	"
.
%-------------------------------------------------------------------
send_page2(C, Req) ->
	p_debug:p("~p:send_page2:~p req:~n~p~n",
		[?MODULE, ?LINE, Req], C#nums.debug, http, 4),
	Args = Req:parse_post(),
	p_debug:p("~p:send_page2:~p args:~n~p~n",
		[?MODULE, ?LINE, Args], C#nums.debug, http, 3),
	V1 = proplists:get_value(?T1, Args, ""),
	V2 = proplists:get_value(?T2, Args, ""),
    proceed_cmd(C, V1, V2)
.
%-------------------------------------------------------------------
proceed_cmd(C, [$j, $s, $o, $n, $=, $~ | Data], V2) ->
	p_debug:p("~p:proceed_cmd:~p json~n~p~n~p~n",
		[?MODULE, ?LINE, Data, V2], C#nums.debug, http, 3),
    Struct = {struct, [
        {type, rest},
        {job_info, {struct, [
            {method, head},
            {url, list_to_binary(Data)}
            ]}}
        ]},
    V1 = mochijson2:encode(Struct),
    proceed_send(C, V1, V2);
proceed_cmd(C, V1, V2) ->
	p_debug:p("~p:proceed_cmd:~p other~n~p~n~p~n",
		[?MODULE, ?LINE, V1, V2], C#nums.debug, http, 3),
    proceed_send(C, V1, V2)
.
%-------------------------------------------------------------------
proceed_send(C, V1, V2) ->
	sender:send_data(C, V1, V2),
	p_debug:p("~p:send_page2:~p done~n",
		[?MODULE, ?LINE], C#nums.debug, http, 5),
	"ok\n".
%-------------------------------------------------------------------
