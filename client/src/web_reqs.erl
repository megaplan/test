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
	Method:
	<INPUT type=\"text\" name=\""
	?T2
	"\"> <br/>
	URL:
	<input size=\"80\" type=\"text\" name=\""
	?T1
	"\"/> <br/>
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
	Url = proplists:get_value(?T1, Args, ""),
	Method = proplists:get_value(?T2, Args, ""),
    proceed_cmd(C, Url, Method)
.
%-------------------------------------------------------------------
proceed_cmd(C, Url, Method) ->
	p_debug:p("~p:proceed_cmd:~p json~n~p~n~p~n",
		[?MODULE, ?LINE, Url, Method], C#nums.debug, http, 3),
    Struct = {struct, [
        {type, rest},
        {job_info, {struct, [
            {method, Method},
            {url, list_to_binary(Url)}
            ]}}
        ]},
    V1 = mochijson2:encode(Struct),
    proceed_send(C, V1, "").
%-------------------------------------------------------------------
proceed_send(C, V1, V2) ->
	sender:send_data(C, V1, V2),
	p_debug:p("~p:send_page2:~p done~n",
		[?MODULE, ?LINE], C#nums.debug, http, 5),
	"ok\n".
%-------------------------------------------------------------------
