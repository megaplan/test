%%%-----------------------------------------------------------------
%%% functions to process http requests
%%%-----------------------------------------------------------------
-module(web_reqs).
-export([handle_http/2]).
-include("nums.hrl").
-define(T1, "fir1").
-define(T2, "fir2").
-define(T3, "fir3").
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
handle(_C, 'GET', ["page3"], Req) ->
	Txt = make_page3(),
	Req:ok([{"Content-Type", "text/html"}], Txt)
;
handle(C, 'POST', ["page4"], Req) ->
	send_page4(C, Req)
;
% 404
handle(_, _, _, Req) ->
	Req:ok([{"Content-Type", "text/plain"}], "Page not found.")
.
%-------------------------------------------------------------------
send_page4(C, Req) ->
	p_debug:p("~p:send_page4:~p req:~n~p~n",
		[?MODULE, ?LINE, Req], C#nums.debug, http, 4),
	Args = Req:parse_post(),
	p_debug:p("~p:send_page4:~p args:~n~p~n",
		[?MODULE, ?LINE, Args], C#nums.debug, http, 3),

	BuildXml = fun({Param, Value}, Acc) ->
		[lists:flatten(io_lib:format("<param><name>~s</name><value>~s</value></param>", [Param, Value]))|Acc]
	end,
	Xml = lists:flatten(lists:reverse(lists:foldl(BuildXml, [], Args))),
	p_debug:p("~p:send_page4:~p xml:~n~p~n",
		[?MODULE, ?LINE, Xml], C#nums.debug, http, 4),
	Req:ok([{"Content-Type", "text/xml"}],
        "<misultin_test>~s</misultin_test>",
        [Xml])
.
%-------------------------------------------------------------------
make_page3() ->
    T = misc_time:get_time_str_us(),
    "<html>
<body>
cur time: " ++
T ++
"
</body>
</html>
"
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
	Params:
	<INPUT type=\"text\" name=\""
	?T3
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
	Url = proplists:get_value(?T1, Args, ""),
	Method = proplists:get_value(?T2, Args, ""),
	Params = proplists:get_value(?T3, Args, ""),
    proceed_cmd(C, Url, Method, Params)
.
%-------------------------------------------------------------------
proceed_cmd(C, Url, Method, Params) ->
	p_debug:p("~p:proceed_cmd:~p json~n~p~n~p~n~p~n",
		[?MODULE, ?LINE, Url, Method, Params], C#nums.debug, http, 3),
    List = make_list_params(C, Params),
    Struct = {struct, [
        {type, rest},
        {rest_info, {struct, [
            {method, Method},
            {url, list_to_binary(Url)},
            {params, {struct, List}}
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
make_list_params(C, Params) ->
    Res = mochiweb_util:parse_qs(Params),
	p_debug:p("~p:make_list_params:~p res:~n~p~n",
		[?MODULE, ?LINE, Res], C#nums.debug, http, 4),
    Res
.
%-------------------------------------------------------------------
