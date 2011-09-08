%%%-----------------------------------------------------------------
%%% functions to process http requests
%%%-----------------------------------------------------------------
-module(web_reqs).
-export([handle_http/2]).
-include("nums.hrl").
-define(T1, "fir1").
-define(T2, "fir2").
-define(T3, "fir3").
-define(T4, "fir4").
-define(T5, "fir5").
-define(T6, "fir6").
-define(T7, "fir7").
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
	Host:
	<INPUT type=\"text\" name=\""
	?T4
	"\"> <br/>
	User:
	<INPUT type=\"text\" name=\""
	?T5
	"\"> <br/>
	Password:
	<INPUT type=\"text\" name=\""
	?T6
	"\"> <br/>
	Random list of data:
	<INPUT type=\"checkbox\" name=\""
	?T7
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
	Host = proplists:get_value(?T4, Args),
	User = proplists:get_value(?T5, Args),
	Pass = proplists:get_value(?T6, Args),
	Rand = proplists:get_value(?T7, Args),
    proceed_cmd(C, Url, Method, Params, Host, User, Pass, Rand)
.
%-------------------------------------------------------------------
proceed_cmd(C, Url, Method, Params, Host, User, Pass, Rand) ->
	p_debug:p("~p:proceed_cmd:~p json~n~p~n~p~n~p~n~p~nauth: ~p, ~p~n"
        "rand: ~p~n",
		[?MODULE, ?LINE, Url, Method, Params, Host, User, Pass, Rand],
        C#nums.debug, http, 3),
    List = make_list_params(C, Params),
    Auth = fill_auth(User, Pass, Rand),
    Struct = {struct, [
        {type, rest},
        {info, {struct, [
            {method, Method},
            {url, list_to_binary(Url)},
            {host, Host},
            Auth,
            {params, {struct, List}}
            ]}}
        ]},
	p_debug:p("~p:proceed_cmd:~p struct:~n~p~n",
		[?MODULE, ?LINE, Struct], C#nums.debug, http, 4),
    V1 = mochijson2:encode(Struct),
    proceed_send(C, V1, "").
%-------------------------------------------------------------------
proceed_send(C, V1, V2) ->
	p_debug:p("~p:proceed_send:~p pars:~n~p~n~p~n",
		[?MODULE, ?LINE, V1, V2], C#nums.debug, http, 5),
	sender:send_data(C, V1, V2),
	p_debug:p("~p:proceed_send:~p done~n",
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
fill_auth(_User, _Pass, "on") ->
    List = gen_rand_items(),
    Type = {type, megaplan},
    {auth_info, [Type | List]}
;
fill_auth(User, Pass, _) ->
    {auth_info, [
        {type, basic},
        {user, User},
        {password, Pass}
    ]}
.
%-------------------------------------------------------------------
gen_rand_items() ->
	crypto:start(),
	N = crypto:rand_uniform(1, 10),
	F = fun(_, Acc) ->
		Str = integer_to_list(Acc),
		Key = "key" ++ Str,
		Val = "val" ++ Str,
		{{Key, Val}, Acc+1}
	end,
	{Res, _} = lists:mapfoldl(F, 0, lists:duplicate(N, true)),
    Res
.
%-------------------------------------------------------------------
