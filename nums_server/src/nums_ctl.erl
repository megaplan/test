%%%-----------------------------------------------------------------
%%% control module to start/stop application
%%%-----------------------------------------------------------------
-module(nums_ctl).
-export([cmd/0]).

cmd() ->
	case init:get_plain_arguments() of
		[Cmd, Node | _] ->
			ctl(Cmd, Node);
		_ ->
			ok
	end
.

ctl("start", App_str) ->
	error_logger:info_msg("ctl start, ~p~n", [App_str]),
	App = list_to_atom(App_str),
	application:start(App)
;
ctl("stop", Node_str) ->
	error_logger:info_msg("ctl stop, ~p~n", [Node_str]),
	Node = list_to_atom(Node_str),
	net_adm:ping(Node),
	rpc:call(Node, erlang, halt, []),
	timer:sleep(50),
	halt()
;
ctl(_, _) ->
	error_logger:info_msg("ctl unknown command~n", []),
	timer:sleep(50),
	halt()
.
