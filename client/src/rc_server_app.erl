-module(rc_server_app).
-behaviour(application).
-export([start/0, start/2, stop/0, stop/1]).

start() ->
	Res = rc_server_sup:start_link(),
	error_logger:info_msg("app start res:~n~p~n", [Res]),
	Res
.

start(_Type, _Args) ->
	start()
.

stop(_State) ->
	ok
.

stop() ->
	ok
.
