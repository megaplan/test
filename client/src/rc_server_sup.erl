-module(rc_server_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, nums_supervisor}, rc_server_sup, [])
.

init(_Args) ->
	{ok, {{one_for_one, 2, 3},
		[
			{
				nums, {rc_server, start_link, []},
				permanent, brutal_kill, worker, [rc_server]
			}
		]}}
.
