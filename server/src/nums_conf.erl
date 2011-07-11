%%%-----------------------------------------------------------------
%%% functions related to config file read, config processing
%%%-----------------------------------------------------------------
-module(nums_conf).
-export([get_config/1]).
-include("nums.hrl").
-include("rabbit_session.hrl").
%-------------------------------------------------------------------
-spec get_config(string()) -> #nums{}.

get_config(File) ->
	List = misc_conf:read_config(File),
	fill_config(List)
.
%-------------------------------------------------------------------
fill_config(List) ->
	R = nums_conf_rabbit:fill_rabbit_with(List),
	New = #nums{
		rabbit = R,
		n = proplists:get_value(n, List, ?N_DEF),
		log = proplists:get_value(log, List, ?LOG),
		port = proplists:get_value(port, List, ?PORT),
		debug = proplists:get_value(debug, List, [])
	},
	p_debug:p("~p:fill_config:~p config list:~n~p~nconfig res:~n~p~n",
		[?MODULE, ?LINE, List, New], New#nums.debug, config, 4),
	New
.
%-------------------------------------------------------------------
