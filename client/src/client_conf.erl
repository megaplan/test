%%%-----------------------------------------------------------------
%%% functions related to config file read, config processing
%%%-----------------------------------------------------------------
-module(client_conf).
-export([get_config/1]).
-include("nums.hrl").
%-------------------------------------------------------------------
-spec read_config(string()) -> list().

read_config(File) ->
	case file:consult(File) of
		{ok, [H|_]} ->
			H;
		{error, Reason} ->
			error_logger:info_msg("~p:read_config:~p error:~n~p~n",
				[?MODULE, ?LINE, Reason]),
			[]
	end
.
%-------------------------------------------------------------------
-spec get_config(string()) -> #nums{}.

get_config(File) ->
	List = read_config(File),
	fill_config(List)
.
%-------------------------------------------------------------------
fill_config(List) ->
	R = nums_conf_rabbit:fill_rabbit_with(List),
	#nums{
		rabbit = R,
		n = proplists:get_value(n, List, ?N_DEF),
		log = proplists:get_value(log, List, ?LOG),
		port = proplists:get_value(port, List, ?PORT),
		debug = proplists:get_value(debug, List, [])
	}
.
%-------------------------------------------------------------------
