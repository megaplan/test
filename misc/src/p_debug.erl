%%%-----------------------------------------------------------------
%%% debug print functions
%%%-----------------------------------------------------------------
-module(p_debug).
-export([p/5, p_ets/6]).
-define(use_p_debug, true).
%-------------------------------------------------------------------
% @doc prints string to error log, if configured loglevel higher than
% coded limit
-spec p(string(), list(), list(), atom(), integer()) -> ok.

-ifdef(use_p_debug).

p(Str, Pars, Conf, Facility, Limit) ->
	Cur_val = proplists:get_value(Facility, Conf, 0),
	if	Cur_val >= Limit ->
			Time = misc_time:get_time_str_us(),
			error_logger:info_msg(Time ++ "~n" ++ Str, Pars);
		true ->
			ok
	end
.

-else.

p(_Str, _Pars, _Conf, _Facility, _Limit) -> ok.

-endif.
%-------------------------------------------------------------------
% @doc prints full ets dump as a list to error log, if configured loglevel
% higher than coded limit
-spec p_ets(string(), list(), list(), atom(), integer(), atom()) -> ok.

-ifdef(use_p_debug).

p_ets(Str, Pars, Conf, Facility, Limit, Table) ->
	Cur_val = proplists:get_value(Facility, Conf, 0),
	if	Cur_val >= Limit ->
			error_logger:info_msg(Str, Pars),
			List = ets:tab2list(Table),
			error_logger:info_msg("~n~p~n", [List]);
		true ->
			ok
	end
.

-else.

p_ets(_Str, _Pars, _Conf, _Facility, _Limit, _Table) -> ok.

-endif.
%-------------------------------------------------------------------
