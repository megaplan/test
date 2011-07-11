-define(p_debug_flag, true).
-ifdef(p_debug_flag).

-define(p_debug(Str, Pars, Conf, Facility, Level),
	p_debug:p_debug(Str, Pars, Conf, Facility, Level)).
-define(p_debug_ets(Str, Pars, Conf, Facility, Level, Table),
	p_debug:p_debug_ets(Str, Pars, Conf, Facility, Level, Table)).

-else.

-define(p_debug(Str, Pars, Conf, Facility, Level), true).
-define(p_debug_ets(Str, Pars, Conf, Facility, Level, Table), true).

-endif.
