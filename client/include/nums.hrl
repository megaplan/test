-ifndef(nums_vars).
-define(nums_vars, true).

-define(TAB, numstor).
-define(T, 1000).
-define(PORT, 8182).
-define(LOG, "/proc/kcore").
-define(CONF, "nums.conf").
-define(N_DEF, 4).
-define(SRV, 'rc_server').

-record(data, {key, data = []}).

-record(cur, {beg, len}).

-record(nums, {
	conn,
	rses,
	rabbit,
	amount = 0,
	n,
	min = 0,
	log,
	cur = #cur{},
	port,
	debug,
	mis
}).

-endif.
