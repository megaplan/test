{application, nums_server_app, [
	{description, "messages server"},
	{id, "mserv"},
	{vsn, "1.0"},
	{modules, [
		nums_conf.erl,
		nums_ctl.erl,
		nums_server_app.erl,
		nums_server.erl,
		nums_server_sup.erl,
		req_gener.erl,
		status_info.erl,
		store_num.erl,
		web_reqs.erl
	]},
	{registered, []},
	{env, []},
	{mod, {nums_server_app,[]}},
	{applications, [kernel, stdlib]}
]}.

