{application, rc_server_app, [
	{description, "rabbit sender server"},
	{id, "nums"},
	{vsn, "1.0"},
	{modules, [
		client_conf.erl,
		nums_ctl.erl,
		rc_server_app.erl,
		rc_server.erl,
		rc_server_sup.erl,
		req_gener.erl,
		sender.erl,
		status_info.erl,
		store_num.erl,
		web_reqs.erl
	]},
	{registered, []},
	{env, []},
	{mod, {rc_server_app,[]}},
	{applications, [kernel, stdlib]}
]}.

