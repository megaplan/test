-ifndef(rabbit_session).
-define(rabbit_session, true).

-record(rses, {
	'host' = "127.0.0.1",
	'port' = 5672,
	'user' = <<"guest">>,
	'password' = <<"guest">>,
	'vhost' = <<"/">>,
	'exchange' = <<"my_e_2">>,
	'exchange_type' = <<"topic">>,
	'queue' = <<"my_q_2">>,
	'routing_key' = <<"snd2_topic">>,
	'timeout' = 15,
	'step_timeout' = 0.1,
	'sep' = <<"=~">>
}).

-record(conn, {
	'channel' = false,
	'connection' = false,
	'consumer_tag' = false
}).

-endif.
