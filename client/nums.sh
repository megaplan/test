#!/bin/sh

start(){
	erl \
	-pa ebin \
	-pa ../misc/ebin \
	-pa ../misultin/ebin \
	-pa ../mochiweb/ebin \
	-pa ../misc_rabbit/ebin \
	-name "$node" \
	-boot start_sasl \
	-noshell -detached \
	-s nums_ctl cmd \
	-- start rc_server_app

}

stop(){
	erl \
	-pa ebin \
	-pa ../misc/ebin \
	-pa ../misultin/ebin \
	-pa ../mochiweb/ebin \
	-pa ../misc_rabbit/ebin \
	-name "$node_ctl" \
	-boot start_sasl \
	-noshell -detached \
	-s nums_ctl cmd \
	-- stop $node
}

node='nums_client@localhost.localdomain'
node_ctl='ctl_nums_client@localhost.localdomain'

case "$1" in
  start)
  	start
	;;
  stop)
  	stop
	;;
  restart)
  	$0 stop
	sleep 1
	$0 start
	;;
  *)
	echo "Usage: $0 {start|stop|restart}"
	exit 1
esac

exit 0
