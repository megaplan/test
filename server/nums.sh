#!/bin/sh

start(){
	erl \
	-pa ebin \
	-pa ../misc/ebin \
	-pa ../amqp_client/ebin \
	-pa ../misc_rabbit/ebin \
	-name "$node" \
	-boot start_sasl \
	-noshell -detached \
	-s nums_ctl cmd \
	-- start nums_server_app

}

stop(){
	erl \
	-pa ebin -pa ../misultin/ebin \
	-name "$node_ctl" \
	-boot start_sasl \
	-noshell -detached \
	-s nums_ctl cmd \
	-- stop $node
}

node='nums_server@localhost.localdomain'
node_ctl='ctl_nums_server@localhost.localdomain'

case "$1" in
  start|s2|s3)
  	"$1"
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
