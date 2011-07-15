INCLUDE_DIRS = [
  '../deps/amqp_client/include',
  '../../RabbitMQ/rabbitmq_server-2.5.1/include',
  '../misc_rabbit/include',
  '../client/include',
  '../misc/include',
  '../server/include'
]

load File.dirname(__FILE__) + '/erlang.rake'