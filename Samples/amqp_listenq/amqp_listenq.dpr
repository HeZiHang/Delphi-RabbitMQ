program amqp_listenq;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uRabbitMQ,
  uRabbitMQ.Utils in '..\uRabbitMQ.Utils.pas';

procedure main;
var
  hostname: AnsiString;
  port, status: Integer;
  queuename: AnsiString;
  socket: pamqp_socket_t;
  conn: amqp_connection_state_t;

  res: amqp_rpc_reply_t;
  envelope: amqp_envelope_t;
begin
  if (ParamCount < 5) then
  begin
    WriteLn('Usage: amqp_listenq host port queuename');
    Halt(1);
  end;

  hostname := ParamStr(1);
  port := StrToInt(ParamStr(2));
  queuename := ParamStr(3);

  conn := amqp_new_connection();

  socket := amqp_tcp_socket_new(conn);
  if not Assigned(socket) then
    die('creating TCP socket', []);

  status := amqp_socket_open(socket, PAnsiChar(hostname), port);
  if (status <> 0) then
    die('opening TCP socket', []);

  die_on_amqp_error(amqp_login(conn, '/', 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, 'guest', 'guest'), 'Logging in');
  amqp_channel_open(conn, 1);
  die_on_amqp_error(amqp_get_rpc_reply(conn), 'Opening channel');

  amqp_basic_consume(conn, 1, amqp_cstring_bytes(PAnsiChar(queuename)), amqp_empty_bytes, 0, 0, 0, amqp_empty_table);
  die_on_amqp_error(amqp_get_rpc_reply(conn), 'Consuming');

  while (True) do
  begin

    amqp_maybe_release_buffers(conn);

    res := amqp_consume_message(conn, envelope, nil, 0);

    if (AMQP_RESPONSE_NORMAL <> res.reply_type) then
      break;

    WriteLn(Format('Delivery %u, exchange %.*s routingkey %.*s', [envelope.delivery_tag, envelope.exchange.len, PAnsiChar(envelope.exchange.bytes), envelope.routing_key.len,
      PAnsiChar(envelope.routing_key.bytes)]));

    if (envelope.message.properties._flags and AMQP_BASIC_CONTENT_TYPE_FLAG <> 0) then
      WriteLn(Format('Content-type: %.*s', [envelope.message.properties.content_type.len, PAnsiChar(envelope.message.properties.content_type.bytes)]));
    WriteLn('----');

    amqp_dump(envelope.message.body.bytes, envelope.message.body.len);

    amqp_destroy_envelope(&envelope);
  end;

  die_on_amqp_error(amqp_channel_close(conn, 1, AMQP_REPLY_SUCCESS), 'Closing channel');
  die_on_amqp_error(amqp_connection_close(conn, AMQP_REPLY_SUCCESS), 'Closing connection');
  die_on_error(amqp_destroy_connection(conn), 'Ending connection');

  Halt(0);
end;

begin
  main

end.
