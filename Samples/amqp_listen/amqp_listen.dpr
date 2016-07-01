program amqp_listen;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uRabbitMQ,
  uRabbitMQ.Utils in '..\uRabbitMQ.Utils.pas';

procedure main;
var
  hostname: AnsiString;
  port, status: Integer;
  exchange: AnsiString;
  bindingkey: AnsiString;
  socket: pamqp_socket_t;
  conn: amqp_connection_state_t;
  queuename: amqp_bytes_t;
  r: pamqp_queue_declare_ok_t;

  res: amqp_rpc_reply_t;
  envelope: amqp_envelope_t;
begin
  if (ParamCount < 5) then
  begin
    WriteLn('Usage: amqp_listen host port exchange bindingkey');
    Halt(1);
  end;

  hostname := ParamStr(1);
  port := StrToInt(ParamStr(2));
  exchange := ParamStr(3);
  bindingkey := ParamStr(4);

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

  r := amqp_queue_declare(conn, 1, amqp_empty_bytes, 0, 0, 0, 1, amqp_empty_table);
  die_on_amqp_error(amqp_get_rpc_reply(conn), 'Declaring queue');
  queuename := amqp_bytes_malloc_dup(r.queue);
  if (queuename.bytes = nil) then
  begin
    WriteLn('Out of memory while copying queue name');
    Halt(1);
  end;

  amqp_queue_bind(conn, 1, queuename, amqp_cstring_bytes(PAnsiChar(exchange)), amqp_cstring_bytes(PAnsiChar(bindingkey)), amqp_empty_table);
  die_on_amqp_error(amqp_get_rpc_reply(conn), 'Binding queue');

  amqp_basic_consume(conn, 1, queuename, amqp_empty_bytes, 0, 1, 0, amqp_empty_table);
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
    begin
      WriteLn(Format('Content-type: %.*s', [envelope.message.properties.content_type.len, PAnsiChar(envelope.message.properties.content_type.bytes)]));
    end;
    WriteLn('----');

    amqp_dump(envelope.message.body.bytes, envelope.message.body.len);

    amqp_destroy_envelope(envelope);
  end;

  die_on_amqp_error(amqp_channel_close(conn, 1, AMQP_REPLY_SUCCESS), 'Closing channel');
  die_on_amqp_error(amqp_connection_close(conn, AMQP_REPLY_SUCCESS), 'Closing connection');
  die_on_error(amqp_destroy_connection(conn), 'Ending connection');

  Halt(0);
end;

begin
  main;

end.
