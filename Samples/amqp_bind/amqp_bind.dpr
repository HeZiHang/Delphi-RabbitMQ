program amqp_bind;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uRabbitMQ,
  uRabbitMQ.Utils in '..\uRabbitMQ.Utils.pas';

procedure Run;
var
  hostname: AnsiString;
  port, status: Integer;
  exchange: AnsiString;
  bindingkey: AnsiString;
  queue: AnsiString;
  socket: pamqp_socket_t;
  conn: amqp_connection_state_t;

  t:amqp_bytes_t;
begin
  socket := nil;
  if (ParamCount < 6) then
  begin
    Writeln('Usage: amqp_bind host port exchange bindingkey queue');
    Halt(1);
  end;

  hostname := ParamStr(1);
  port := StrToInt(ParamStr(2));
  exchange := ParamStr(3);
  bindingkey := ParamStr(4);
  queue := ParamStr(5);

  conn := amqp_new_connection();

  socket := amqp_tcp_socket_new(conn);
  if not Assigned(socket) then
    die('creating TCP socket', []);

  status := amqp_socket_open(socket, PAnsiChar(hostname), port);

  if status <> 0 then
    die('opening TCP socket', []);

  die_on_amqp_error(amqp_login(conn, '/', 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, 'guest', 'guest'), 'Logging in');
  amqp_channel_open(conn, 1);
  die_on_amqp_error(amqp_get_rpc_reply(conn), 'Opening channel');

  amqp_queue_bind(conn, 1, amqp_cstring_bytes(PAnsiChar(queue)), amqp_cstring_bytes(PAnsiChar(exchange)), amqp_cstring_bytes(PAnsiChar(bindingkey)), amqp_empty_table);
  die_on_amqp_error(amqp_get_rpc_reply(conn), 'Unbinding');

  die_on_amqp_error(amqp_channel_close(conn, 1, AMQP_REPLY_SUCCESS), 'Closing channel');
  die_on_amqp_error(amqp_connection_close(conn, AMQP_REPLY_SUCCESS), 'Closing connection');
  die_on_error(amqp_destroy_connection(conn), 'Ending connection');
  Halt(0);
end;

begin
  Run;

end.
