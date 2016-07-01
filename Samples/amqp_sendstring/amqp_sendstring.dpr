program amqp_sendstring;

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
  routingkey: AnsiString;
  messagebody: AnsiString;
  socket: pamqp_socket_t;
  conn: amqp_connection_state_t;

  props:  amqp_basic_properties_t ;
begin
  if (ParamCount < 6) then
  begin
    WriteLn('Usage: amqp_sendstring host port exchange routingkey messagebody');
    Halt(1);
  end;

  hostname := ParamStr(1);
  port := StrToInt(ParamStr(2));
  exchange := ParamStr(3);
  routingkey := ParamStr(4);
  messagebody := ParamStr(5);

  conn := amqp_new_connection();

  socket := amqp_tcp_socket_new(conn);
  if not Assigned(socket) then
    die('creating TCP socket', []);

  status := amqp_socket_open(socket, PAnsiChar(hostname), port);

  if status <> 0 then
    die('opening TCP socket', []);

  die_on_amqp_error(amqp_login(conn, '/', 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, 'guest', 'guest'),
                    'Logging in');
  amqp_channel_open(conn, 1);
  die_on_amqp_error(amqp_get_rpc_reply(conn), 'Opening channel');

  begin
    props._flags := AMQP_BASIC_CONTENT_TYPE_FLAG or AMQP_BASIC_DELIVERY_MODE_FLAG;
    props.content_type := amqp_cstring_bytes('text/plain');
    props.delivery_mode := 2; (* persistent delivery mode *)
    die_on_error(amqp_basic_publish(conn,
                                    1,
                                    amqp_cstring_bytes(PAnsiChar(exchange)),
                                    amqp_cstring_bytes(PAnsiChar(routingkey)),
                                    0,
                                    0,
                                    @props,
                                    amqp_cstring_bytes(PAnsiChar(messagebody))),
                 'Publishing');
  end;

  die_on_amqp_error(amqp_channel_close(conn, 1, AMQP_REPLY_SUCCESS), 'Closing channel');
  die_on_amqp_error(amqp_connection_close(conn, AMQP_REPLY_SUCCESS), 'Closing connection');
  die_on_error(amqp_destroy_connection(conn), 'Ending connection');
  Halt(0);
end;

begin
  Main;
end.
