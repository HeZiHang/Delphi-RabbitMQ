program amqp_producer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uRabbitMQ,
  uRabbitMQ.Utils in '..\uRabbitMQ.Utils.pas';


const SUMMARY_EVERY_US = 1000000;

procedure send_batch(conn:amqp_connection_state_t ;queue_name:AnsiString; rate_limit, message_count:Integer);
var
  start_time:UInt64;
  i, sent, previous_sent:Integer;
  previous_report_time, next_summary_time:UInt64;
  message:array[0..255]of AnsiChar;
  message_bytes:amqp_bytes_t ;
  now:UInt64;
        countOverInterval :Integer;
         intervalRate :Double;
    stop_time :UInt64;
     total_delta :Integer;
 begin
  start_time := now_microseconds();
  sent := 0;
  previous_sent := 0;
  previous_report_time := start_time;
  next_summary_time := start_time + SUMMARY_EVERY_US;


  for i := 0 to sizeof(message)-1 do
    message[i] := AnsiChar(i and $ff);

  message_bytes.len := sizeof(message);
  message_bytes.bytes := @message;

  for i := 0 to message_count-1 do
  begin
    now :=now_microseconds();

    die_on_error(amqp_basic_publish(conn,
                                    1,
                                    amqp_cstring_bytes(PAnsiChar('amq.direct')),
                                    amqp_cstring_bytes(PAnsiChar(queue_name)),
                                    0,
                                    0,
                                    nil,
                                    message_bytes),
                 'Publishing');
    Inc(sent);
    if (now > next_summary_time) then
    begin
      countOverInterval := sent - previous_sent;
      intervalRate := countOverInterval / ((now - previous_report_time) / 1000000.0);
      WriteLn(Format('%d ms: Sent %d - %d since last report (%d Hz)',
             [(now - start_time) / 1000, sent, countOverInterval, intervalRate]));

      previous_sent := sent;
      previous_report_time := now;
      Inc(next_summary_time ,SUMMARY_EVERY_US);
    end;

    while (((i * 1000000.0) / (now - start_time)) > rate_limit) do
    begin
      microsleep(2000);
      now := now_microseconds();
    end;
  end;


    stop_time := now_microseconds();
    total_delta := (stop_time - start_time);

    WriteLn(Format('PRODUCER - Message count: %d', [message_count]));
    WriteLn(Format('Total time, milliseconds: %d', [total_delta / 1000]));
    WriteLn(Format('Overall messages-per-second: %g', [message_count / (total_delta / 1000000.0)]));
end;

procedure Main;
var
  hostname: AnsiString;
  port, status: Integer;
  rate_limit, message_count:Integer;
  socket: pamqp_socket_t;
  conn: amqp_connection_state_t;
begin

  socket := nil;
  if (ParamCount < 5) then
  begin
    Writeln('Usage: amqp_producer host port rate_limit message_count');
    Halt(1);
  end;


  hostname := ParamStr(1);
  port := StrToInt(ParamStr(2));
  rate_limit := StrToInt(ParamStr(3));
  message_count := StrToInt(ParamStr(4));

  conn := amqp_new_connection();

  socket := amqp_tcp_socket_new(conn);
  if not Assigned(socket) then
    die('creating TCP socket', []);

  status := amqp_socket_open(socket, PAnsiChar(hostname), port);
  if (status <> 0) then
    die('opening TCP socket', []);


  die_on_amqp_error(amqp_login(conn, '/', 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, 'guest', 'guest'),
                    'Logging in');
  amqp_channel_open(conn, 1);
  die_on_amqp_error(amqp_get_rpc_reply(conn), 'Opening channel');

  send_batch(conn, 'test queue', rate_limit, message_count);

  die_on_amqp_error(amqp_channel_close(conn, 1, AMQP_REPLY_SUCCESS), 'Closing channel');
  die_on_amqp_error(amqp_connection_close(conn, AMQP_REPLY_SUCCESS), 'Closing connection');
  die_on_error(amqp_destroy_connection(conn), 'Ending connection');
  Halt(0);
end;

begin
  Main;
end.


