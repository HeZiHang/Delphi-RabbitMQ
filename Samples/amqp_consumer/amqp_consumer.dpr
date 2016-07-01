program amqp_consumer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uRabbitMQ,
  uRabbitMQ.Utils in '..\uRabbitMQ.Utils.pas';

const
  SUMMARY_EVERY_US = 1000000;

procedure run(conn: amqp_connection_state_t);
var
  start_time: UInt64;
  received: Integer;
  previous_received: Integer;
  previous_report_time: UInt64;
  next_summary_time: UInt64;

  frame: amqp_frame_t;

  now: UInt64;

  ret: amqp_rpc_reply_t;
  envelope: amqp_envelope_t;
  countOverInterval: Integer;
  intervalRate: Double;

  &message: amqp_message_t;
begin
  start_time := now_microseconds();
  received := 0;
  previous_received := 0;
  previous_report_time := start_time;
  next_summary_time := start_time + SUMMARY_EVERY_US;

  while (True) do
  begin
    now := now_microseconds();
    if (now > next_summary_time) then
    begin
      countOverInterval := received - previous_received;
      intervalRate := countOverInterval / ((now - previous_report_time) / 1000000.0);
      WriteLn(Format('%d ms: Received %d - %d since last report (%d Hz)\n', [Trunc((now - start_time) / 1000), received, countOverInterval, Trunc(intervalRate)]));

      previous_received := received;
      previous_report_time := now;
      next_summary_time := next_summary_time + SUMMARY_EVERY_US;
    end;

    amqp_maybe_release_buffers(conn);
    ret := amqp_consume_message(conn, @envelope, nil, 0);

    if (AMQP_RESPONSE_NORMAL <> ret.reply_type) then
    begin
      if ((AMQP_RESPONSE_LIBRARY_EXCEPTION = ret.reply_type) and (AMQP_STATUS_UNEXPECTED_STATE = amqp_status_enum(ret.library_error))) then
      begin
        if (AMQP_STATUS_OK <> amqp_status_enum(amqp_simple_wait_frame(conn, @frame))) then
          Exit;

        if (AMQP_FRAME_METHOD = frame.frame_type) then
        begin
          case (frame.payload.method.id) of
            AMQP_BASIC_ACK_METHOD:
              // if we've turned publisher confirms on, and we've published a message
              // here is a message being confirmed
              ;
            AMQP_BASIC_RETURN_METHOD:
              // if a published message couldn't be routed and the mandatory flag was set
              // this is what would be returned. The message then needs to be read.
              //
              begin
                ret := amqp_read_message(conn, frame.channel, @&message, 0);
                if (AMQP_RESPONSE_NORMAL <> ret.reply_type) then
                  Exit;

                amqp_destroy_message(@&message);
              end;

            AMQP_CHANNEL_CLOSE_METHOD:
              // a channel.close method happens when a channel exception occurs, this
              // can happen by publishing to an exchange that doesn't exist for example
              //
              // In this case you would need to open another channel redeclare any queues
              // that were declared auto-delete, and restart any consumers that were attached
              // to the previous channel
              Exit;

            AMQP_CONNECTION_CLOSE_METHOD:
              // a connection.close method happens when a connection exception occurs,
              // this can happen by trying to use a channel that isn't open for example.
              //
              // In this case the whole connection must be restarted.
              Exit;

          else
            begin
              WriteLn(Format('An unexpected method was received %u', [frame.payload.method.id]));
              Exit;
            end;
          end;
        end;
      end;

    end
    else
      amqp_destroy_envelope(@envelope);

    Inc(received);
  end;
end;

procedure main;
var
  hostname: AnsiString;
  port, status: Integer;
  exchange: AnsiString;
  bindingkey: AnsiString;
  queue: AnsiString;
  socket: pamqp_socket_t;
  conn: amqp_connection_state_t;

  queuename: amqp_bytes_t;

  r: pamqp_queue_declare_ok_t;
begin
  if (ParamCount < 3) then
  begin
    WriteLn('Usage: amqp_consumer host port');
    Halt(1);
  end;

  hostname := ParamStr(1);
  port := StrToInt(ParamStr(2));
  exchange := 'amq.direct'; // ParamStr(3);
  bindingkey := 'test queue'; // ParamStr(4);

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

  run(conn);

  die_on_amqp_error(amqp_channel_close(conn, 1, AMQP_REPLY_SUCCESS), 'Closing channel');
  die_on_amqp_error(amqp_connection_close(conn, AMQP_REPLY_SUCCESS), 'Closing connection');
  die_on_error(amqp_destroy_connection(conn), 'Ending connection');

  Halt(0);
end;

end.
