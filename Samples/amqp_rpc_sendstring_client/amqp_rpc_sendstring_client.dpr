program amqp_rpc_sendstring_client;

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

  reply_to_queue: amqp_bytes_t;

  r: pamqp_queue_declare_ok_t;
  props:  amqp_basic_properties_t ;

     frame: amqp_frame_t ;
     result:Integer ;

     d: pamqp_basic_deliver_t ;
     p: pamqp_basic_properties_t ;
    body_target,body_received:  size_t ;
begin
  if (ParamCount < 6) then
  begin
    WriteLn('usage:\namqp_rpc_sendstring_client host port exchange routingkey messagebody');
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

  (*
     create private reply_to queue
  *)
  begin
    r := amqp_queue_declare(conn, 1, amqp_empty_bytes, 0, 0, 0, 1, amqp_empty_table);
    die_on_amqp_error(amqp_get_rpc_reply(conn), 'Declaring queue');
    reply_to_queue := amqp_bytes_malloc_dup(r.queue);
    if (reply_to_queue.bytes = nil) then
    begin
      Write( 'Out of memory while copying queue name');
      Halt(1);
    end;
  end;
  (*
     send the message
  *)

  begin
    (*
      set properties
    *)
    props._flags := AMQP_BASIC_CONTENT_TYPE_FLAG or
                   AMQP_BASIC_DELIVERY_MODE_FLAG or
                   AMQP_BASIC_REPLY_TO_FLAG or
                   AMQP_BASIC_CORRELATION_ID_FLAG;
    props.content_type := amqp_cstring_bytes('text/plain');
    props.delivery_mode := 2; (* persistent delivery mode *)
    props.reply_to := amqp_bytes_malloc_dup(reply_to_queue);
    if (props.reply_to.bytes =nil) then
    begin
      Write( 'Out of memory while copying queue name');
      Halt(1);
    end;
    props.correlation_id := amqp_cstring_bytes('1');

    (*
      publish
    *)
    die_on_error(amqp_basic_publish(conn,
                                    1,
                                    amqp_cstring_bytes(PAnsiChar(exchange)),
                                    amqp_cstring_bytes(PAnsiChar(routingkey)),
                                    0,
                                    0,
                                    @props,
                                    amqp_cstring_bytes(PAnsiChar(messagebody))),
                 'Publishing');

    amqp_bytes_free(props.reply_to);
  end;

  (*
    wait an answer
  *)

  begin
    amqp_basic_consume(conn, 1, reply_to_queue, amqp_empty_bytes, 0, 1, 0, amqp_empty_table);
    die_on_amqp_error(amqp_get_rpc_reply(conn), 'Consuming');
    amqp_bytes_free(reply_to_queue);

    begin

      while(True) do
      begin
        amqp_maybe_release_buffers(conn);
        result := amqp_simple_wait_frame(conn, &frame);
        WriteLn(Format('Result: %d',[ result]));
        if (result < 0) then Break;

        WriteLn(Format('Frame type: %u channel: %u', [frame.frame_type, frame.channel]));
        if (frame.frame_type <> AMQP_FRAME_METHOD) then Continue;

        WriteLn(Format('Method: %s', [amqp_method_name(frame.payload.method.id)]));
        if (frame.payload.method.id <> AMQP_BASIC_DELIVER_METHOD) then
          continue;

        d := frame.payload.method.decoded;
        WriteLn(Format('Delivery: %u exchange: %.*s routingkey: %.*s',
               [ d.delivery_tag,
                 d.exchange.len, PAnsiChar(d.exchange.bytes),
                 d.routing_key.len, PAnsiChar( d.routing_key.bytes)]));

        result := amqp_simple_wait_frame(conn, frame);
        if (result < 0) then Break;

        if (frame.frame_type <> AMQP_FRAME_HEADER) then
        begin
          Write('Expected header!');
          Halt;
        end;
        p := frame.payload.properties.decoded;
        if (p._flags and AMQP_BASIC_CONTENT_TYPE_FLAG<>0) then
          WriteLn(Format('Content-type: %.*s',
                 [p.content_type.len, PAnsiChar( p.content_type.bytes)]));

        WriteLn('----');

        body_target := frame.payload.properties.body_size;
        body_received := 0;

        while (body_received < body_target) do
        begin
          result := amqp_simple_wait_frame(conn, &frame);
          if (result < 0) then
            break;

          if (frame.frame_type <> AMQP_FRAME_BODY) then
          begin
            Write( 'Expected body!');
            Halt;
          end;

          Inc(body_received , frame.payload.body_fragment.len);
          assert(body_received <= body_target);

          amqp_dump(frame.payload.body_fragment.bytes,
                    frame.payload.body_fragment.len);
        end;

        if (body_received <> body_target) then
          (* Can only happen when amqp_simple_wait_frame returns <= 0 *)
          (* We break here to close the connection *)
          break;


        (* everything was fine, we can quit now because we received the reply *)
        break;
      end;
    end;
  end;

  (*
     closing
  *)

  die_on_amqp_error(amqp_channel_close(conn, 1, AMQP_REPLY_SUCCESS), 'Closing channel');
  die_on_amqp_error(amqp_connection_close(conn, AMQP_REPLY_SUCCESS), 'Closing connection');
  die_on_error(amqp_destroy_connection(conn), 'Ending connection');

  Halt(0);
end;

begin
  Main;
end.

