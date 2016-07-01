program amqp_connect_timeout;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  uRabbitMQ,
  uRabbitMQ.Utils in '..\uRabbitMQ.Utils.pas';

procedure Run;
var
  hostname:AnsiString;
   port:Integer;
  socket:pamqp_socket_t;
   conn:amqp_connection_state_t;
  tval:timeval;
  tv:ptimeval;
begin
  if (ParamCount < 3) then
  begin
    Writeln('Usage: amqp_connect_timeout host port [timeout_sec [timeout_usec=0]]');
    Halt(1);
  end;

  if (ParamCount > 3) then
  begin
    tv := @tval;

    tv.tv_sec := StrToInt(ParamStr(3));

    if (ParamCount > 4 ) then
      tv.tv_usec := StrToInt(ParamStr(4))
     else
      tv.tv_usec := 0;


  end else
    tv := nil;



  hostname := ParamStr(1);
  port := StrToInt(ParamStr(2));

  conn := amqp_new_connection();

  socket := amqp_tcp_socket_new(conn);

  if not Assigned(socket) then
    die('creating TCP socket',[]);


  die_on_error(amqp_socket_open_noblock(socket, PAnsiChar(hostname), port, tv), 'opening TCP socket');

  die_on_amqp_error(amqp_login(conn, '/', 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, 'guest', 'guest'), 'Logging in');

  die_on_amqp_error(amqp_connection_close(conn, AMQP_REPLY_SUCCESS), 'Closing connection');
  die_on_error(amqp_destroy_connection(conn), 'Ending connection');

  WriteLn ('Done');
  halt(0);
end;

begin
  Run;
end.
