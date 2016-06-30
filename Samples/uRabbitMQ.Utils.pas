unit uRabbitMQ.Utils;

interface

uses uRabbitMQ;


function now_microseconds:UInt64;

procedure microsleep(usec:Integer);

procedure die(fmt: String; Args: array of const);

procedure die_on_error(x: Integer; context: AnsiString);

procedure die_on_amqp_error(x: amqp_rpc_reply_t; context: AnsiString);

// procedure dump_row(count: Cardinal; numinrow: Integer; chs: PInteger);



implementation

uses SysUtils, Windows;

function now_microseconds:UInt64;
var
  ft:FILETIME;
begin
  GetSystemTimeAsFileTime(&ft);
  Result:= ((UInt64(ft.dwHighDateTime) shl 32) or ft.dwLowDateTime)
         div 10;
end;

procedure microsleep(usec:Integer);
begin
  Sleep(usec div 1000);
end;


procedure die(fmt: String; Args: array of const);
begin
  WriteLn(Format(fmt, Args));
  Halt(1);
end;

procedure die_on_error(x: Integer; context: AnsiString);
begin
  if (x < 0) then
  begin
    WriteLn(Format('%s: %s\n', [context, amqp_error_string2(x)]));
    Halt(1);
  end;
end;

procedure die_on_amqp_error(x: amqp_rpc_reply_t; context: AnsiString);
var
  m: pamqp_connection_close_t;
begin
  case x.reply_type of
    AMQP_RESPONSE_NORMAL:
      Exit;
    AMQP_RESPONSE_NONE:
      WriteLn(Format('%s: missing RPC reply type!', [context]));
    AMQP_RESPONSE_LIBRARY_EXCEPTION:
      WriteLn(Format('%s: %s', [context, amqp_error_string2(x.library_error)]));
    AMQP_RESPONSE_SERVER_EXCEPTION:
      case (x.reply.id) of
        AMQP_CONNECTION_CLOSE_METHOD:
          begin
            m := pamqp_connection_close_t(x.reply.decoded);
            WriteLn(Format('%s: server connection error %d, message: %.*s', [context, m.reply_code, m.reply_text.len, PAnsiChar(m.reply_text.bytes)]));
          end;

        AMQP_CHANNEL_CLOSE_METHOD:
          begin
            m := pamqp_connection_close_t(x.reply.decoded);
            WriteLn(Format('%s: server channel error %d, message: %.*s', [context, m.reply_code, m.reply_text.len, PAnsiChar(m.reply_text.bytes)]));
          end;
      else
        WriteLn(Format('%s: unknown server error, method id 0x%08X', [context, x.reply.id]));
      end;
  end;
  Halt(1);
end;

end.
