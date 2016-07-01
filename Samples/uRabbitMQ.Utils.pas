unit uRabbitMQ.Utils;

interface

uses SysUtils, Windows, System.Character, uRabbitMQ;

procedure amqp_dump(const buffer:Pointer; len:size_t);

function now_microseconds:UInt64;

procedure microsleep(usec:Integer);

procedure die(fmt: String; Args: array of const);

procedure die_on_error(x: Integer; context: AnsiString);

procedure die_on_amqp_error(x: amqp_rpc_reply_t; context: AnsiString);


implementation


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

function IsPrint(C:Char):Boolean;
begin
  Result:= C.IsLetterOrDigit or C.IsSeparator or C.IsSymbol or C.IsNumber or C.IsPunctuation or C.IsWhiteSpace;
end;

procedure dump_row(count:UInt32; numinrow:Integer; chs:PIntegerArray);
var
  i:Integer;
begin
  Write(Format('%08lX:', [count - numinrow]));

  if (numinrow > 0) then
  begin
    for i := 0 to numinrow-1 do
    begin
      if (i = 8) then
        Write(' :');

      Write(Format(' %02X', [Char(chs[i])]));
    end;
    for i:= numinrow to 16-1 do
    begin
      if (i = 8) then
        Write(' :');
      Write('   ');
    end;
    WriteLn('  ');
    for i := 0 to numinrow-1 do
    begin
      if (IsPrint(Char(chs[i]))) then
        Write(Char(chs[i]))
      else
        Write('.');
    end;
  end;
  WriteLn;
end;

function rows_eq(a, b:PIntegerArray):Boolean;
var
  i:Integer;
begin
  for i:=0 to 16-1 do
    if (a[i] <> b[i]) then Exit(False);
  Result:=True;
end;

procedure amqp_dump(const buffer:Pointer; len:size_t);
var
  buf:PAnsiChar;
  count:UInt32;
  numinrow :Integer;
  chs:array[0..16-1] of Integer;
  oldchs:array[0..16-1] of Integer;
  showed_dots:Boolean;
  i:size_t;
  ch:AnsiChar;
  j:Integer;
begin
  buf := buffer;
  count := 0;
  numinrow := 0;
  showed_dots := False;
  for i := 0 to len-1 do
  begin
    ch := buf[i];

    if (numinrow = 16) then
    begin
      if (rows_eq(@oldchs, @chs)) then
      begin
        if (not showed_dots) then
        begin
          showed_dots := True;
          WriteLn('          .. .. .. .. .. .. .. .. : .. .. .. .. .. .. .. ..');
        end
      end else
      begin
        showed_dots := False;
        dump_row(count, numinrow, @chs);
      end;

      for j:=0 to 16-1 do
        oldchs[j] := chs[j];

      numinrow := 0;
    end;

    Inc(count);
    chs[numinrow] := Ord(ch);
    Inc(numinrow);
  end;

  dump_row(count, numinrow, @chs);

  if (numinrow <> 0) then
    WriteLn(Format('%08lX:', [count]));
end;


end.
