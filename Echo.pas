program Echo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Serial, BaseUnix;

(* By default, don't use the functions which allow a timeout to be specified.   *)
(* The reason for this is that they use the operating system's select syscall,  *)
(* details of which vary between different unix implementations; this is most   *)
(* notable in the case of the four-parameter SerReadTimeout() function.         *)

{ define USE_TIMEOUT }

const
  baudRate= 9600;                       (* Thank you, yes, I /do/ know.         *)
  crToCrLf= false;                      (* Affects outgoing CR                  *)
  lfFudge= false;                       (* Affects incoming LF                  *)

var
  handle: TSerialHandle;
  state: TSerialState;
  buffer: array[0..1] of byte;
  count: integer;
  seenSigint: boolean= false;


procedure sigHandler(sig: longint; info: PSigInfo; context: PSigContext); cdecl;

begin
  seenSigint := true
end { sigHandler } ;


procedure catchsignals;

var     action: SigActionRec;

begin
  FillChar(action, SizeOf(action), 0);
  action.Sa_Handler := @sigHandler;
  action.Sa_Flags := SA_SIGINFO;
  if fpSigAction(SIGINT, @action, nil) <> 0 then
    WriteLn('Warning: ^C (SIGINT) not hooked, error ', fpGetErrNo);
  Flush(Output)
end { catchSignals } ;


begin
  handle := SerOpen(ParamStr(1));

(* SerOpen() does not try to get an exclusive lock on unix, to conform to the   *)
(* normal expectations engendered by Getty etc.                                 *)

  if handle >= 0 then
    try
      state := SerSaveState(handle);
      SerSetParams(handle, baudRate, 8, NoneParity, 2, []);
      SerSetDTR(handle, true);
      SerSetRTS(handle, true);
      catchSignals;
      WriteLn('Running at ', baudRate, ', ^C on keyboard terminates.');
      repeat
{$ifndef USE_TIMEOUT }
        count := SerRead(handle, buffer, 1);
{$else               }
        count := SerReadTimeout(handle, buffer, 1, 10);
{$endif USE_TIMEOUT  }
        if count > 0 then begin
          if lfFudge then
            case buffer[0] of
              $0a: ;
              $0d: begin
                     buffer[1] := $0a;
                     SerWrite(handle, buffer, 2);
                     continue
                   end
            otherwise
            end;
          if crToCrLf and (buffer[0] = $0d) then begin
            buffer[1] := $0a;
            SerWrite(handle, buffer, 2)
          end else
            SerWrite(handle, buffer, 1)
        end
{$ifndef USE_TIMEOUT }
          else
            Sleep(10)
{$endif USE_TIMEOUT  }
      until seenSigint
    finally
      WriteLn;
      SerRestoreState(handle, state);
      SerClose(handle)
    end
  else
    WriteLn('Open of "', ParamStr(1), '" failed.')
end.

