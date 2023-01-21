program Term;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Serial, Termio, BaseUnix;

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


(* Remove as many of the terminal cooked flags as are needed to get readKeyTimeout()
  (below) working properly.
*)
procedure rare(makeRare: boolean);

const
  unset= cardinal(&7777777);
  NO_ICANON= not cardinal(ICANON);
  NO_ECHO= not cardinal(ECHO);

const
  lflag: cardinal= unset;               (* Static variables                     *)

var
  tios: TermIOS;

begin
  if makeRare then begin
    if TCGetAttr(0, tios) <> 0 then
      exit;
    lflag := tios.c_lflag;
    tios.c_lflag := tios.c_lflag and NO_ICANON;
    tios.c_lflag := tios.c_lflag and NO_ECHO;
    TCSetAttr(0, TCSANOW, tios)
  end else begin
    WriteLn;
    Write('Attempting to sanify termios state: ');
    if lflag = unset then begin
      WriteLn('initial state not saved');
      exit
    end;
    if TCGetAttr(0, tios) <> 0 then begin
      WriteLn('unable to read current state');
      exit
    end;
    tios.c_lflag := lflag;
    if TCSetAttr(0, TCSANOW, tios) = 0 then
      WriteLn('OK')
    else
      WriteLn('failed')
  end
end { rare } ;


(* Read a single key, obeying the timeout which is assumed to be updated by
  the system call. Return zero if nothing is available.
*)
function readKeyTimeout(mSec: integer= 10): char;

var
  readSet: TFDSet;
  selectTimeout: TTimeVal;

begin
  result := #$00;
  fpFD_ZERO(readSet);
  fpFD_SET(0, readSet);
  selectTimeout.tv_sec := mSec div 1000;
  selectTimeout.tv_usec := (mSec mod 1000) * 1000;
  if fpSelect(1, @readSet, nil, nil, @selectTimeout) > 0 then
    fpRead(0, result, 1);

(* In at least some cases, the <Enter> key is appearing as LF rather than CR.   *)

  if result = #$0a then
    result := #$0d
end { readKeyTimeout } ;


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
      rare(true);
      WriteLn('Running at ', baudRate, ', ^C on keyboard terminates.');
      repeat
{$ifndef USE_TIMEOUT }
        count := SerRead(handle, buffer, 1);
{$else               }
        count := SerReadTimeout(handle, buffer, 1, 10);
{$endif USE_TIMEOUT  }
        while count > 0 do begin
          if not lfFudge then
            Write(Chr(buffer[0]))
          else
            case buffer[0] of
              $0a: ;
              $0d: begin
                     Write(Chr(buffer[0]));
                     Write(Chr($0a))
                   end
            otherwise
              Write(Chr(buffer[0]))
            end;
{$ifndef USE_TIMEOUT }
          count := SerRead(handle, buffer, 1)
{$else               }
          count := SerReadTimeout(handle, buffer, 1, 10)
{$endif USE_TIMEOUT  }
        end;
        buffer[0] := Ord(readKeyTimeout(10));
        if buffer[0] <> 0 then
          if crToCrLf and (buffer[0] = $0d) then begin
            buffer[1] := $0a;
            SerWrite(handle, buffer, 2)
          end else
            SerWrite(handle, buffer, 1)
      until seenSigint
    finally
      WriteLn;
      rare(false);
      SerRestoreState(handle, state);
      SerClose(handle)
    end
  else
    WriteLn('Open of "', ParamStr(1), '" failed.')
end.

