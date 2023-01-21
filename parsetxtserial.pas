(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit ParseTxtSerial;

(* Parse lines out of a serial stream, and if necessary put them back (using    *)
(* local storage) so that backtracking may re-read them in the correct order    *)
(* even if recursive. Each "get" function takes a pair of mSec timeouts (time   *)
(* to first character, maximum time between subsequent characters) returning    *)
(* false if data is exhausted: this is assumed to be a permanent error which    *)
(* can't be recovered by backtracking.                                          *)
(*                                                                              *)
(* DO NOT assume that backtrack state is shared with or available to any other  *)
(* unit, or that multiple serial lines can be handled simultaneously unless     *)
(* isolated in individual threads.                                              *)
(*                                                              MarkMLl.        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Serial;

(* Fetch a CR-terminated line, trimming the terminator, a leading LF is noted
  globally but otherwise ignored. Partial lines are buffered between calls, the
  result is false and the string parameter blank if an intact line cannot be
  assembled from what has already been seen plus newly-arriving serial data.
*)
function GetLn(h: TSerialHandle; out s: ansistring; t1: integer= 1000; t2: integer= 100): boolean;

(* Backtrack a terminated string, taking the global state noting any leading LF
  into account and appending an implied CR. Assume that this not safe for
  recursion/nesting.
*)
procedure PutLn(h: TSerialHandle; const s: ansistring);


implementation

uses
  LocatePorts
{$ifdef LCL }
  , Forms
{$endif LCL }
  ;

{$i ParseCommonSerial.inc }

threadvar
(* This stores state between a call to GetLn() and an adjacent call to PutLn(),
  it is always cleared by PutLn() which moves all state into the backtrack
  buffer.
*)
  leadingLF: boolean;

(* Fetch a CR-terminated line, trimming the terminator, a leading LF is noted
  globally but otherwise ignored. Partial lines are buffered between calls, the
  result is false and the string parameter blank if an intact line cannot be
  assembled from what has already been seen plus newly-arriving serial data.
*)
function GetLn(h: TSerialHandle; out s: ansistring; t1: integer= 1000; t2: integer= 100): boolean;

var
  i: integer;
  buffer: ansistring;
  scratch: ansichar;
  mSecOnEntry: qword;

begin
  result := false;
  s := '';

(* On entry, recover any state saved by either an unsuccessful call to GetLn()  *)
(* or by a PutLn().                                                             *)

  SetLength(buffer{%H- 5091 }, Length(backtrack));
  for i := 0 to Length(backtrack) - 1 do
    buffer[i + 1] := ansichar(backtrack[i]);
  SetLength(backtrack, 0);
  leadingLF := false;
  try

(* Check whether the recovered state was actually an intact line.               *)

    if (buffer <> '') and (buffer[Length(buffer)] = #$0d) then begin
      Delete(buffer, Length(buffer), 1);
      result := true;
      exit                              (* via finally block                    *)
    end;

// TODO : Should be prepared to handle CR and LF emplaced by an initial partial-line or binary backtrack.

//    Assert((Pos(#$0d, buffer) <= 0) and (Pos(#$0a, buffer) <= 0), 'Unhandled CR/LF in backtrack');

(* If there is no recovered state then anticipate a printable character before  *)
(* t1 elapses, optionally preceded by an LF within the same period. Note that   *)
(* this variant of SerReadTimeout() reads at most one byte, so shouldn't miss   *)
(* CRs etc.                                                                     *)

    if buffer = '' then begin
      mSecOnEntry := GetTickCount64();
      if SerReadTimeout(h, scratch{%H- 5057 }, t1) > 0 then begin
        buffer := scratch;              (* Might be a leading LF                *)
        if scratch = #$0a then
          if SerReadTimeout(h, scratch, t1 - (GetTickCount64() - mSecOnEntry)) = 0 then
            exit                        (* Via finally block                    *)
          else
            buffer += scratch
      end
    end;

(* We have either recovered state or seen the start of a line, in any event     *)
(* we've now got something buffered. Continue reading using t2 as the timeout,  *)
(* this again handles at most one byte at a time.                               *)

    if buffer <> '' then
      while SerReadTimeout(h, scratch, t2) > 0 do begin
        result := scratch = #$0d;
        if result then
          break
        else
          buffer += scratch
      end
  finally
    if not result then begin
      SetLength(backtrack, Length(buffer));
      for i := 1 to Length(buffer) do
        backtrack[i - 1] := byte(buffer[i])
    end else begin
      leadingLF := (buffer <> '') and (buffer[1] = #$0a);
      if leadingLF then
        s := Copy(buffer, 2, Length(buffer) - 1)
      else
        s := buffer
    end
  end;

(* On exit, we have six possible cases:                                         *)
(*                                                                              *)
(* * No LF was seen, nothing else was seen, returning false.                    *)
(*                                                                              *)
(* * LF was seen but nothing else, returning false.                             *)
(*                                                                              *)
(* * No LF was seen, an unterminated line was seen, returning false.            *)
(*                                                                              *)
(* * LF was seen, an unterminated line was seen, returning false.               *)
(*                                                                              *)
(* * No LF was seen, after zero or more characters CR was seen, returning true. *)
(*                                                                              *)
(* * LF was seen, after zero or more characters CR was seen, returning true.    *)
(*                                                                              *)
(* In the first four cases, the optional leading LF and any partial line are    *)
(* written to the backtrack buffer. It is assumed that this will be followed by *)
(* another GetLn(), not by PutLn().                                             *)
(*                                                                              *)
(* In the final two cases the string parameter might or might not be blank. It  *)
(* is assumed that this will be followed either by another GetLn() (overwriting *)
(* saved state), or by a PutLn().                                               *)

end { GetLn } ;


(* Backtrack a terminated string, taking any leading LF and an implied trailing
  CR into account. Assume that this not safe for recursion/nesting.
*)
procedure PutLn(h: TSerialHandle; const s: ansistring);

var
  i: integer;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');

(* On entry, we assume one of two possible cases from an immediately-preceding  *)
(* GetLn():                                                                     *)
(*                                                                              *)
(* LF was logged, the string might or might not be blank.                       *)
(*                                                                              *)
(* LF was not logged, the string might or might not be blank.                   *)
(*                                                                              *)
(* In both cases, CR is assumed to have been seen as a terminator.              *)

  if leadingLF then begin
    SetLength(backtrack, Length(s) + 2);
    backtrack[0] := $0a;
    for i := 1 to Length(s) do
      backtrack[i] := byte(s[i]);
    backtrack[Length(s) + 1] := $0d
  end else begin
    SetLength(backtrack, Length(s) + 1);
    for i := 1 to Length(s) do
      backtrack[i - 1] := byte(s[i]);
    backtrack[Length(s)] := $0d
  end;
  leadingLF := false
end { PutLn } ;


begin
  lastGetFrom := InvalidSerialHandle;
  leadingLF := false
end.

