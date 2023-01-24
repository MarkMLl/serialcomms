
type
  byteArray= array of byte;

threadvar
  lastGetFrom: TSerialHandle;
  backtrack: bytearray;

(* This uses dynamic array insertions excessively when backtracking, but I'm    *)
(* assuming that this is comparatively infrequent and am trading efficiency for *)
(* clarity.                                                                     *)


function min(i1, i2: integer): integer; inline;

begin
  if i1 < i2 then
    result := i1
  else
    result := i2
end { min } ;


function max(i1, i2: integer): integer; inline;

begin
  if i1 > i2 then
    result := i1
  else
    result := i2
end { max } ;


(* Sleep for the given number of mSec, with granularity assumed to be of the
  order of 10 mSec.
*)
procedure sleepMSec(mSec: integer= 10);

var
  onEntry: TimerDescriptor;

begin
  InitTimer(onEntry);
  Assert(mSec >= 0, 'Unexpected -ve delay in ' + {$I %CURRENTROUTINE%} + '()');
  mSec := Round((mSec / 1000) * TimerResolution);      (* Convert to ticks      *)
  while not Elapsed(onEntry, mSec) do
{$ifdef LCL }
    if GetCurrentThreadId = MainThreadId then
      Application.ProcessMessages
    else
{$endif LCL }
      Sleep(10)
end { sleepMSec } ;


(* Require at least one byte of data to become (or already be) available before
  the first timeout expires, with the remaining bytes to be available with no
  more than the second timeout between them. This does not transfer data, if
  both timeouts are zero this is an undelayed "is this much data available?"
  test.
*)
function dataAvailable(h: TSerialHandle; c: integer= 1; t1: integer= 0; t2: integer= -1): boolean;

var
  need, got, z: integer;
  debug: boolean;

begin
  result := false;
  debug := c < 0;
  c := Abs(c);

(* The rationale for the two-stage timeout is slightly flaky: if the length of  *)
(* the packets and the length of the data being progressively consumed from     *)
(* them is such that a single byte can be left in the buffer then it's possible *)
(* to have a situation where we're waiting for a fresh packet but only the      *)
(* inter-byte timeout is applied. The really important thing is that one or     *)
(* other of the timeouts must always be applied at least once, since that       *)
(* allows the length of the timeouts to be tuned at the application level.      *)

  if t2 < 0 then
    t2 := t1;
  Assert((t1 >= 0) and (t2 >= 0), 'Bad timeout in ' + {$I %CURRENTROUTINE%} + '()');
  Assert(c >= 0, 'Bad count in ' + {$I %CURRENTROUTINE%} + '()');
  need := max(c - Length(backtrack), 0);
  if need = 0 then                     (* Skip unnecessary OS call              *)
    exit(true);
  got := SerAvailable(h);
  if t1 + t2 = 0 then                  (* Explicitly skip all later comparisons *)
    if got >= need then
      exit(true);

(* The first timeout might be inter-packet, so could be relatively long: don't  *)
(* sit in the loop any longer than necessary.                                   *)
(*                                                                              *)
(* There's a problem here: if we have no data we have an inter-packet wait t1,  *)
(* and then later we have c - 2 inter-byte waits t2, while if we don't have a   *)
(* t1 wait we need c - 1 t2s.                                                   *)

  if debug then
    WriteLn({$I %CURRENTROUTINE%} + '() A ', got, ' ', need, ' ', t1);
  z := 1;                               (* Assume we need c - 1 t2 waits        *)
  while (got < 1) and (t1 > 0) do begin
    z := 0;                             (* No, we only need c - 2 t2 waits      *)
    sleepMsec(10);
    t1 -= 10;
    got := SerAvailable(h);
    if debug then
      WriteLn({$I %CURRENTROUTINE%} + '() B ', got, ' ', need, ' ', t1)
  end;
  if debug then
    WriteLn({$I %CURRENTROUTINE%} + '() C ', got, ' ', need, ' ', t1);
  if got >= need then                   (* Data now arriving fast so return ASAP *)
    exit(true)
  else
    if got = 0 then                     (* Initial timeout unsatisfied          *)
      exit(false);

(* The second timeout is inter-byte, so is likely to be relatively short. As a  *)
(* simplistic first attempt, simply pause up to the expected amount of time and *)
(* then test whether an adequate amount of data is now available. The problem   *)
(* with this approach is that it doesn't fail early if incoming data is bursty  *)
(* and there is an unreasonable stall between bytes: it's almost good enough,   *)
(* and in practice this poor implementation works to compensate for the poor    *)
(* assumptions discussed in the earlier comment about the timeout rationale.    *)

  z := (z + c - 2) * t2;                (* Work out total wait in mSec          *)
  if debug then
    WriteLn({$I %CURRENTROUTINE%} + '() D ', got, ' ', need, ' ', z);
  while (got < need) and (z > 0) do begin (* Assumes got still valid from earlier *)
    sleepMsec(10);
    z -= 10;
    got := SerAvailable(h);
    if debug then
      WriteLn({$I %CURRENTROUTINE%} + '() E ', got, ' ', need, ' ', z)
  end;
  if debug then
    WriteLn({$I %CURRENTROUTINE%} + '() F ', got, ' ', need, ' ', t2);
  result := got >= need
end { dataAvailable } ;


(* Assuming an immediately-preceding successful call to dataAvailable(), fetch
  the specified number of bytes. If adequate data is not now available it is an
  immediate non-recoverable error: dataAvailable() and dataFetch() MUST be
  paired accurately.
*)
function dataFetch(h: TSerialHandle; c: integer): byteArray;

var
  b, s: integer;

begin
  if not dataAvailable(h, c, 0, 0) then begin
{$push}{$C+ }
    Assert(false, 'Data unexpectedly unavailable');
{$pop       }
    SerRead(InvalidSerialHandle, result{%H-}[0], 1); (* This is invariably fatal *)
    Halt(127)                           (* YOU CANNOT PASS.                     *)
  end;
  b := min(Length(backtrack), c);       (* Bytes we can get from backup         *)
  s := c - b;                           (* Bytes we need from serial stream     *)
  result := Copy(backtrack, 0, b);
{$ifndef VER3_2 }
  {$warning Attempt to directly manipulate content of dynamic array }
  Move(backtrack[b], backtrack[0], b);
  SetLength(backtrack, Length(backtrack) - b);
{$else          }
  Delete(backtrack, 0, b);
{$endif VER3_2  }
  SetLength(result, c);
  if s > 0 then                         (* Otherwise test cases will fail since *)
    SerRead(h, result[b], s)            (* h will be (intentionally) invalid.   *)
end { dataFetch } ;

