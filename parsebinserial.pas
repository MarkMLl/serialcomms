(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit ParseBinSerial;

(* Parse characters, bytes etc. out of a binary serial stream, and if necessary *)
(* put them back (using local storage) so that backtracking may re-read them in *)
(* the correct order even if recursive. Each "get" function takes a pair of     *)
(* mSec timeouts (time to first byte, maximum time between subsequent bytes)    *)
(* returning false if data is exhausted: this is assumed to be a permanent      *)
(* error which can't be recovered by backtracking.                              *)
(*                                                                              *)
(* DO NOT assume that backtrack state is shared with or available to any other  *)
(* unit, or that multiple serial lines can be handled simultaneously unless     *)
(* isolated in individual threads.                              MarkMLl.        *)

// TODO : Consider implementing floats.
// TODO : Am I crazy enough to support LEB128, up to at least 64 bits?
// See https://blog.timhutt.co.uk/fst_spec/

// TODO : Needs provision for CRC accumulation and backtrack.
// In order to implement backtracking this will need a buffer of either the
// "lost" (eight?) bits per incoming byte or the accumulated CRC values between
// successive initialisations. It would probably be reasonable to limit this to
// 4K in account of the SerAvailable() restriction, in which case buffering the
// CRCs as they are generated- even if these are 32 bits or larger and have to
// be done on a per-thread or per-handle basis- would present no problem on
// modern equipment.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Serial, LocatePorts;

const
  LargestSafeGet= LargestSafeRead;      (* Linux kernel restriction             *)
  LargestRecommendedGet= 2048;          (* Suggested limit for GetB() etc.      *)

type
  (* An exception of this type will be raised if a function has been asked to
    read more data than can be safely handled by the underlying OS calls etc.,
    see the LargestSafeGet constant. Assume that state was restored before the
    exception was raised and retry restricting the size to LargestRecommendedRead.
  *)
  EOverlongGet= class(Exception);

(* Fetch a single ASCII character.
*)
function GetA(h: TSerialHandle; out a: ansichar; t1: integer= 1000): boolean;

(* Fetch a fixed number of ASCII characters with neither count nor terminator.
  Assume that the kernel imposes a limit of one page size (i.e. typically
  0..4095 bytes) on this operation, if this is exceeded an EOverlongGet will be
  raised and the operation should be retried using smaller fragments.
*)
function GetF(h: TSerialHandle; out s: ansistring; c: integer; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a zero-terminated string, trimming the terminator.
*)
function GetZ(h: TSerialHandle; out s: ansistring; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a string counted by an initial 8-bit length.
*)
function GetSc8(h: TSerialHandle; out s: ansistring; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a string counted by an initial big-endian 16-bit length. Assume that
  the kernel imposes a limit of one page size (i.e. typically 0..4095 bytes)
  on this operation, if this is exceeded an EOverlongGet will be raised and the
  operation should be retried using smaller fragments.
*)
function GetSc16b(h: TSerialHandle; out s: ansistring; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a string counted by an initial little-endian 16-bit length. Assume that
  the kernel imposes a limit of one page size (i.e. typically 0..4095 bytes)
  on this operation, if this is exceeded an EOverlongGet will be raised and the
  operation should be retried using smaller fragments.
*)
function GetSc16l(h: TSerialHandle; out s: ansistring; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch an 8-bit (unsigned) cardinal.
*)
function GetC8(h: TSerialHandle; out b: byte; t1: integer= 1000): boolean;

(* Fetch an 8-bit (signed) integer.
*)
function GetI8(h: TSerialHandle; out b: shortint; t1: integer= 1000): boolean;

(* Fetch a big-endian 16-bit (unsigned) cardinal.
*)
function GetC16b(h: TSerialHandle; out w: word; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a little-endian 16-bit (unsigned) cardinal.
*)
function GetC16l(h: TSerialHandle; out w: word; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a big-endian 16-bit (signed) integer.
*)
function GetI16b(h: TSerialHandle; out s: smallint; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a little-endian 16-bit (signed) integer.
*)
function GetI16l(h: TSerialHandle; out s: smallint; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a big-endian 32-bit (unsigned) cardinal.
*)
function GetC32b(h: TSerialHandle; out w: longword; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a little-endian 32-bit (unsigned) cardinal.
*)
function GetC32l(h: TSerialHandle; out w: longword; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a big-endian 32-bit (signed) integer.
*)
function GetI32b(h: TSerialHandle; out s: longint; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a little-endian 32-bit (signed) integer.
*)
function GetI32l(h: TSerialHandle; out s: longint; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a big-endian 64-bit (unsigned) cardinal.
*)
function GetC64b(h: TSerialHandle; out w: qword; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a little-endian 64-bit (unsigned) cardinal.
*)
function GetC64l(h: TSerialHandle; out w: qword; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a big-endian 64-bit (signed) integer.
*)
function GetI64b(h: TSerialHandle; out s: int64; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a little-endian 64-bit (signed) integer.
*)
function GetI64l(h: TSerialHandle; out s: int64; t1: integer= 1000; t2: integer= 100): boolean;

(* Fetch a fixed number of bytes with neither count nor terminator. Assume that
  the kernel imposes a limit of one page size (i.e. typically 0..4095 bytes) on
  this operation, if this is exceeded an EOverlongGet will be raised and the
  operation should be retried using smaller fragments.
*)
function GetB(h: TSerialHandle; out b; c: integer; t1: integer= 1000; t2: integer= 100): boolean;

(* Backtrack a single ASCII character.
*)
procedure PutA(h: TSerialHandle; a: ansichar);

(* Backtrack a fixed number of ASCII characters with neither count nor terminator.
*)
procedure PutF(h: TSerialHandle; const s: ansistring);

(* Backtrack a zero-terminated string, appending the terminator.
*)
procedure PutZ(h: TSerialHandle; const s: ansistring);

(* Backtrack a string counted by an initial 8-bit length.
*)
procedure PutSc8(h: TSerialHandle; const s: ansistring);

(* Backtrack a string counted by an initial big-endian 16-bit length.
*)
procedure PutSc16b(h: TSerialHandle; const s: ansistring);

(* Backtrack a string counted by an initial little-endian 16-bit length.
*)
procedure PutSc16l(h: TSerialHandle; const s: ansistring);

(* Backtrack an 8-bit (unsigned) cardinal.
*)
procedure PutC8(h: TSerialHandle; b: byte);

(* Backtrack an 8-bit (signed) integer.
*)
procedure PutI8(h: TSerialHandle; b: shortint);

(* Backtrack a big-endian 16-bit (unsigned) cardinal.
*)
procedure PutC16b(h: TSerialHandle; w: word);

(* Backtrack a little-endian 16-bit (unsigned) cardinal.
*)
procedure PutC16l(h: TSerialHandle; w: word);

(* Backtrack a big-endian 16-bit (signed) integer.
*)
procedure PutI16b(h: TSerialHandle; s: smallint);

(* Backtrack a little-endian 16-bit (signed) integer.
*)
procedure PutI16l(h: TSerialHandle; s: smallint);

(* Backtrack a big-endian 32-bit (unsigned) cardinal.
*)
procedure PutC32b(h: TSerialHandle; w: longword);

(* Backtrack a little-endian 32-bit (unsigned) cardinal.
*)
procedure PutC32l(h: TSerialHandle; w: longword);

(* Backtrack a big-endian 32-bit (signed) integer.
*)
procedure PutI32b(h: TSerialHandle; s: longint);

(* Backtrack a little-endian 32-bit (signed) integer.
*)
procedure PutI32l(h: TSerialHandle; s: longint);

(* Backtrack a big-endian 64-bit (unsigned) cardinal.
*)
procedure PutC64b(h: TSerialHandle; w: qword);

(* Backtrack a little-endian 64-bit (unsigned) cardinal.
*)
procedure PutC64l(h: TSerialHandle; w: qword);

(* Backtrack a big-endian 64-bit (signed) integer.
*)
procedure PutI64b(h: TSerialHandle; s: int64);

(* Backtrack a little-endian 64-bit (signed) integer.
*)
procedure PutI64l(h: TSerialHandle; s: int64);

(* Backtrack a fixed number of bytes with neither count nor terminator.
*)
procedure PutB(h: TSerialHandle; const b; c: integer);


implementation

{$ifdef LCL }
uses
  Forms;
{$endif LCL }

{$i ParseCommonSerial.inc }


(* Fetch a single ASCII character.
*)
function GetA(h: TSerialHandle; out a: ansichar; t1: integer= 1000): boolean;

begin
  lastGetFrom := h;
  if Length(backtrack) > 0 then begin
    a := ansichar(backtrack[0]);
    Delete(backtrack, 0, 1);
    result := true
  end else
    result := SerReadTimeout(h, a, t1) > 0
{$define A }                            (* Enable test case                     *)
end { GetA } ;


(* Fetch a fixed number of ASCII characters with neither count nor terminator.
  Assume that the kernel imposes a limit of one page size (i.e. typically
  0..4095 bytes) on this operation, if this is exceeded an EOverlongGet will be
  raised and the operation should be retried using smaller fragments.
*)
function GetF(h: TSerialHandle; out s: ansistring; c: integer; t1: integer= 1000; t2: integer= 100): boolean;

var
  extraction: byteArray;
  i: integer;

begin
  if c > LargestSafeGet then
    raise EOverlongGet.Create('In ParseBinSerial.GetF(), unsafe attempt to read ' + IntToStr(c) + ' bytes.');
  lastGetFrom := h;
  if c = 0 then
    exit(true);
  if not dataAvailable(h, c, t1, t2) then
    exit(false);
  extraction := dataFetch(h, c);
  SetLength(s{%H-}, c);
  for i := 0 to c - 1 do
    s[i + 1] := ansichar(extraction[i]);
  result := true
{$define F }                            (* Enable test case                     *)
end { GetF } ;


(* Fetch a zero-terminated string, trimming the terminator.
*)
function GetZ(h: TSerialHandle; out s: ansistring; t1: integer= 1000; t2: integer= 100): boolean;

var
  a: ansichar= #$ff;

begin
  lastGetFrom := h;
  result := false;
// TODO : GetZ() not tested and I'm not confident in it
{$Warning: GetZ() not tested and I'm not confident in it }
  s := '';
  if not GetA(h, a, t1) then
    exit;
  if a <> #$00 then begin
    s := a;
    while GetA(h, a, t2) do
      if a = #$00 then
        break
      else
        s += a
  end;
  if a <> #$00 then
    PutF(h, s)
  else
    result := true
{$define Z }                            (* Enable test case                     *)
end { GetZ } ;


(* Fetch a string counted by an initial 8-bit length.
*)
function GetSc8(h: TSerialHandle; out s: ansistring; t1: integer= 1000; t2: integer= 100): boolean;

var
  l: byte;
  b: byteArray;
  i: integer;

begin
  lastGetFrom := h;
  result := false;
  if not GetC8(h, l, t1) then
    exit;
  if not dataAvailable(h, l, t2) then
    PutC8(h, l)
  else begin
    b := dataFetch(h, l);
    SetLength(s{%H-}, l);
    for i := 1 to l do
      s[i] := ansichar(b[i - 1]);
    result := true
  end
{$define SC8 }                          (* Enable test case                     *)
end { GetSc8 } ;


(* Fetch a string counted by an initial big-endian 16-bit length. Assume that
  the kernel imposes a limit of one page size (i.e. typically 0..4095 bytes)
  on this operation, if this is exceeded an EOverlongGet will be raised and the
  operation should be retried using smaller fragments.
*)
function GetSc16b(h: TSerialHandle; out s: ansistring; t1: integer= 1000; t2: integer= 100): boolean;

var
  l: word;
  b: byteArray;
  i: integer;

begin
  lastGetFrom := h;
  result := false;
  if not GetC16b(h, l, t1) then
    exit;
  if l > LargestSafeGet then begin
    PutC16b(h, l);
    raise EOverlongGet.Create('In ParseBinSerial.GetSc16b(), unsafe attempt to read ' + IntToStr(l) + ' bytes.')
  end;
  if not dataAvailable(h, l, t2) then
    PutC16b(h, l)
  else begin
    b := dataFetch(h, l);
    SetLength(s{%H-}, l);
    for i := 1 to l do
      s[i] := ansichar(b[i - 1]);
    result := true
  end
{$define SC16B }                        (* Enable test case                     *)
end { GetSc16b } ;


(* Fetch a string counted by an initial kittle-endian 16-bit length. Assume that
  the kernel imposes a limit of one page size (i.e. typically 0..4095 bytes)
  on this operation, if this is exceeded an EOverlongGet will be raised and the
  operation should be retried using smaller fragments.
*)
function GetSc16l(h: TSerialHandle; out s: ansistring; t1: integer= 1000; t2: integer= 100): boolean;

var
  l: word;
  b: byteArray;
  i: integer;

begin
  lastGetFrom := h;
  result := false;
  if not GetC16l(h, l, t1) then
    exit;
  if l > LargestSafeGet then begin
    PutC16l(h, l);
    raise EOverlongGet.Create('In ParseBinSerial.GetSc16l(), unsafe attempt to read ' + IntToStr(l) + ' bytes.')
  end;
  if not dataAvailable(h, l, t2) then
    PutC16l(h, l)
  else begin
    b := dataFetch(h, l);
    SetLength(s{%H-}, l);
    for i := 1 to l do
      s[i] := ansichar(b[i - 1]);
    result := true
  end
{$define SC16L }                        (* Enable test case                     *)
end { GetSc16l } ;


(* Fetch an 8-bit (unsigned) cardinal.
*)
function GetC8(h: TSerialHandle; out b: byte; t1: integer= 1000): boolean;

begin
  lastGetFrom := h;
  if Length(backtrack) > 0 then begin
    b := byte(backtrack[0]);
    Delete(backtrack, 0, 1);
    result := true
  end else
    result := SerReadTimeout(h, b, t1) > 0
{$define C8  }                          (* Enable test case                     *)
end { GetC8 } ;


(* Fetch an 8-bit (signed) integer.
*)
function GetI8(h: TSerialHandle; out b: shortint; t1: integer= 1000): boolean;

begin
  lastGetFrom := h;
  if Length(backtrack) > 0 then begin
    b := shortint(backtrack[0]);
    Delete(backtrack, 0, 1);
    result := true
  end else
    result := SerReadTimeout(h, b, t1) > 0
{$define I8  }                          (* Enable test case                     *)
end { GetI8 } ;


(* Fetch a big-endian 16-bit (unsigned) cardinal.
*)
function GetC16b(h: TSerialHandle; out w: word; t1: integer= 1000; t2: integer= 100): boolean;

var
  extraction: byteArray;

begin
  lastGetFrom := h;
  if not dataAvailable(h, 2, t1, t2) then
    exit(false);
  extraction := dataFetch(h, 2);
  w := (word(extraction[0]) << 8) + extraction[1];
  result := true
{$define C16B }                         (* Enable test case                     *)
end { GetC16b } ;


(* Fetch a little-endian 16-bit (unsigned) cardinal.
*)
function GetC16l(h: TSerialHandle; out w: word; t1: integer= 1000; t2: integer= 100): boolean;

var
  extraction: byteArray;

begin
  lastGetFrom := h;
  if not dataAvailable(h, 2, t1, t2) then
    exit(false);
  extraction := dataFetch(h, 2);
  w := (word(extraction[1]) << 8) + extraction[0];
  result := true
{$define C16L }                         (* Enable test case                     *)
end { GetC16l } ;


(* Fetch a big-endian 16-bit (signed) integer.
*)
function GetI16b(h: TSerialHandle; out s: smallint; t1: integer= 1000; t2: integer= 100): boolean;

var
  w: word;

begin
  lastGetFrom := h;
  result := GetC16b(h, w, t1, t2);
  if result then
{$push }{$Q- }
    s := smallint(w)
{$pop        }
{$define I16B }                         (* Enable test case                     *)
end { GetI16b } ;


(* Fetch a little-endian 16-bit (signed) integer.
*)
function GetI16l(h: TSerialHandle; out s: smallint; t1: integer= 1000; t2: integer= 100): boolean;

var
  w: word;

begin
  lastGetFrom := h;
  result := GetC16l(h, w, t1, t2);
  if result then
{$push }{$Q- }
    s := smallint(w)
{$pop        }
{$define I16L }                         (* Enable test case                     *)
end { GetI16l } ;


(* Fetch a big-endian 32-bit (unsigned) cardinal.
*)
function GetC32b(h: TSerialHandle; out w: longword; t1: integer= 1000; t2: integer= 100): boolean;

var
  extraction: byteArray;

begin
  lastGetFrom := h;
  if not dataAvailable(h, 4, t1, t2) then
    exit(false);
  extraction := dataFetch(h, 4);
  w := (longword(extraction[0]) << 24) + (longword(extraction[1]) << 16) +
                                (longword(extraction[2]) << 8) + extraction[3];
  result := true
{$define C32B }                         (* Enable test case                     *)
end { GetC32b } ;


(* Fetch a little-endian 32-bit (unsigned) cardinal.
*)
function GetC32l(h: TSerialHandle; out w: longword; t1: integer= 1000; t2: integer= 100): boolean;

var
  extraction: byteArray;

begin
  lastGetFrom := h;
  if not dataAvailable(h, 4, t1, t2) then
    exit(false);
  extraction := dataFetch(h, 4);
  w := (longword(extraction[3]) << 24) + (longword(extraction[2]) << 16) +
                                (longword(extraction[1]) << 8) + extraction[0];
  result := true
{$define C32L }                         (* Enable test case                     *)
end { GetC32l } ;


(* Fetch a big-endian 32-bit (signed) integer.
*)
function GetI32b(h: TSerialHandle; out s: longint; t1: integer= 1000; t2: integer= 100): boolean;

var
  w: longword;

begin
  lastGetFrom := h;
  result := GetC32b(h, w, t1, t2);
  if result then
{$push }{$Q- }
    s := longint(w)
{$pop        }
{$define I32B }                         (* Enable test case                     *)
end { GetI32b } ;


(* Fetch a little-endian 32-bit (signed) integer.
*)
function GetI32l(h: TSerialHandle; out s: longint; t1: integer= 1000; t2: integer= 100): boolean;

var
  w: longword;

begin
  lastGetFrom := h;
  result := GetC32l(h, w, t1, t2);
  if result then
{$push }{$Q- }
    s := longint(w)
{$pop        }
{$define I32L }                         (* Enable test case                     *)
end { GetI32l } ;


(* Fetch a big-endian 64-bit (unsigned) cardinal.
*)
function GetC64b(h: TSerialHandle; out w: qword; t1: integer= 1000; t2: integer= 100): boolean;

var
  extraction: byteArray;

begin
  lastGetFrom := h;
  if not dataAvailable(h, 8, t1, t2) then
    exit(false);
  extraction := dataFetch(h, 8);
  w := (qword(extraction[0]) << 56) + (qword(extraction[1]) << 48) +
                (qword(extraction[2]) << 40) + (qword(extraction[3]) << 32) +
                (qword(extraction[4]) << 24) + (qword(extraction[5]) << 16) +
                                (qword(extraction[6]) << 8) + extraction[7];
  result := true
{$define C64B }                         (* Enable test case                     *)
end { GetC64b } ;


(* Fetch a little-endian 64-bit (unsigned) cardinal.
*)
function GetC64l(h: TSerialHandle; out w: qword; t1: integer= 1000; t2: integer= 100): boolean;

var
  extraction: byteArray;

begin
  lastGetFrom := h;
  if not dataAvailable(h, 8, t1, t2) then
    exit(false);
  extraction := dataFetch(h, 8);
  w := (qword(extraction[7]) << 56) + (qword(extraction[6]) << 48) +
                (qword(extraction[5]) << 40) + (qword(extraction[4]) << 32) +
                (qword(extraction[3]) << 24) + (qword(extraction[2]) << 16) +
                                (qword(extraction[1]) << 8) + extraction[0];
  result := true
{$define C64L }                         (* Enable test case                     *)
end { GetC64l } ;


(* Fetch a big-endian 64-bit (signed) integer.
*)
function GetI64b(h: TSerialHandle; out s: int64; t1: integer= 1000; t2: integer= 100): boolean;

var
  w: qword;

begin
  lastGetFrom := h;
  result := GetC64b(h, w, t1, t2);
  if result then
{$push }{$Q- }
    s := int64(w)
{$pop        }
{$define I64B }                         (* Enable test case                     *)
end { GetI64b } ;


(* Fetch a little-endian 64-bit (signed) integer.
*)
function GetI64l(h: TSerialHandle; out s: int64; t1: integer= 1000; t2: integer= 100): boolean;

var
  w: qword;

begin
  lastGetFrom := h;
  result := GetC64l(h, w, t1, t2);
  if result then
{$push }{$Q- }
    s := int64(w)
{$pop        }
{$define I64L }                         (* Enable test case                     *)
end { GetI64l } ;


(* Fetch a fixed number of bytes with neither count nor terminator. Assume that
  the kernel imposes a limit of one page size (i.e. typically 0..4095 bytes) on
  this operation, if this is exceeded an EOverlongGet will be raised and the
  operation should be retried using smaller fragments.
*)
function GetB(h: TSerialHandle; out b; c: integer; t1: integer= 1000; t2: integer= 100): boolean;

var
  extraction: byteArray;

begin
  if c > LargestSafeGet then
    raise EOverlongGet.Create('In ParseBinSerial.GetB(), unsafe attempt to read ' + IntToStr(c) + ' bytes.');
  lastGetFrom := h;
  if c = 0 then
    exit(true);
  if not dataAvailable(h, c, t1, t2) then
    exit(false);
  extraction := dataFetch(h, c);
  Move(extraction[0], b{%H-}, c);       (* WARNING: EXTREMELY HAZARDOUS OPERATION *)
  result := true
{$define B }                            (* Enable test case                     *)
end { GetB } ;


(* NOTE: {$I %CURRENTROUTINE%} used below requires FPC >= 3.2.0.                *)


(* Backtrack a single ASCII character.
*)
procedure PutA(h: TSerialHandle; a: ansichar);

var
  insertion: byteArray;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, 1);
  insertion[0] := byte(a);
  Insert(insertion, backtrack, 0)
end { PutA } ;


(* Backtrack a fixed number of ASCII characters with neither count nor terminator.
*)
procedure PutF(h: TSerialHandle; const s: ansistring);

var
  insertion: byteArray;
  i: integer;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, Length(s));
  for i := 0 to Length(insertion) - 1 do
    insertion[i] := byte(s[i + 1]);
  Insert(insertion, backtrack, 0)
end { PutF } ;


(* Backtrack a zero-terminated string, appending the terminator.
*)
procedure PutZ(h: TSerialHandle; const s: ansistring);

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  PutF(h, s + #$00)
end { PutZ } ;


(* Backtrack a string counted by an initial 8-bit length.
*)
procedure PutSc8(h: TSerialHandle; const s: ansistring);

var
  insertion: byteArray;
  i: integer;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, 1 + Length(s));
  insertion[0] := byte(Length(s) and $ff);
  for i := 1 to Length(s) do
    insertion[i] := byte(s[i]);
  Insert(insertion, backtrack, 0)
end { PutSc8 } ;


(* Backtrack a string counted by an initial big-endian 16-bit length.
*)
procedure PutSc16b(h: TSerialHandle; const s: ansistring);

var
  insertion: byteArray;
  i: integer;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, 2 + Length(s));
  insertion[0] := byte((Length(s) >> 8) and $ff);
  insertion[1] := byte(Length(s) and $ff);
  for i := 1 to Length(s) do
    insertion[1 + i] := byte(s[i]);
  Insert(insertion, backtrack, 0)
end { PutSc16b } ;


(* Backtrack a string counted by an initial little-endian 16-bit length.
*)
procedure PutSc16l(h: TSerialHandle; const s: ansistring);

var
  insertion: byteArray;
  i: integer;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, 2 + Length(s));
  insertion[1] := byte((Length(s) >> 8) and $ff);
  insertion[0] := byte(Length(s) and $ff);
  for i := 1 to Length(s) do
    insertion[1 + i] := byte(s[i]);
  Insert(insertion, backtrack, 0)
end { PutSc16l } ;


(* Backtrack an 8-bit (unsigned) cardinal.
*)
procedure PutC8(h: TSerialHandle; b: byte);

var
  insertion: byteArray;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, 1);
  insertion[0] := b;
  Insert(insertion, backtrack, 0)
end { PutC8 } ;


(* Backtrack an 8-bit (signed) integer.
*)
procedure PutI8(h: TSerialHandle; b: shortint);

var
  insertion: byteArray;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, 1);
  insertion[0] := byte(b);
  Insert(insertion, backtrack, 0)
end { PutI8 } ;


(* Backtrack a big-endian 16-bit (unsigned) cardinal.
*)
procedure PutC16b(h: TSerialHandle; w: word);

var
  insertion: byteArray;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, 2);
  insertion[0] := byte(w >> 8) and $ff;
  insertion[1] := byte(w) and $ff;
  Insert(insertion, backtrack, 0)
end { PutC16b } ;


(* Backtrack a little-endian 16-bit (unsigned) cardinal.
*)
procedure PutC16l(h: TSerialHandle; w: word);

var
  insertion: byteArray;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, 2);
  insertion[1] := byte(w >> 8) and $ff;
  insertion[0] := byte(w) and $ff;
  Insert(insertion, backtrack, 0)
end { PutC16l } ;


(* Backtrack a big-endian 16-bit (signed) integer.
*)
procedure PutI16b(h: TSerialHandle; s: smallint);

var
  w: word;

begin
{$push }{$Q- }
  w := word(s);
{$pop        }
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  PutC16b(h, w)
end { PutI16b } ;


(* Backtrack a little-endian 16-bit (signed) integer.
*)
procedure PutI16l(h: TSerialHandle; s: smallint);

var
  w: word;

begin
{$push }{$Q- }
  w := word(s);
{$pop        }
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  PutC16l(h, w)
end { PutI16l } ;


(* Backtrack a big-endian 32-bit (unsigned) cardinal.
*)
procedure PutC32b(h: TSerialHandle; w: longword);

var
  insertion: byteArray;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, 4);
  insertion[0] := byte(w >> 24) and $ff;
  insertion[1] := byte(w >> 16) and $ff;
  insertion[2] := byte(w >> 8) and $ff;
  insertion[3] := byte(w) and $ff;
  Insert(insertion, backtrack, 0)
end { PutC32b } ;


(* Backtrack a little-endian 32-bit (unsigned) cardinal.
*)
procedure PutC32l(h: TSerialHandle; w: longword);

var
  insertion: byteArray;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, 4);
  insertion[3] := byte(w >> 24) and $ff;
  insertion[2] := byte(w >> 16) and $ff;
  insertion[1] := byte(w >> 8) and $ff;
  insertion[0] := byte(w) and $ff;
  Insert(insertion, backtrack, 0)
end { PutC32l } ;


(* Backtrack a big-endian 32-bit (signed) integer.
*)
procedure PutI32b(h: TSerialHandle; s: longint);

var
  w: longword;

begin
{$push }{$Q- }
  w := longword(s);
{$pop        }
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  PutC32b(h, w)
end { PutI32b } ;


(* Backtrack a little-endian 32-bit (signed) integer.
*)
procedure PutI32l(h: TSerialHandle; s: longint);

var
  w: longword;

begin
{$push }{$Q- }
  w := longword(s);
{$pop        }
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  PutC32l(h, w)
end { PutI32l } ;


(* Backtrack a big-endian 64-bit (unsigned) cardinal.
*)
procedure PutC64b(h: TSerialHandle; w: qword);

var
  insertion: byteArray;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, 8);
  insertion[0] := byte(w >> 56) and $ff;
  insertion[1] := byte(w >> 48) and $ff;
  insertion[2] := byte(w >> 40) and $ff;
  insertion[3] := byte(w >> 32) and $ff;
  insertion[4] := byte(w >> 24) and $ff;
  insertion[5] := byte(w >> 16) and $ff;
  insertion[6] := byte(w >> 8) and $ff;
  insertion[7] := byte(w) and $ff;
  Insert(insertion, backtrack, 0)
end { PutC64b } ;


(* Backtrack a little-endian 64-bit (unsigned) cardinal.
*)
procedure PutC64l(h: TSerialHandle; w: qword);

var
  insertion: byteArray;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, 8);
  insertion[7] := byte(w >> 56) and $ff;
  insertion[6] := byte(w >> 48) and $ff;
  insertion[5] := byte(w >> 40) and $ff;
  insertion[4] := byte(w >> 32) and $ff;
  insertion[3] := byte(w >> 24) and $ff;
  insertion[2] := byte(w >> 16) and $ff;
  insertion[1] := byte(w >> 8) and $ff;
  insertion[0] := byte(w) and $ff;
  Insert(insertion, backtrack, 0)
end { PutC64l } ;


(* Backtrack a big-endian 64-bit (signed) integer.
*)
procedure PutI64b(h: TSerialHandle; s: int64);

var
  w: qword;

begin
{$push }{$Q- }
  w := qword(s);
{$pop        }
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  PutC64b(h, w)
end { PutI64b } ;


(* Backtrack a little-endian 64-bit (signed) integer.
*)
procedure PutI64l(h: TSerialHandle; s: int64);

var
  w: qword;

begin
{$push }{$Q- }
  w := qword(s);
{$pop        }
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  PutC64l(h, w)
end { PutI64l } ;


(* Backtrack a fixed number of bytes with neither count nor terminator.
*)
procedure PutB(h: TSerialHandle; const b; c: integer);

var
  insertion: byteArray;

begin
  Assert(h = lastGetFrom, 'Internal error: bad put handle in ' + {$I %CURRENTROUTINE%} + '()');
  SetLength(insertion{%H-}, c);
  Move(b, insertion[0], c);             (* WARNING: EXTREMELY HAZARDOUS OPERATION *)
  Insert(insertion, backtrack, 0)
end { PutB } ;


procedure test;

const
  a= '@';
  f= 'ABC';
  z= 'DEF';
  sc8= 'GHI';
  sc16b= 'JKL';
  sc16l= 'MNO';
  c8= $55;
  i8= -$55;
  c16b= $1122;
  c16l= $1199;
  i16b= -$1122;
  i16l= -$1199;
  c32b= $11223344;
  c32l= $119922aa;
  i32b= -$11223344;
  i32l= -$119922aa;
  c64b= $1122334455667788;
  c64l= $119922aa33bbcc44;
  i64b= -$1122334455667788;
  i64l= -$119922aa33bbcc44;

var
  s: ansistring;
  c: ansichar;
  b: byte;
  bi: shortint;
  w: word;
  wi: smallint;
  l: longword;
  li: longint;
  q: qword;
  qi: int64;
  b1, b2: array[0..15] of byte;

begin
  Assert(Length(backtrack) = 0, 'Internal error: backtrack buffer not empty at start of test');
  for b := 0 to 15 do
    b1[b] := b;
{$ifdef A       }
  PutA(InvalidSerialHandle, a);
{$endif A       }
{$ifdef F       }
  PutF(InvalidSerialHandle, f);
{$endif F       }
{$ifdef Z       }
  PutZ(InvalidSerialHandle, z);
{$endif Z       }
{$ifdef SC8     }
  PutSc8(InvalidSerialHandle, sc8);
{$endif SC8     }
{$ifdef SC16B   }
  PutSc16b(InvalidSerialHandle, sc16b);
{$endif SC16B   }
{$ifdef SC16L   }
  PutSc16l(InvalidSerialHandle, sc16l);
{$endif SC16L   }
{$ifdef C8      }
  PutC8(InvalidSerialHandle, c8);
{$endif C8      }
{$ifdef I8      }
  PutI8(InvalidSerialHandle, i8);
{$endif I8      }
{$ifdef C16B    }
  PutC16b(InvalidSerialHandle, c16b);
{$endif C16B    }
{$ifdef C16L    }
  PutC16l(InvalidSerialHandle, c16l);
{$endif C16L    }
{$ifdef I16B    }
  PutI16b(InvalidSerialHandle, i16b);
{$endif I16B    }
{$ifdef I16L    }
  PutI16l(InvalidSerialHandle, i16l);
{$endif I16L    }
{$ifdef C32B    }
  PutC32b(InvalidSerialHandle, c32b);
{$endif C32B    }
{$ifdef C32L    }
  PutC32l(InvalidSerialHandle, c32l);
{$endif C32L    }
{$ifdef I32B    }
  PutI32b(InvalidSerialHandle, i32b);
{$endif I32B    }
{$ifdef I32L    }
  PutI32l(InvalidSerialHandle, i32l);
{$endif I32L    }
{$ifdef C64B    }
  PutC64b(InvalidSerialHandle, c64b);
{$endif C64B    }
{$ifdef C64L    }
  PutC64l(InvalidSerialHandle, c64l);
{$endif C64L    }
{$ifdef I64B    }
  PutI64b(InvalidSerialHandle, i64b);
{$endif I64B    }
{$ifdef I64L    }
  PutI64l(InvalidSerialHandle, i64l);
{$endif I64L    }
{$ifdef B       }
  PutB(InvalidSerialHandle, b1, 16);
{$endif B       }

(* That should have put -$119922aa33bbcc44 near the *start* of the backtrack    *)
(* buffer (i.e. even though this test looks like a stack, the code being        *)
(* tested- which would normally get before put- isn't).                         *)

{$ifdef B       }
  GetB(InvalidSerialHandle, b2, 16);
  Assert(CompareByte(b2, b1, 16) = 0, 'Internal Error: bad B put/get test');
{$endif B       }
{$ifdef I64L    }
  GetI64l(InvalidSerialHandle, qi);
  Assert(qi = i64l, 'Internal Error: bad I64l put/get test');
{$endif I64L    }
{$ifdef I64B    }
  GetI64b(InvalidSerialHandle, qi);
  Assert(qi = i64b, 'Internal Error: bad I64b put/get test');
{$endif I64B    }
{$ifdef C64L    }
  GetC64l(InvalidSerialHandle, q);
  Assert(q = c64l, 'Internal Error: bad C64l put/get test');
{$endif C64L    }
{$ifdef C64B    }
  GetC64b(InvalidSerialHandle, q);
  Assert(q = c64b, 'Internal Error: bad C64b put/get test');
{$endif C64B    }
{$ifdef I32L    }
  GetI32l(InvalidSerialHandle, li);
  Assert(li = i32l, 'Internal Error: bad I32l put/get test');
{$endif I32L    }
{$ifdef I32B    }
  GetI32b(InvalidSerialHandle, li);
  Assert(li = i32b, 'Internal Error: bad I32b put/get test');
{$endif I32B    }
{$ifdef C32L    }
  GetC32l(InvalidSerialHandle, l);
  Assert(l = c32l, 'Internal Error: bad C32l put/get test');
{$endif C32L    }
{$ifdef C32B    }
  GetC32b(InvalidSerialHandle, l);
  Assert(l = c32b, 'Internal Error: bad C32b put/get test');
{$endif C32B    }
{$ifdef I16L    }
  GetI16l(InvalidSerialHandle, wi);
  Assert(wi = i16l, 'Internal Error: bad I16l put/get test');
{$endif I16L    }
{$ifdef I16B    }
  GetI16b(InvalidSerialHandle, wi);
  Assert(wi = i16b, 'Internal Error: bad I16b put/get test');
{$endif I16B    }
{$ifdef C16L    }
  GetC16l(InvalidSerialHandle, w);
  Assert(w = c16l, 'Internal Error: bad C16l put/get test');
{$endif C16L    }
{$ifdef C16B    }
  GetC16b(InvalidSerialHandle, w);
  Assert(w = c16b, 'Internal Error: bad C16b put/get test');
{$endif C16B    }
{$ifdef I8      }
  GetI8(InvalidSerialHandle, bi);
  Assert(bi = i8, 'Internal Error: bad I8 put/get test');
{$endif I8      }
{$ifdef C8      }
  GetC8(InvalidSerialHandle, b);
  Assert(b = c8, 'Internal Error: bad C8 put/get test');
{$endif C8      }
{$ifdef SC16L   }
  GetSc16l(InvalidSerialHandle, s);
  Assert(s = sc16l, 'Internal Error: bad Sc16l put/get test');
{$endif SC16L   }
{$ifdef SC16B   }
  GetSc16b(InvalidSerialHandle, s);
  Assert(s = sc16b, 'Internal Error: bad Sc16b put/get test');
{$endif SC16B   }
{$ifdef SC8     }
  GetSc8(InvalidSerialHandle, s);
  Assert(s = sc8, 'Internal Error: bad Sc8 put/get test');
{$endif SC8     }
{$ifdef Z       }
  GetZ(InvalidSerialHandle, s);
  Assert(s = z, 'Internal Error: bad Z put/get test');
{$endif Z       }
{$ifdef F       }
  GetF(InvalidSerialHandle, s, Length(f));
  Assert(s = f, 'Internal Error: bad F put/get test');
{$endif F       }
{$ifdef A       }
  GetA(InvalidSerialHandle, c);
  Assert(c = a, 'Internal Error: bad A put/get test');
{$endif A       }
  Assert(Length(backtrack) = 0, 'Internal error: backtrack buffer not empty at end of test')
end { test } ;


begin
  lastGetFrom := InvalidSerialHandle;
  Assert(SizeOf(ansichar) = 1, 'Internal error: unexpected ansichar size');
  Assert(SizeOf(byte) = 1, 'Internal error: unexpected byte size');
  Assert(SizeOf(shortint) = 1, 'Internal error: unexpected shortint size');
  Assert(SizeOf(word) = 2, 'Internal error: unexpected word size');
  Assert(SizeOf(smallint) = 2, 'Internal error: unexpected smallint size');
  Assert(SizeOf(longword) = 4, 'Internal error: unexpected longword size');
  Assert(SizeOf(longint) = 4, 'Internal error: unexpected longint size');
  Assert(SizeOf(qword) = 8, 'Internal error: unexpected qword size');
  Assert(SizeOf(int64) = 8, 'Internal error: unexpected int64 size');
  test                                  (* Terminates with assertion on failure *)
end.

