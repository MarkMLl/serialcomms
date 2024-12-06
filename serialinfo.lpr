(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

program serialinfo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Serial, BaseUnix, Termio, Errors;

{$if not declared(SerialStruct) }
(* Translated from include/linux/serial.h, see also linux/tty_flags.h. This is  *)
(* ancient and is related to the original setserial program, it's got Ty Ts'o's *)
(* name on it which lends it some credibility; however I don't know how widely  *)
(* this is implemented, i.e. whether it is deprecated in later kernels and      *)
(* whether some devices do not support it at the driver level.                  *)
(*                                                                              *)
(* See https://forum.lazarus.freepascal.org/index.php/topic,35302.msg403957.html *)
(* but note the necessity for canary fields since the kernel might write more   *)
(* into the structure than is documented.                                       *)

{ packrecords C} // No apparent effect
type
  SerialStruct = packed record
         typ             : Cardinal;
         line            : Cardinal;
         port            : Cardinal;
         irq             : Cardinal;
         flags           : Cardinal;
         xmit_fifo_size  : Cardinal;
         custom_divisor  : Cardinal;
         baud_base       : Cardinal;
         close_delay     : Word;
         io_type         : Byte;
         reserved_char   : Byte;
         hub6            : Cardinal;
         closing_wait    : Word; // time to wait before closing
         closing_wait2   : Word; // no longer used...
         iomem_base      : PtrUInt;
         iomem_reg_shift : Word;
         port_high       : Cardinal;
         iomap_base      : Cardinal; // cookie passed into ioremap
         canary: array[0..31] of cardinal
  end;


(*
 * For the close wait times, 0 means wait forever for serial port to
 * flush its output.  65535 means don't wait at all.
 *)

const
  ASYNC_CLOSING_WAIT_INF= 0;
  ASYNC_CLOSING_WAIT_NONE= 65535;
  birdseed: cardinal= $55555555;

(*
 * These are the supported serial types.
 *)

type
  portType= (_Unknown= 0, _8250= 1, _16450= 2, _16550= 3, _16550A= 4, _Cirrus= 5,
                _16650= 6, _16650v2= 7, _16750= 8, _Startech= 9, _16C950= 10,
                _16654= 11, _16850=12, _RSA= 13);

  ioType= (Port= 0, Hub6= 1, Mem= 2, Mem32= 3, AU= 4, TSI= 5, Mem32BE= 6, Mem16= 7);

const
  PORT_MAX= 13;
  IO_MAX= 7;

  UART_CLEAR_FIFO=          $01;
  UART_USE_FIFO=            $02;
  UART_STARTECH=            $04;
  UART_NATSEMI=             $08;

  (* Flag bits:                                                                 *)

  ASYNCB_HUP_NOTIFY=        0; (* Notify getty on hangups and closes
                                    * on the callout port *)
  ASYNCB_FOURPORT=          1; (* Set OUT1, OUT2 per AST Fourport settings *)
  ASYNCB_SAK=               2; (* Secure Attention Key (Orange book) *)
  ASYNCB_SPLIT_TERMIOS=     3; (* [x] Separate termios for dialin/callout *)
  ASYNCB_SPD_HI=            4; (* Use 57600 instead of 38400 bps *)
  ASYNCB_SPD_VHI=           5; (* Use 115200 instead of 38400 bps *)
  ASYNCB_SKIP_TEST=         6; (* Skip UART test during autoconfiguration *)
  ASYNCB_AUTO_IRQ=          7; (* Do automatic IRQ during
                                    * autoconfiguration *)
  ASYNCB_SESSION_LOCKOUT=   8; (* [x] Lock out cua opens based on session *)
  ASYNCB_PGRP_LOCKOUT=      9; (* [x] Lock out cua opens based on pgrp *)
  ASYNCB_CALLOUT_NOHUP=    10; (* [x] Don't do hangups for cua device *)
  ASYNCB_HARDPPS_CD=       11; (* Call hardpps when CD goes high  *)
  ASYNCB_SPD_SHI=          12; (* Use 230400 instead of 38400 bps *)
  ASYNCB_LOW_LATENCY=      13; (* Request low latency behaviour *)
  ASYNCB_BUGGY_UART=       14; (* This is a buggy UART, skip some safety
                                    * checks.  Note: can be dangerous! *)
  ASYNCB_AUTOPROBE=        15; (* [x] Port was autoprobed by PCI/PNP code *)
  ASYNCB_MAGIC_MULTIPLIER= 16; (* Use special CLK or divisor *)
  ASYNCB_LAST_USER=        16;

  ASYNC_HUP_NOTIFY=        (1 << ASYNCB_HUP_NOTIFY);
  ASYNC_FOURPORT=          (1 << ASYNCB_FOURPORT);
  ASYNC_SAK=               (1 << ASYNCB_SAK);
  ASYNC_SPLIT_TERMIOS=     (1 << ASYNCB_SPLIT_TERMIOS);
  ASYNC_SPD_HI=            (1 << ASYNCB_SPD_HI);
  ASYNC_SPD_VHI=           (1 << ASYNCB_SPD_VHI);
  ASYNC_SKIP_TEST=         (1 << ASYNCB_SKIP_TEST);
  ASYNC_AUTO_IRQ=          (1 << ASYNCB_AUTO_IRQ);
  ASYNC_SESSION_LOCKOUT=   (1 << ASYNCB_SESSION_LOCKOUT);
  ASYNC_PGRP_LOCKOUT=      (1 << ASYNCB_PGRP_LOCKOUT);
  ASYNC_CALLOUT_NOHUP=     (1 << ASYNCB_CALLOUT_NOHUP);
  ASYNC_HARDPPS_CD=        (1 << ASYNCB_HARDPPS_CD);
  ASYNC_SPD_SHI=           (1 << ASYNCB_SPD_SHI);
  ASYNC_LOW_LATENCY=       (1 << ASYNCB_LOW_LATENCY);
  ASYNC_BUGGY_UART=        (1 << ASYNCB_BUGGY_UART);
  ASYNC_AUTOPROBE=         (1 << ASYNCB_AUTOPROBE);
  ASYNC_MAGIC_MULTIPLIER=  (1 << ASYNCB_MAGIC_MULTIPLIER);

  ASYNC_FLAGS=             ((1 << (ASYNCB_LAST_USER + 1)) - 1);
  ASYNC_DEPRECATED=        (ASYNC_SPLIT_TERMIOS + ASYNC_SESSION_LOCKOUT +
                ASYNC_PGRP_LOCKOUT + ASYNC_CALLOUT_NOHUP + ASYNC_AUTOPROBE);
  ASYNC_SPD_CUST=          (ASYNC_SPD_HI + ASYNC_SPD_VHI);
  ASYNC_SPD_WARP=          (ASYNC_SPD_HI + ASYNC_SPD_SHI);
  ASYNC_SPD_MASK=          (ASYNC_SPD_HI + ASYNC_SPD_VHI + ASYNC_SPD_SHI);
  ASYNC_USR_MASK=          (ASYNC_SPD_MASK + ASYNC_CALLOUT_NOHUP + ASYNC_LOW_LATENCY);

  ASYNC_DEPRECATED_N= ASYNC_FLAGS - ASYNC_DEPRECATED;
  ASYNC_SPD_CUST_N= ASYNC_FLAGS -ASYNC_SPD_CUST;
  ASYNC_SPD_WARP_N= ASYNC_FLAGS - ASYNC_SPD_WARP;
  ASYNC_SPD_MASK_N= ASYNC_FLAGS - ASYNC_SPD_MASK;
  ASYNC_USR_MASK_N= ASYNC_FLAGS - ASYNC_USR_MASK;
{$endif                         }

var
  i: integer;


function portName(typ: integer): string;

begin
  if typ <= PORT_MAX then begin
    Str(portType(typ), result);
    Delete(result, 1, 1)
  end else
    result := IntToStr(typ)
end { portName } ;


function ioName(io: integer): string;

begin
  if io <= IO_MAX then
    Str(ioType(io), result)
  else
    result := IntToStr(io)
end { ioName } ;


function hex(b: byte): string;

begin
  result := '0x' + LowerCase(HexStr(b, 2))
end { hex } ;


function hex(w: word): string;

begin
  result := '0x' + LowerCase(HexStr(w, 4))
end { hex } ;


function hex(d: cardinal): string;

begin
  result := '0x' + LowerCase(HexStr(d, 8))
end { hex } ;


function hex(q: qword): string;

begin
  result := '0x' + LowerCase(HexStr(q, 16))
end { hex } ;


procedure serialInfo1(paramNum: integer);

var
  handle: TSerialHandle;
  serInfo: SerialStruct;
  rc, err, j: integer;

begin
  handle := SerOpen(ParamStr(paramNum));

(* SerOpen() does not try to get an exclusive lock on unix, to conform to the   *)
(* normal expectations engendered by Getty etc.                                 *)

  if handle < 0 then begin
    err := fpGetErrno;
    WriteLn(ErrOutput, 'Cannot open ', ParamStr(paramNum), ' info, error ', err, ': "', StrError(err), '".')
  end else
    try
      FillDWord(serInfo, SizeOf(serInfo) div SizeOf(birdseed), birdseed); (* Note canary fields in definition *)
      rc := fpIoctl(handle, TIOCGSERIAL, @serinfo);
      if rc <> 0 then begin
        err := fpGetErrno;
        WriteLn(ErrOutput, 'Cannot read ', ParamStr(paramNum), ' info, error ', err, ': "', StrError(err), '".');
        exit                            (* Via finally block                    *)
      end;
      WriteLn(ParamStr(paramNum), ':');
      with serinfo do begin
        WriteLn('  Type: ', portName(typ));
        WriteLn('  Line: ', line);
        WriteLn('  Port: ', hex(port));
        WriteLn('  IRQ: ', irq);
        WriteLn('  Flags: ', hex(flags));
        WriteLn('  Xmit FIFO: ', xmit_fifo_size);
        WriteLn('  Custom divisor: ', custom_divisor);
        WriteLn('  Baud base: ', baud_base);
        WriteLn('  Close delay: ', close_delay);
        WriteLn('  IO type: ', ioName(io_type));
        WriteLn('  Hub6: ', hub6);
        WriteLn('  Closing wait: ', closing_wait);
        WriteLn('  Closing wait 2: ', closing_wait2);
        WriteLn('  Iomem base: ', hex(iomem_base));
        WriteLn('  Iomem shift: ', iomem_reg_shift);
        WriteLn('  Port high: ', hex(port_high));
        WriteLn('  Iomap base: ', hex(iomap_base));
        j := 0;
        repeat
          WriteLn('  Canary[', j, ']: ', hex(canary[j]), ' (should be ', hex(birdseed), ')');
          j += 1
        until (j > High(canary)) or (canary[j - 1] = birdseed)
      end
    finally
      SerClose(handle)
    end
end { serialInfo1 } ;


begin
  for i := 1 to ParamCount() do
    serialInfo1(i)
end.

