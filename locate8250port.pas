(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit Locate8250Port;

(* Find one or more generic serial ports based on the 8250 chip. This is a      *)
(* specimen unit, and will usually be replaced by something with a tighter      *)
(* description based on lsusb etc. output.                      MarkMLl         *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  an 8250 chip or derivative.
*)
function Find8250Port(): string;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  an 8250 chip or derivative, the result is comma-separated.
*)
function Find8250PortAll(): string;


implementation

uses
  LocatePorts, Serial, BaseUnix, termio;


(* Attempt to open the device and if successful see if internal loopback works.
*)
function accessible(portname: string): string;

var
  handle: TSerialHandle;
  ch: char= 'L';


  procedure SerSetLoop(Handle: TSerialHandle; State: Boolean);
  const
    loop: Cardinal = $8000; // TIOCM_LOOP;
  begin
    if State then
      fpioctl(Handle, TIOCMBIS, @loop)
    else
      fpioctl(Handle, TIOCMBIC, @loop);
  end;


begin
  result := portName;
  if portName = '' then
    exit;
  handle := SerOpen(portName);
  if handle <= 0 then
    exit;
  try
    SerSetLoop(handle, true);
    if SerWrite(handle, ch, 1) <> 1 then
      exit('');
    if SerReadTimeout(handle, ch, 10) < 1 then
      exit('');
    if ch <> 'L' then
      exit('')
  finally
    SerSetLoop(handle, false);
    SerClose(handle)
  end
end { accessible } ;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  an 8250 chip or derivative.
*)
function Find8250Port(): string;

const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyS';
                         idVendor: '';
                         idProduct: '';
                         busType: 'platform';
                         driverName: 'platform/drivers/serial8250'
                       );

begin
  result := accessible(FindPortByDescription(descriptionTemplate))
end { Find8250Port } ;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  an 8250 chip or derivative, the result is comma-separated.
*)
function Find8250PortAll(): string;

const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyS';
                         idVendor: '';
                         idProduct: '';
                         busType: 'platform';
                         driverName: 'platform/drivers/serial8250'
                       );

var
  allPorts: TStringList;
  i: integer;

begin
  result := '';
  allPorts := FindPortsByDescription(descriptionTemplate);
  for i := 0 to allPorts.Count - 1 do
    if accessible(allPorts[i]) <> '' then
      if result = '' then
        result := allPorts[i]
      else
        result += ',' + allPorts[i];
  allPorts.Free
end { Find8250PortAll } ;


end.























// Placeholder: remember that the kernel might think it has /dev/ttyS2
// and /dev/ttyS3 even if they are a fiction, and that detection of
// actual ports might have changed around kernel 6.8.
//
// https://forum.lazarus.freepascal.org/index.php/topic,69437.msg539187.html

