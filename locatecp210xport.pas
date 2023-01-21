(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit LocateCp210xPort;

(* Find a piece of test equipment (specifically, a Mastech MS2115B) with an     *)
(* embedded Silicon Labs CP210x chip.                           MarkMLl.        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  a CP210x which is what's in the Mastech 2115B. Note that this is also used by
  the DSO112A, so it's not entirely foolproof... if it were an FTDI chip it
  could be branded with the device serial number (or model number etc.) as a
  one-time operation which would make it more reliable.
*)
function FindMs2115bPort(portScan: boolean= false): string;


implementation

uses
  LocatePorts;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a CP210x which is what's in the Mastech 2115B. Note that this is also used by
  the DSO112A, so it's not entirely foolproof... if it were an FTDI chip it
  could be branded with the device serial number (or model number etc.) as a
  one-time operation which would make it more reliable.
*)
function FindMs2115bPort(portScan: boolean= false): string;

const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyUSB';
                         idVendor: '';
                         idProduct: '';
                         busType: '';
                         driverName: 'usb-serial/drivers/cp210x';
                         manufacturer: 'Silicon Labs';
                         product: 'CP2102 USB to UART Bridge Controller'; (* N suffix might *)
                         serial: '0001'        (* indicate non-constant serial number. *)
                       );

begin
  result := FindPortByDescription(descriptionTemplate, portScan)
end { FindMs2115bPort } ;


end.

