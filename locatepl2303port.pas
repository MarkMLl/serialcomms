(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit LocatePl2303Port;

(* Find a piece of test equipment etc. interfaced via an (often counterfeit)    *)
(* Prolific 2303 chip. If it were a genuine Prolific chip it might be branded   *)
(* with the device serial number (or model number etc.) as a one-time operation *)
(* which would make it more reliable.                           MarkMLl         *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  a (counterfeit) Prolific chip. Some variants might be branded with a serial
  number, but by default the driver does not present this field (lsusb shows it
  as 0).
*)
function FindPl2303Port(serialNumber: string= ''; portScan: boolean= false): string;


implementation

uses
  LocatePorts;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a (counterfeit) Prolific chip. Some variants might be branded with a serial
  number, but by default the driver does not present this field (lsusb shows it
  as 0).
*)
function FindPl2303Port(serialNumber: string= ''; portScan: boolean= false): string;

const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyUSB';
                         idVendor: '';
                         idProduct: '';
                         busType: '';
                         driverName: 'usb-serial/drivers/pl2303';
                         manufacturer: 'Prolific Technology Inc.';
                         product: 'USB-Serial Controller';
                         serial: '';
                       );

var
  description: TPortDescription;

begin
  description := descriptionTemplate;
  if serialNumber <> '' then
    description.serial := serialNumber;
  result := FindPortByDescription(description, portScan)
end { FindPl2303Port } ;


end.

