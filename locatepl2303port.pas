(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit LocatePl2303Port;

(* Find one or more serial ports, either standalone converters or integrated    *)
(* into an instrument, based on a Prolific PL2303 chip (in practice this will   *)
(* often be a counterfeit). This is a specimen unit, and will usually be        *)
(* replaced by something with a tighter description based on lsusb etc. output. *)
(*                                                                              *)
(* If it were a genuine Prolific chip it might be branded with the device       *)
(* serial number (or model number etc.) as a one-time operation which would     *)
(* make detection more reliable.                                MarkMLl         *)

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

(* Similar to FindPl2303Port() but returns all ports (rather than just one) as a
  comma-separated list.
*)
function FindPl2303Ports(serialNumber: string= ''; portScan: boolean= false): string;


implementation

uses
  LocatePorts;

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


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a (counterfeit) Prolific chip. Some variants might be branded with a serial
  number, but by default the driver does not present this field (lsusb shows it
  as 0).
*)
function FindPl2303Port(serialNumber: string= ''; portScan: boolean= false): string;

var
  description: TPortDescription;

begin
  description := descriptionTemplate;
  if serialNumber <> '' then
    description.serial := serialNumber;
  result := FindPortByDescription(description)
end { FindPl2303Port } ;


(* Similar to FindPl2303Port() but returns all ports (rather than just one) as a
  comma-separated list.
*)
function FindPl2303Ports(serialNumber: string= ''; portScan: boolean= false): string;

var
  description: TPortDescription;

begin
  description := descriptionTemplate;
  if serialNumber <> '' then
    description.serial := serialNumber;
  with FindPortsByDescription(description) do begin
    result := CommaText;
    Free
  end
end { FindPl2303Ports } ;


end.

