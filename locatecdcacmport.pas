(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit LocateCdcAcmPort;

(* Find a piece of test equipment (specifically, a Fenrir HP-IB interface) with *)
(* an embedded PIC behaving like a CDC serial device.           MarkMLl.        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  a PIC acting as a CDC serial device. Note that this is also used by many other
  embedded controllers including some Arduinos so it's not entirely foolproof...
  if it were an FTDI chip it could be branded with the device serial number (or
  model number etc.) as a one-time operation which would make it more reliable.
*)
function FindFenrirPort(portScan: boolean= false): string;


implementation

uses
  LocatePorts;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a PIC acting as a CDC serial device. Note that this is also used by many other
  embedded controllers including some Arduinos so it's not entirely foolproof...
  if it were an FTDI chip it could be branded with the device serial number (or
  model number etc.) as a one-time operation which would make it more reliable.
*)
function FindFenrirPort(portScan: boolean= false): string;

const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyACM';
                         idVendor: '10c4';
                         idProduct: '8a5f'
                       {%H-});

begin
  result := FindPortByDescription(descriptionTemplate, portScan)
end { FindFenrirPort } ;


end.

