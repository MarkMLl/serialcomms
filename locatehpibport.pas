(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit LocateHpibPort;

(* Find one or more serial ports, either standalone converters or integrated    *)
(* into an instrument (in this example a Prologix or Fenrir HP-IB interface),   *)
(* based on a CDC serial devce. This is a specimen unit, and will usually be   *)
(* replaced by something with a tighter description based on lsusb etc. output. *)
(*                                                              MarkMLl         *)

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
function FindHpibPort(portScan: boolean= false): string;


implementation

uses
  LocatePorts;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a PIC acting as a CDC serial device. Note that this is also used by many other
  embedded controllers including some Arduinos so it's not entirely foolproof...
  if it were an FTDI chip it could be branded with the device serial number (or
  model number etc.) as a one-time operation which would make it more reliable.
*)
function FindHpibPort(portScan: boolean= false): string;

const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyACM';
                         idVendor: '10c4';
                         idProduct: '8a5f'
                       {%H-});

begin
  result := FindPortByDescription(descriptionTemplate)
end { FindHpibPort } ;


end.

