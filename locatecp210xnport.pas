(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit LocateCp210xNPort;

(* Find one or more serial ports, either standalone converters or integrated    *)
(* into an instrument (in this example a Mastech MS2115B), based on a Silicon   *)
(* Labs CP210x. This is a specimen unit, and will usually be replaced by        *)
(* something with a tighter description based on lsusb etc. output.             *)
(*                                                              MarkMLl         *)

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
function FindContecPort(portScan: boolean= false): string;

// TODO : Check applicability of comment

(* Similar to FindContecPort() but returns all ports (rather than just one) as a
  comma-separated list.
*)
function FindContecPorts(portScan: boolean= false): string;


implementation

uses
  LocatePorts;


const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyUSB';
                         idVendor: '';
                         idProduct: '';
                         busType: '';
                         driverName: 'usb-serial/drivers/cp210x';
                         manufacturer: 'Silicon Labs';
                         product: 'CP2102N USB to UART Bridge Controller'; (* N suffix might *)
                         serial: ''        (* indicate non-constant serial number. *)
                       );


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a CP210x which is what's in the Mastech 2115B. Note that this is also used by
  the DSO112A, so it's not entirely foolproof... if it were an FTDI chip it
  could be branded with the device serial number (or model number etc.) as a
  one-time operation which would make it more reliable.
*)
function FindContecPort(portScan: boolean= false): string;

begin
  result := FindPortByDescription(descriptionTemplate)
end { FindContecPort } ;


(* Similar to FindContecPort() but returns all ports (rather than just one) as a
  comma-separated list.
*)
function FindContecPorts(portScan: boolean= false): string;

begin
  with FindPortsByDescription(descriptionTemplate) do begin
    result := CommaText;
    Free
  end
end { FindContecPorts } ;


end.

