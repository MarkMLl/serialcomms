(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit LocateCh34xPort;

(* Find one or more serial ports, either standalone converters or integrated    *)
(* into an instrument (in this example a Juntek JDS6600), based on a Nanjing    *)
(* Qinheng Microelectronics Co (WCH) CH34x. This is a specimen unit, and will   *)
(* usually be replaced by something with a tighter description based on lsusb   *)
(* etc. output.                                                 MarkMLl         *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  a CH34x which is what's in the Juntek JDS6600. Note that this is not entirely
  foolproof... if it were an FTDI chip it could be branded with the device
  serial number (or model number etc.) as a one-time operation which would make
  it more reliable.
*)
function FindJds6600Port(portScan: boolean= false): string;


implementation

uses
  LocatePorts;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a CH34x which is what's in the Juntek JDS6600. Note that this is not entirely
  foolproof... if it were an FTDI chip it could be branded with the device
  serial number (or model number etc.) as a one-time operation which would make
  it more reliable.
*)
function FindJds6600Port(portScan: boolean= false): string;

const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyUSB';
                         idVendor: '';
                         idProduct: '';
                         busType: '';
                         driverName: 'usb-serial/drivers/ch341';
                         manufacturer: '';
//                         product: 'USB Serial';
 product: 'USB2.0-Serial';
//                         serial: '0001' "Which in your case you have not got"
                       );

begin
  result := FindPortByDescription(descriptionTemplate)
end { FindJds6600Port } ;


end.

