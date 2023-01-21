(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit LocateCh34xPort;

(* Find a piece of test equipment (specifically, a Juntek JDS6600) with an      *)
(* embedded Nanjing Qinheng Microelectronics Co (WCH) CH34x chip. MarkMLl.      *)

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
                         product: 'USB Serial';
//                         serial: '0001' "Which in your case you have not got"
                       );

begin
  result := FindPortByDescription(descriptionTemplate, portScan)
end { FindJds6600Port } ;


end.

