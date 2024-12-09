(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit locate16550port;

(* Find one or more generic serial ports based on the 16550 chip. This is a     *)
(* specimen unit, and will usually be replaced by something with a tighter      *)
(* description based on lsusb etc. output.                      MarkMLl         *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  a 16550 chip or derivative.
*)
function Find16550PortPnp(): string;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  a 16550 chip or derivative.
*)
function Find16550PortPci(): string;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  a 16550 chip or derivative, the result is comma-separated.
*)
function Find16550PortAll(): string;


implementation

uses
  LocatePorts;

{ TODO : Detection of actual ports might have changed around kernel 6.8. }
// https://forum.lazarus.freepascal.org/index.php/topic,69437.msg539187.html


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a 16550 chip or derivative.
*)
function Find16550PortPnp(): string;

const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyS';
                         idVendor: '';
                         idProduct: '';
                         busType: 'pnp*';
                         driverName: 'pnp/drivers/serial'
                       );

begin
  result := FindPortByDescription(descriptionTemplate)
end { Find16550PortPnp } ;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a 16550 chip or derivative.
*)
function Find16550PortPci(): string;

const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyS';
                         idVendor: '';
                         idProduct: '';
                         busType: 'pci*';
                         driverName: 'pci/drivers/serial'
                       );

begin
  result := FindPortByDescription(descriptionTemplate)
end { Find16550PortPci } ;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a 16550 chip or derivative, the result is comma-separated.
*)
function Find16550PortAll(): string;

const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyS';
                         idVendor: '';
                         idProduct: '';
                         busType: '';
                         driverName: 'drivers/serial'
                       );

begin
  with FindPortsByDescription(descriptionTemplate) do begin
    result := CommaText;
    Free
  end
end { Find16550PortAll } ;


end.

