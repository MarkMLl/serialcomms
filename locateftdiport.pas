(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit LocateFtdiPort;

(* Find a piece of test equipment interfaced via an (often counterfeit) FTDI    *)
(* chip.                                                        MarkMLl         *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  a (counterfeit) FTDI chip. If it were a genuine FTDI chip it could be branded
  with the device serial number (or model number etc.) as a one-time operation
  which would make it more reliable, but most "clones" appear to have disabled
  this facility after FTDI's assertive actions of 2014.
*)
function FindFtdiPort(serialNumber: string= ''; portScan: boolean= false): string;


implementation

uses
  LocatePorts;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a (counterfeit) FTDI chip. If it were a genuine FTDI chip it could be branded
  with the device serial number (or model number etc.) as a one-time operation
  which would make it more reliable, but most "clones" appear to have disabled
  this facility after FTDI's assertive actions of 2014.
*)
function FindFtdiPort(serialNumber: string= ''; portScan: boolean= false): string;

const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyUSB';
                         idVendor: '';
                         idProduct: '';
                         busType: '';
                         driverName: 'usb-serial/drivers/ftdi_sio';
                         manufacturer: ''; (* Avoid: varies on counterfeits etc. *)
                         product: 'FT232R USB UART';
                         serial: 'A50285BI' (* Common in counterfeit R/O EEPROM *)
                       );

var
  description: TPortDescription;

begin
  description := descriptionTemplate;
  if serialNumber <> '' then
    description.serial := serialNumber;
  result := FindPortByDescription(description, portScan)
end { FindFtdiPort } ;


end.

