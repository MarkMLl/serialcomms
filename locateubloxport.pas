(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit LocateUbloxPort;

(* Find a piece of test equipment (specifically, a Fenrir HP-IB interface) with *)
(* an embedded PIC behaving like a CDC serial device.           MarkMLl.        *)

// TODO : Unit untested
// TODO : Would benefit from spaces being compacted in product name IN ALL CASES

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
function FindUbloxPort(generation: integer= 7; portScan: boolean= false): string;


implementation

uses
  LocatePorts;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a PIC acting as a CDC serial device. Note that this is also used by many other
  embedded controllers including some Arduinos so it's not entirely foolproof...
  if it were an FTDI chip it could be branded with the device serial number (or
  model number etc.) as a one-time operation which would make it more reliable.
*)
function FindUbloxPort(generation: integer= 7; portScan: boolean= false): string;

const
  descriptionTemplate: TPortDescription= (
                         baseName: 'ttyACM';
                         idVendor: '1546';
                         idProduct: '01a7';
                         busType: '';
                         driverName: '';        (* Not reliable, don't use      *)
                         manufacturer: '';
                         product: 'u-blox 7 - GPS/GNSS Receiver';
                       {%H-});

(* Apropos finding the drivername: see the command                              *)
(*                                                                              *)
(* # udevadm info -q all -n /dev/ttyACM0 --attribute-walk                       *)
(*                                                                              *)
(* suggested by https://back7.co/home/scaling-octoprint-with-a-raspberry-pi     *)
(* which was originally from Prusa. However while this appears to work with an  *)
(* FTDI interface etc. it doesn't work with CDC-ACM.                            *)

var
  description: TPortDescription;

begin
  description := descriptionTemplate;
  with description do
    case generation of
      6: begin
           idproduct := '01a6';
           product := 'u-blox 6  -  GPS Receiver' (* Spaces actually get squashed *)
         end
    otherwise
    end;
  result := FindPortByDescription(description, portScan)
end { FindUbloxPort } ;


end.

