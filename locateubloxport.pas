(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit LocateUbloxPort;

(* Find one or more serial ports, either standalone converters or integrated    *)
(* into an instrument (in this example a U-Blox GPS receiver), based on a CDC   *)
(* serial device. This is a specimen unit, and will usually be replaced by       *)
(* something with a tighter description based on lsusb etc. output.             *)
(*                                                              MarkMLl         *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  a CDC serial device embedded in a U-Blox GPS receiver.
*)
function FindUbloxPort(generation: integer= 7; portScan: boolean= false): string;

(* Similar to FindUbloxPort() but returns all ports (rather than just one) as a
  comma-separated list.
*)
function FindUbloxPorts(generation: integer= 7; portScan: boolean= false): string;


implementation

uses
  LocatePorts;

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


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a CDC serial device embedded in a U-Blox GPS receiver.
*)
function FindUbloxPort(generation: integer= 7; portScan: boolean= false): string;

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
  result := FindPortByDescription(description)
end { FindUbloxPort } ;


(* Similar to FindUbloxPort() but returns all ports (rather than just one) as a
  comma-separated list.
*)
function FindUbloxPorts(generation: integer= 7; portScan: boolean= false): string;

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
  with FindPortsByDescription(description) do begin
    result := CommaText;
    Free
  end
end { FindUbloxPorts } ;


end.

