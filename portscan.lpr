(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

program portscan;

(* Demo program for detection of various types of serial port.  MarkMLl         *)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, Locate8250Port, Locate16550Port, LocateFtdiPort, LocateCdcAcmPort,
                LocateCh34xPort, LocateCp210xPort, LocateCp210xNPort,
                LocatePl2303Port, LocateUbloxPort, LocateHpibPort, LocatePorts;

const
  debug= false;                         (* Blow-by-blow account to stderr       *)

begin
  LocatePortsDebug := debug;

{  Write('Scanning for legacy 8250 device... ');
  if debug then
    WriteLn;
  WriteLn(Find8250Port());
  if debug then
    DumpCachedPorts; }

  Write('Scanning for all legacy 8250 devices... ');
  if debug then
    WriteLn;
  WriteLn(Find8250PortAll());
  if debug then
    DumpCachedPorts;

{  Write('Scanning for PnP 16550 device... ');
  if debug then
    WriteLn;
  WriteLn(Find16550PortPnp());
  if debug then
    DumpCachedPorts; }

{  Write('Scanning for PCI 16550 device... ');
  if debug then
    WriteLn;
  WriteLn(Find16550PortPci());
  if debug then
    DumpCachedPorts; }

  Write('Scanning for all 16550 devices... ');
  if debug then
    WriteLn;
  WriteLn(Find16550PortAll());
  if debug then
    DumpCachedPorts;

{  Write('Scanning for unserialised FTDI device... ');
  if debug then
    WriteLn;
  WriteLn(FindFtdiPort(''));
  if debug then
    DumpCachedPorts; }

{  Write('Scanning for CDC serial device... ');
  if debug then
    WriteLn;
  WriteLn(FindFenrirPort());
  if debug then
    DumpCachedPorts; }

{  Write('Scanning for WCH CH34x device... ');
  if debug then
    WriteLn;
  WriteLn(FindJds6600Port());
  if debug then
    DumpCachedPorts; }

{  Write('Scanning for Silicon Labs CP210x device... ');
  if debug then
    WriteLn;
  WriteLn(FindMs2115bPort());
  if debug then
    DumpCachedPorts; }

{  Write('Scanning for Silicon Labs CP210xN device... ');
  if debug then
    WriteLn;
  WriteLn(FindContecPort());
  if debug then
    DumpCachedPorts; }

{  Write('Scanning for Prolific PL2303 device... ');
  if debug then
    WriteLn;
  WriteLn(FindPl2303Port(''));
  if debug then
    DumpCachedPorts; }

{  Write('Scanning for Generation 6 UBlox device... ');
  if debug then
    WriteLn;
  WriteLn(FindUbloxPort(6));
  if debug then
    DumpCachedPorts; }

{  Write('Scanning for Generation 7 UBlox device... ');
  if debug then
    WriteLn;
  WriteLn(FindUbloxPort(7));
  if debug then
    DumpCachedPorts; }

{  Write('Scanning for Prologix/Fenrir HP-IB device... ');
  if debug then
    WriteLn;
  WriteLn(FindHpibPort());
  if debug then
    DumpCachedPorts }
end.

