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

var
  ports: string;

begin
  LocatePortsDebug := debug;

  Write('Scanning for legacy 8250 device... ');
  if debug then
    WriteLn;
  WriteLn(Find8250Port());
  if debug then
    DumpCachedPorts;

  Write('Scanning for all legacy 8250 devices... ');
  if debug then
    WriteLn;
  WriteLn(Find8250PortAll());
  if debug then
    DumpCachedPorts;

  Write('Scanning for PnP 16550 device... ');
  if debug then
    WriteLn;
  WriteLn(Find16550PortPnp());
  if debug then
    DumpCachedPorts;

  Write('Scanning for PCI 16550 device... ');
  if debug then
    WriteLn;
  WriteLn(Find16550PortPci());
  if debug then
    DumpCachedPorts;

  Write('Scanning for all 16550 devices... ');
  if debug then
    WriteLn;
  WriteLn(Find16550PortAll());
  if debug then
    DumpCachedPorts;

  Write('Scanning for specific FTDI device... ');
  if debug then
    WriteLn;
  WriteLn(FindFtdiPorts('AR0JLDEV'));
  if debug then
    DumpCachedPorts;

  Write('Scanning for counterfeit FTDI devices... ');
  if debug then
    WriteLn;
  WriteLn(FindFtdiPorts(''));
  if debug then
    DumpCachedPorts;

  Write('Scanning for CDC serial devices... ');
  if debug then
    WriteLn;
  WriteLn(FindCdcPorts());
  if debug then
    DumpCachedPorts;

  Write('Scanning for WCH CH34x devices in JDS6600... ');
  if debug then
    WriteLn;
  WriteLn(FindJds6600Ports());
  if debug then
    DumpCachedPorts;

  Write('Scanning for Silicon Labs CP210x device in MS2115B... ');
  if debug then
    WriteLn;
  WriteLn(FindMs2115bPorts());
  if debug then
    DumpCachedPorts;

  Write('Scanning for Silicon Labs CP210xN device in Contec Pulse Oximeter... ');
  if debug then
    WriteLn;
  WriteLn(FindContecPorts());
  if debug then
    DumpCachedPorts;

  Write('Scanning for Prolific PL2303 device... ');
  if debug then
    WriteLn;
  WriteLn(FindPl2303Ports(''));
  if debug then
    DumpCachedPorts;

  Write('Scanning for Generation 6 UBlox device... ');
  if debug then
    WriteLn;
  WriteLn(FindUbloxPorts(6));
  if debug then
    DumpCachedPorts;

  Write('Scanning for Generation 7 UBlox device... ');
  if debug then
    WriteLn;
  WriteLn(FindUbloxPorts(7));
  if debug then
    DumpCachedPorts;

  Write('Scanning for Prologix/Fenrir HP-IB device... ');
  if debug then
    WriteLn;
  WriteLn(FindHpibPorts());
  if debug then
    DumpCachedPorts
end.

