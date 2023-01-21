(* Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FPC 1.6.4+3.0.2 on Linux Lazarus+FP *)

unit LocatePorts;

(* Construct a list containing anything that looks like a Serial port. Also     *)
(* implement a couple of "must-have" medium-resolution timing etc. functions    *)
(* for message timeouts etc.                                    MarkMLl.        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Serial;

const
  InvalidSerialHandle= TSerialHandle(-1);
  TimerResolution= 100;                 (* Ticks per second                     *)
  TimerMaxPeriod= 1000000;              (* Ticks                                *)
  Second= TimerResolution;
  TenthSecond= Second div 10;
  HundredthSecond= Second div 100;
{$ifdef LINUX }
  LargestSafeRead= 4095;                (* Linux kernel restriction             *)
{$else        }
  LargestSafeRead= $7fffffff;
{$endif LINUX }


(* Observe that for low-resolution clocks a local Second div 10 or div 100      *)
(* quite simply won't work.                                                     *)

type
(* This is a low-resolution timer for handling things like serial protocol
  turnaround and retry delays. It is not intended for high-speed events or
  extended durations.
*)
  TimerDescriptor= qword;

(* A numbered list of ports is prepared at startup, or any time this is called
  and no open port is specified. The list is cached as necessary and also
  returned as a StringList, which should be freed by the caller.
*)
function ListPorts(currentPort: TSerialHandle= InvalidSerialHandle): TStringList;

(* Assuming that the cached list has been properly populated, dump its contents.
*)
procedure DumpCachedPorts;

type
(* This represents the description of a type of device. Assume it is used to
  initialise a constant record, e.g. something like

  description: TPortDescription= (
                 baseName: 'ttyUSB';
                 idVendor: '';
                 idProduct: '';
                 busType: '';
                 driverName: 'usb-serial/drivers/cp210x';
                 manufacturer: 'Silicon Labs';
                 product: 'CP2102 USB to UART Bridge Controller';
                 serial: '0001'
               );

  or

  description: TPortDescription= (
                 baseName: 'ttyACM';
                 idVendor: '10c4';
                 idProduct: '8a5f'
               );

  with blank fields having significance as noted. Capitalisation and repeated
  spaces are squashed in the product name to make transcription a bit less
  fraught.
*)
  TPortDescription= record
                      baseName: string;   (* This field is mandatory, e.g. ttyS *)
                      idVendor: string;   (* Not tested if blank                *)
                      idProduct: string;  (* Not tested if blank                *)
                      busType: string; // UNUSED (* Non-presence is ignored if blank   *)
                      driverName: string; (* Non-presence is ignored if blank   *)
                      manufacturer: string; (* Non-presence is ignored if blank *)
                      product: string;    (* Non-presence is ignored if blank   *)
                      serial: string      (* Non-presence is ignored if blank   *)
                    end;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  one with the described properties. Some types of serial device are used by
  more than one instrument (e.g. a CP210x may be found in both the Mastech
  2115B and the DSO112A) so it's not entirely foolproof... if it were a genuine
  FTDI chip it could be branded with the device serial number (or model number
  etc.) as a one-time operation which would make it more reliable.
*)
function FindPortByDescription(const description: TPortDescription;
                                        portScan: boolean= false): string;

(*  Return true if a device addition or removal has been detected.

  The hotplug (udev) events will be asynchronous to the actual addition or
  removal of entries in the /sys and /dev trees. As such a limited number are of
  actual use: a bind with the devicepath leading to vendor etc. information can
  be expected at about the same time this is stable in /sys etc., and a matching
  unbind shortly after the corresponding device has been removed.

  Note that the parameter's basename is the only field applicable to a device
  removal, and that the other parameters cannot be checked at removal.
*)
function PollHotplugEvents(const description: TPortDescription): boolean;

(*  Return true if a device addition or removal has been detected.

  The hotplug (udev) events will be asynchronous to the actual addition or
  removal of entries in the /sys and /dev trees. As such a limited number are of
  actual use: a bind with the devicepath leading to vendor etc. information can
  be expected at about the same time this is stable in /sys etc., and a matching
  unbind shortly after the corresponding device has been removed.

  It would be reasonable to test a port description against /sys at (or at least
  shortly after) the bind event, however by the time that the unbind is parsed
  the device will probably have been removed from the /sys and /dev trees. As
  such, for the moment at least, it's probably wisest to use this simply as a
  "something's changed" notification that prompts the main program to redo its
  scan for device types (e.g. serial interfaces) that interest it.
*)
function PollHotplugEvents(): boolean;

(* Like SerOpen(), but with a mandatory lock at the operating system level and
  explicitly setting a default line speed etc. with terminal modes zeroed.
*)
function SerOpenLocked(const deviceName: string): TSerialHandle;

(* Return the number of bytes buffered for reception by the operating system,
  or a -ve result on error. Since ioctl() returns an integer of indeterminate
  length via a pointer this has provision for investigating its actual size by
  presetting the return value to a non-zero bit pattern.
*)
function SerAvailable(handle: TSerialHandle; defaultResult: longint= 0): longint;

(* This is lifted from the DT800 MicroKernel. The parameter is set to represent
  the current time, for use only by Elapsed().
*)
procedure InitTimer(out td: TimerDescriptor);

(* Return true if the interval (measured in ticks) has expired.
*)
function Elapsed(const td: TimerDescriptor; interval: int64): boolean;

// TODO : Implement something like SerSetParamsNG(), existing code doesn't call tcgetattr().
// TODO : Consider something to list serial mode bits, see ICRNL etc. in rtl/linux/termios.inc


implementation

uses
  StrUtils, BaseUnix, Termio, Regexpr, Netlink, Sockets;

const
  cachedidVendor= 0;
  cachedidProduct= 1;
  cachedDriver= 2;
  cachedManufacturer= 3;
  cachedProduct= 4;
  cachedSerial= 5;
  cachedTop= cachedSerial;

var
  portsListCache: TStringList= nil;

// This currently works by laboriously parsing the /sys/devices tree, looking
// for subtrees which match specified predicates. An alternative would be to
// use  udevadm info  (or equivalent functionality from a system library) to
// list everything that's known about e.g. /dev/tty* devices, and to work from
// vendor and product IDs to something usable by the usual unix-style APIs.
//
// This would be approximately compatible with device addition/removal messages
// received from udev via the netlink socket, and would open up the possibility
// of handling the initial population of e.g. a device selection combobox by
// simulating a series of hotplug messages for suitable devices which already
// existed at program startup.

{ define DEBUGWRITEF }


(* Read manufacturer name etc. This is a standard file operation handling data
  generated by the kernel, so I'm being fairly relaxed about the risk of
  overrun etc.
*)
function cat(const fn: string): string;

var
  handle: THandle;
  buffer: array[0..255] of byte;

begin
  handle := FileOpen(fn, fmOpenRead);
  if handle >= 0 then
    try
      FillByte(buffer{%H-}, SizeOf(buffer), 0);
      if FileRead(handle, buffer, SizeOf(buffer) - 1) > 0 then
        result := Trim(StrPas(Addr(buffer)))    (* Discard trailing EOL etc.    *)
      else
        result := ''
    finally
      FileClose(handle)
    end
  else
    result := ''
end { cat } ;


(* Prior to FPC 3.2.0 FileExists() also matched directories. This function is
  optimised by version to avoid a spurious call if FileExists() fails on FPC
  3.0.4 or older.
*)
function fileOrDirectoryExists(const name: RawByteString; followLink: boolean= true): boolean;

begin
{$if FPC_FULLVERSION < 030200 }
  Assert(followLink = true, 'Internal error: unsupported no-follow-link.');
  result := FileExists(name)
{$else                        }
  result := FileExists(name, followLink) or DirectoryExists(name, followLink)
{$endif FPC_FULLVERSION       }
end { fileOrDirectoryExists } ;


procedure debugWriteF(fmt{%H- 5024 }: string; values{%H- 5024 }: array of const);

// See FormatEx() in e.g. Borg-NG for possible enhancements.

var
  i{%H- 5025 }: integer;

begin
{$ifdef DEBUGWRITEF }
  i := Pos('\n', fmt);
  if i > 0 then begin
    SetLength(fmt, i - 1);
    WriteLn(ErrOutput, Format(fmt, values))
  end else
    Write(ErrOutput, Format(fmt, values))
{$endif DEBUGWRITEF }
end { debugWriteF } ;


type
  TWalkTestPredicate= function(const dir: string; const name: string;
                        const content: string; const extra: string;
                                                hint: integer= -1): boolean;


(* Look in the directory specified as the first parameter (terminated by /) and
  either return the canonical path if a directory matching the second parameter
  is encountered or recurse into any other directories. If the third parameter
  exists then the directory that is found must contain it, however it is not
  appended to the result and the result is not terminated by /, in other cases
  the result is terminated by /. The final parameter is an optional test which
  is applied after all naming requirements have been met.

  We know that this is being used on the /sys filesystem which contains numerous
  recursive symlinks, so need to look at real directories *only*. The format of
  the directory names appears to have been stable from Linux 2.6 to at least 4,
  however the position in the tree that the hidraw directory is located depends
  on e.g. whether the mouse device is connected by USB or Bluetooth:

  USB: /sys/devices/pci0000:00/0000:00:03.1/usb4/4-3/4-3:1.0/0003:1BCF:0053.0009/hidraw

  Bt:  /sys/devices/virtual/misc/uhid/0005:248A:8266.000D/hidraw

  The hidraw directory (note: no appended number) contains an hidrawN directory
  (note: one or more decimal digits appended) which should match something in
  /dev giving us a file name which can be handled by open(), ioctl() and so on.
*)
function walkDirs(dir: string; pattern: string; content: string= '';
                        wtp: TWalkTestPredicate= nil; extra: string= '';
                                                hint: integer= -1): string;

(* This is lifted from my Logitech G600 interface program.                      *)

var
  searchRec: TUnicodeSearchRec;
  sorter: TStringList;
  i: integer;
  candidate: string;


  function wtp2(const dir: string; const name: string; const content: string;
                        const extra: string; hint: integer= -1): boolean; inline;

  begin
    if assigned(wtp) then
      result := wtp(dir, name, content, extra, hint)
    else
      result := true
  end { wtp2 } ;


begin
  result := '';
  if FindFirst(dir + '*'{%H-}, faDirectory, searchRec) = 0 then begin
    sorter := TStringList.Create;
    sorter.Sorted := true;
    try

(* It is not safe to assume that the kernel returns names in a natural or       *)
(* reproducible order, particularly when looking at e.g. /sys, so handle it in  *)
(* two stages.                                                                  *)

      repeat
        if searchRec.Name[1] = '.' then (* Always ignore . and .. completely    *)
          continue;

(* For everything in the directory, make sure it's a subdirectory and not a     *)
(* symlink. Don't try to use searchRec.Attr for this.                           *)

        if FileGetAttr({%H-}dir + searchRec.Name) and (faDirectory + faSymLink{%H-}) = faDirectory then
          sorter.Add(searchRec.Name{%H-})
      until FindNext(searchRec) <> 0;
      for i := 0 to sorter.Count - 1 do begin
        candidate := sorter[i];         (* Make visible for debugging           *)

(* Does the subdirectory name match the pattern?                                *)

        if IsWild(candidate, pattern, false) then

(* If there's content specified does it exist in the subdirectory, a symlink    *)
(* being acceptable in this case? Note that this also determines whether / is   *)
(* appended to the result, the rationale being that if we are explicitly        *)
(* looking for some content then we're likely to be continuing to process the   *)
(* tree in some form. If all conditions are satisfactory then apply one final   *)
(* test passed as a function, which can be used to distinguish between e.g.     *)
(* multiple endpoints associated with the same USB device.                      *)

          if content = '' then
            if wtp2(dir, candidate, content, extra, hint) then
              exit(dir + candidate)
            else begin end              (* Watch for dangling else here         *)
          else begin
            DebugWriteF('Test existence "%s" "%s" / "%s" ', [dir, candidate, content]);
            if fileOrDirectoryExists(dir + candidate + '/' + content) then begin
              DebugWriteF('OK\n', []);
              DebugWriteF('Test predicate "%s" "%s" / "%s" "%s" ', [dir, candidate, content, extra]);
              if wtp2(dir, candidate, content, extra, hint) then begin
                DebugWriteF('OK\n', []);
                exit(dir + candidate + '/')
              end else
                DebugWriteF('Failed\n', [])
            end else
              DebugWriteF('Failed\n', [])
          end;

(* We've got a subdirectory but either it doesn't match the pattern or it lacks *)
(* the expected content: recurse.                                               *)

        result := walkDirs(dir + candidate + '/', pattern, content, wtp, extra, hint);

(* If recursion found a hit then return it.                                     *)

        if result <> '' then
          exit
      end
    finally
      Sorter.Free;
      FindClose(searchRec)
    end
  end
end { walkDirs } ;


(* Callback for TStringList.CustomSort(). Assume that the list contains
  name=value pairs, and order by value.
*)
function compareValues(list: TStringList; index1: integer; index2: integer): integer;

var
  v1, v2: integer;

begin
  v1 := StrToIntDef(ExtractDelimited(2, list[index1], ['=']), -99999);
  v2 := StrToIntDef(ExtractDelimited(2, list[index2], ['=']), +99999);
  if v1 = v2 then
    result := 0
  else
    if v1 < v2 then
      result := -1
    else
      result := +1
end { compareValues } ;


(* A numbered list of ports is prepared at startup, or any time this is called
  and no open port is specified. The list is cached as necessary and also
  returned as a StringList, which should be freed by the caller.
*)
function ListPorts(currentPort: TSerialHandle= InvalidSerialHandle): TStringList;

(* This is lifted from a port I'm working on of some very old DOS/Windows code  *)
(* implementing a proprietary protocol.                                         *)

var
  searchRec: TSearchRec;
  majorList, minorList: TStringList;
  r: TRegExpr;
  i, j: integer;


  (* The parameter is a number, extract as implemented in the clib macros.
  *)
  function major(dev: qword): dword;

//  __NTH (gnu_dev_major (unsigned long long int __dev))
//  {
//    return ((__dev >> 8) & 0xfff) | ((unsigned int) (__dev >> 32) & ~0xfff);
//  }

  begin
    result := ((dev shr 8) and $0fff) or ((dev shr 32) and $fffff000)
  end { major } ;


  (* The parameter is a number, extract as implemented in the clib macros.
  *)
  function minor(dev: qword): dword;

//  __NTH (gnu_dev_minor (unsigned long long int __dev))
//  {
//    return (__dev & 0xff) | ((unsigned int) (__dev >> 12) & ~0xff);
//  }

  begin
    result := (dev and $00ff) or ((dev shr 12) and $ffffff00)
  end { minor } ;


  (* The parameter is a number as documented in the inode structure.
  *)
  function isChr(mode: dword): boolean;

  begin
    result := mode and &0120000 = &0020000
  end { isChr } ;


  (* The parameter is the entire device name.
  *)
  function major(const devName: string): dword;

  var
    devStat: Stat;

  begin
    FillByte(devStat{%H-}, SizeOf(devStat), 0);
    if FpStat(devName, devStat) <> 0 then
      result := 0
    else
      result := major(devStat.st_rdev)
  end { major } ;


  (* The parameter is the entire device name.
  *)
  function minor(const devName: string): dword;

  var
    devStat: Stat;

  begin
    FillByte(devStat{%H-}, SizeOf(devStat), 0);
    if FpStat(devName, devStat) <> 0 then
      result := 0
    else
      result := minor(devStat.st_rdev)
  end { minor } ;


  (* The parameter is the entire device name.
  *)
  function isChr(const devName: string): boolean;

  var
    devStat: Stat;

  begin
    FillByte(devStat{%H-}, SizeOf(devStat), 0);
    if FpStat(devName, devStat) <> 0 then
      result := false
    else
      result := isChr(devStat.st_mode)
  end { isChr } ;


  (* The parameter is the first part of the device name, i.e. with no appended
    digits.
  *)
  function blacklisted(const {%H-}devName: string): boolean;

  begin
    result := false
  end { blacklisted } ;


begin
  if currentPort <> InvalidSerialHandle then begin
    if portsListCache = nil then
      result := nil
    else begin
      result := TStringList.Create;
      result.Assign(portsListCache)
    end;
    exit
  end;
  if portsListCache = nil then
    portsListCache := TStringList.Create
  else begin
    for i := 0 to portsListCache.Count - 1 do
      if portsListCache.Objects[i] <> nil then begin
        Assert(portsListCache.Objects[i] is TStringList, 'Internal error: bad cached port list.');
        TStringList(portsListCache.Objects[i]).Free
      end;
    portsListCache.Clear
  end;
  majorList := TStringList.Create;
  minorList := TStringList.Create;
  r := TRegExpr.Create;
  r.Expression := '(tty\D+)\d+';
  try

(* Run through all devices named like /dev/tty[[:alpha:]]+[[:digit:]]+ checking *)
(* that they're character-mode and not blacklisted and saving the major device  *)
(* number; repeat for /dev/rfcomm0 etc. to accommodate the default naming       *)
(* convention of Bluetooth-connected serial devices. The final result is a list *)
(* of devices without appended digits, plus the associated major device number. *)

    majorList.Sorted := true;
    majorList.Duplicates := dupIgnore;
    IF FindFirst('/dev/tty*', faSysFile{%H-}, searchRec) = 0 THEN
      REPEAT
        if r.Exec(searchRec.Name) and isChr('/dev/' + searchRec.Name) then
          if not blacklisted('/dev/' + r.Match[1]) then
            majorList.Add('/dev/' + r.Match[1] + '=' + IntToStr(major('/dev/' + searchRec.Name)))
      UNTIL FindNext(searchRec) <> 0;
    FindClose(searchRec);
    r.Expression := '(rfcomm)\d+';
    IF FindFirst('/dev/rfcomm*', faSysFile{%H-}, searchRec) = 0 THEN
      REPEAT
        if r.Exec(searchRec.Name) and isChr('/dev/' + searchRec.Name) then
          if not blacklisted('/dev/' + r.Match[1]) then
            majorList.Add('/dev/' + r.Match[1] + '=' + IntToStr(major('/dev/' + searchRec.Name)))
      UNTIL FindNext(searchRec) <> 0;
    FindClose(searchRec);

(* Sort by major device number. Enumeration order might be determined by the    *)
(* order that kernel modules are loaded, in practice it's probably best to make *)
(* no assumption in which case the default Quicksort is probably appropriate.   *)

    majorList.Sorted := false;
    majorList.CustomSort(@compareValues);

(* For each major device get the actually-available physical devices. This      *)
(* assumes that udev or equivalent is being used to create devices dynamically, *)
(* i.e. won't work with Linux kernels older than 2.6 (circa 2003), in fact it   *)
(* doesn't even bother trying to check for statically-allocated systems since   *)
(* this would end up in a minefield of heuristics looking at the age of the     *)
(* kernel, whether it was possible to decide whether udev support was compiled  *)
(* into the kernel, whether systemd or equivalent was running and so on: it's   *)
(* quite simply not worth it for a change made 15 years ago particularly since  *)
(* this code "fails safe" by possibly listing more devices than actually exist  *)
(* rather than by hiding some which can't be opened as a result.                *)

    for i := 0 to majorList.Count - 1 do begin
      minorList.Clear;
      IF FindFirst(majorList.Names[i] + '*', faSysFile{%H-}, searchRec) = 0 THEN
        REPEAT
          minorList.Add('/dev/' + searchRec.Name + '=' + IntToStr(minor('/dev/' + searchRec.Name)))
        UNTIL FindNext(searchRec) <> 0;
      FindClose(searchRec);

(* In the context of the current major device, sort by minor device number. The *)
(* enumeration order might be reversed, which suggests that something like a    *)
(* comb sort would be appropriate; however this can't be relied on so again use *)
(* the default Quicksort.                                                       *)

      minorList.CustomSort(@compareValues);

(* Discarding major and minor device number, append the name to the cache.      *)

      for j := 0 to MinorList.Count - 1 do
        portsListCache.Append(minorList.Names[j])
    end;

(* The cached result should first have traditional serial devices /dev/ttySx,   *)
(* ISDN devices /dev/ttyIx, USB devices /dev/ttyUSBx with additional support    *)
(* for devices implemented by multiport cards etc. inserted as appropriate      *)
(* based on the major device numbers which were allocated approximately         *)
(* chronologically. It should also have any "Low-density serial ports" found    *)
(* to be present, hopefully at the end of the list, where those are e.g. on-    *)
(* chip console ports and are distinguished by minor rather than major device   *)
(* number:                                                                      *)
(*                                                                              *)
(*   4 /dev/ttySx                                                               *)
(*  43 /dev/ttyIx                                                               *)
(* 188 /dev/ttyUSBx                                                             *)
(* 204 /dev/ttyAMAx (undocumented Raspberry Pi serial console port) etc.        *)
(*                                                                              *)
(* The overall result will hopefully be "correct" both from the system and user *)
(* POV. See Documentation/devices.txt or Documentation/admin-guide/devices.txt  *)
(* in the Linux source tree.                                                    *)

  finally
    r.Free;
    minorList.Free;
    majorList.Free
  end;

(* Add extra storage to the cache so that as we're checking the ports we can    *)
(* accumulate driver names etc.                                                 *)

  for i := 0 to portsListCache.Count - 1 do begin
    portsListCache.Objects[i] := TStringList.Create;
    for j := 0 to cachedTop do
      TStringList(portsListCache.Objects[i]).Append('');
  end;
  result := TStringList.Create;
  result.Assign(portsListCache)
end { ListPorts } ;


(* Assuming that the cached list has been properly populated, dump its contents.
*)
procedure DumpCachedPorts;

var
  i: integer;

begin
  for i := 0 to portsListCache.Count - 1 do begin
    WriteLn(portsListCache[i]);
    if portsListCache.Objects[i] <> nil then begin
      Assert(portsListCache.Objects[i] is TStringList, 'Internal error: bad cached port list.');
      with TStringList(portsListCache.Objects[i]) do begin
        Assert(Count = cachedTop + 1, 'Internal error: bad cached port list.');
        WriteLn('  idVendor: ', Strings[cachedidVendor]);
        WriteLn('  idProduct: ', Strings[cachedidProduct]);
        WriteLn('  Driver: ', Strings[cachedDriver]);
        WriteLn('  Manufacturer: ', Strings[cachedManufacturer]);
        WriteLn('  Product: ', Strings[cachedProduct]);
        WriteLn('  Serial: ', Strings[cachedSerial])
      end
    end
  end;
  WriteLn
end { DumpCachedPorts } ;


var
  assumeBlankIsWildcard: boolean= false;


(* This represents a hack that I'm not entirely happy about. Having found a
  serial device name in the tree such as USB0 or ACM0, it appears to be
  necessary to step back by a varying amount (../.. or ../../.. in the case of
  those two examples) before starting to look for attributes such as idVendor.
  The layout is probably dependent on the device driver (kernel module), but
  it's not possible to identify this without knowing the tree layout; making
  things worse, it's not possible to simply use a deep search every time since
  that might pick up spurious USB hubs.
*)
function prefix(const device: string): string;

begin
  if Pos('ttyACM', device) > 0 then
    result := '../../../'
  else
    result := '../../'
end { prefix } ;


(* With the naming requirements met, make an additional final check that the
  serial device has a plausible vendor ID.
*)
function testidVendor(const dir: string; const name: string; const content: string;
                                                const extra: string; hint: integer= -1): boolean;

var
  scratch: string;

begin

(* The directory parameter will look something like /sys/devices/pci0000:00/0000:00:1d.0/usb2/2-2/2-2:1.0/ttyUSB2/tty *)
(* ...ttyUSB2/driver should be a symlink to /sys/bus/usb-serial/drivers/cp210x  *)
(* but might be distressingly relative.                                         *)

  scratch := dir + name + '/' + content;
  if not fileOrDirectoryExists(scratch) then
    scratch := ''
  else
    scratch := cat(scratch);
  if hint >= 0 then begin
    Assert(portsListCache.Objects[hint] is TStringList, 'Internal error: bad cached port list.');
    TStringList(portsListCache.Objects[hint])[cachedidVendor] := scratch
  end;
  result := extra = scratch
end { testidVendor } ;


(* Return true if the device named in the form /dev/ttyUSBn has a plausible
  vendor number.
*)
function usingidVendor(const device, idVendor: string; hint: integer= -1): boolean;

var
  name, path: string;

begin
  result := false;
  name := ExtractFilename(device);
  path := walkDirs('/sys/devices/', name, prefix(device) + 'idVendor', @testidVendor, idVendor, hint); (* Appends /  *)
  if not assumeBlankIsWildcard then
    result := path <> ''
  else begin
    result := (path <> '') or (idVendor = ''); (* Added for CDC_ACM device in Fenrir i/f *)
    if result and (path = '') then
      DebugWriteF('(Wildcarded, continuing)\n', [])
  end
end { usingidVendor } ;


(* With the naming requirements met, make an additional final check that the
  serial device has a plausible product ID.
*)
function testidProduct(const dir: string; const name: string; const content: string;
                                                const extra: string; hint: integer= -1): boolean;

var
  scratch: string;

begin

(* The directory parameter will look something like /sys/devices/pci0000:00/0000:00:1d.0/usb2/2-2/2-2:1.0/ttyUSB2/tty *)
(* ...ttyUSB2/driver should be a symlink to /sys/bus/usb-serial/drivers/cp210x  *)
(* but might be distressingly relative.                                         *)

  scratch := dir + name + '/' + content;
  if not fileOrDirectoryExists(scratch) then
    scratch := ''
  else
    scratch := cat(scratch);
  if hint >= 0 then begin
    Assert(portsListCache.Objects[hint] is TStringList, 'Internal error: bad cached port list.');
    TStringList(portsListCache.Objects[hint])[cachedidProduct] := scratch
  end;
  result := extra = scratch
end { testidProduct } ;


(* Return true if the device named in the form /dev/ttyUSBn has a plausible
  vendor number.
*)
function usingidProduct(const device, idProduct: string; hint: integer= -1): boolean;

var
  name, path: string;

begin
  result := false;
  name := ExtractFilename(device);
  path := walkDirs('/sys/devices/', name, prefix(device) + 'idProduct', @testidProduct, idProduct, hint); (* Appends /  *)
  if not assumeBlankIsWildcard then
    result := path <> ''
  else begin
    result := (path <> '') or (idProduct = ''); (* Added for CDC_ACM device in Fenrir i/f *)
    if result and (path = '') then
      DebugWriteF('(Wildcarded, continuing)\n', [])
  end
end { usingidProduct } ;


(* With the naming requirements met, make one final check that the serial
  device has a plausible description.
*)
function testDriver(const dir: string; const name: string; const content: string;
                                                const extra: string; hint: integer= -1): boolean;

var
  scratch: string;

begin

(* The directory parameter will look something like /sys/devices/pci0000:00/0000:00:1d.0/usb2/2-2/2-2:1.0/ttyUSB2/tty *)
(* ...ttyUSB2/driver should be a symlink to /sys/bus/usb-serial/drivers/cp210x  *)
(* but might be distressingly relative.                                         *)

  scratch := dir + name + '/' + content;
  scratch := fpReadLink(scratch);
  if hint >= 0 then begin
    Assert(portsListCache.Objects[hint] is TStringList, 'Internal error: bad cached port list.');
    TStringList(portsListCache.Objects[hint])[cachedDriver] := ExtractFileName(scratch)
  end;
  result := pos('/' + extra, scratch) > 0
end { testDriver } ;


(* Return true if the device named in the form /dev/ttyUSBn appears to be using
  a plausible device driver module.
*)
function usingDriver(const device, driver: string; hint: integer= -1): boolean;

var
  name, path: string;

begin
  result := false;
  name := ExtractFilename(device);
  path := walkDirs('/sys/devices/', name, 'driver', @testDriver, driver, hint); (* Appends /  *)
  if not assumeBlankIsWildcard then
    result := path <> ''
  else begin
    result := (path <> '') or (driver = ''); (* Added for CDC_ACM device in Fenrir i/f *)
    if result and (path = '') then
      DebugWriteF('(Wildcarded, continuing)\n', [])
  end
end { usingDriver } ;


(* With the naming requirements met, make one final check that the serial
  device has a plausible description.
*)
function testManufacturer(const dir: string; const name: string; const content: string;
                                                const extra: string; hint: integer= -1): boolean;

var
  scratch: string;

begin

(* The directory parameter will look something like /sys/devices/pci0000:00/0000:00:1d.0/usb2/2-2/2-2:1.0/ttyUSB2/tty *)
(* ...ttyUSB2/driver should be a symlink to /sys/bus/usb-serial/drivers/cp210x  *)
(* but might be distressingly relative.                                         *)

  scratch := dir + name + '/' + content;
  if not fileOrDirectoryExists(scratch) then
    scratch := ''
  else
    scratch := LowerCase(DelSpace1(Trim(cat(scratch)))); (* Squash repeated spaces etc. *)
  if hint >= 0 then begin
    Assert(portsListCache.Objects[hint] is TStringList, 'Internal error: bad cached port list.');
    TStringList(portsListCache.Objects[hint])[cachedManufacturer] := scratch
  end;
  result := LowerCase(DelSpace1(Trim(extra))) = scratch  (* Squash repeated spaces etc. *)
end { testManufacturer } ;


(* Return true if the device named in the form /dev/ttyUSBn has a plausible
  Manufacturer description.
*)
function usingManufacturer(const device, manufacturer: string; hint: integer= -1): boolean;

var
  name, path: string;

begin
  result := false;
  name := ExtractFilename(device);
  path := walkDirs('/sys/devices/', name, prefix(device) + 'manufacturer', @testManufacturer, manufacturer, hint); (* Appends /  *)
  if not assumeBlankIsWildcard then
    result := path <> ''
  else begin
    result := (path <> '') or (manufacturer = ''); (* Added for CDC_ACM device in Fenrir i/f *)
    if result and (path = '') then
      DebugWriteF('(Wildcarded, continuing)\n', [])
  end
end { usingManufacturer } ;


(* With the naming requirements met, make one final check that the serial
  device has a plausible description.
*)
function testProduct(const dir: string; const name: string; const content: string;
                                                const extra: string; hint: integer= -1): boolean;

var
  scratch: string;

begin

(* The directory parameter will look something like /sys/devices/pci0000:00/0000:00:1d.0/usb2/2-2/2-2:1.0/ttyUSB2/tty *)
(* ...ttyUSB2/driver should be a symlink to /sys/bus/usb-serial/drivers/cp210x  *)
(* but might be distressingly relative.                                         *)

  scratch := dir + name + '/' + content;
  if not fileOrDirectoryExists(scratch) then
    scratch := ''
  else
    scratch := LowerCase(DelSpace1(Trim(cat(scratch)))); (* Squash repeated spaces etc. *)
  if hint >= 0 then begin
    Assert(portsListCache.Objects[hint] is TStringList, 'Internal error: bad cached port list.');
    TStringList(portsListCache.Objects[hint])[cachedProduct] := scratch
  end;
  result := LowerCase(DelSpace1(Trim(extra))) = scratch  (* Squash repeated spaces etc. *)
end { testProduct } ;


(* Return true if the device named in the form /dev/ttyUSBn has a plausible
  product description.
*)
function usingProduct(const device, product: string; hint: integer= -1): boolean;

var
  name, path: string;

begin
  result := false;
  name := ExtractFilename(device);
  path := walkDirs('/sys/devices/', name, prefix(device) + 'product', @testProduct, product, hint); (* Appends /  *)
  if not assumeBlankIsWildcard then
    result := path <> ''
  else begin
    result := (path <> '') or (product = ''); (* Added for CDC_ACM device in Fenrir i/f *)
    if result and (path = '') then
      DebugWriteF('(Wildcarded, continuing)\n', [])
  end
end { usingProduct } ;


(* With the naming requirements met, make one final check that the serial
  device has a plausible description.
*)
function testSerial(const dir: string; const name: string; const content: string;
                                                const extra: string; hint: integer= -1): boolean;

var
  scratch: string;

begin

(* The directory parameter will look something like /sys/devices/pci0000:00/0000:00:1d.0/usb2/2-2/2-2:1.0/ttyUSB2/tty *)
(* ...ttyUSB2/driver should be a symlink to /sys/bus/usb-serial/drivers/cp210x  *)
(* but might be distressingly relative.                                         *)

  scratch := dir + name + '/' + content;
  if not fileOrDirectoryExists(scratch) then
    scratch := ''
  else
    scratch := cat(scratch);
  if hint >= 0 then begin
    Assert(portsListCache.Objects[hint] is TStringList, 'Internal error: bad cached port list.');
    TStringList(portsListCache.Objects[hint])[cachedSerial] := scratch
  end;
  result := extra = scratch
end { testSerial } ;


(* Return true if the device named in the form /dev/ttyUSBn has a plausible
  serial number.
*)
function usingSerial(const device, serial: string; hint: integer= -1): boolean;

var
  name, path: string;

begin
  result := false;
  name := ExtractFilename(device);
  path := walkDirs('/sys/devices/', name, prefix(device) + 'serial', @testSerial, serial, hint); (* Appends /  *)
  if not assumeBlankIsWildcard then
    result := path <> ''
  else begin
    result := (path <> '') or (serial = ''); (* Added for CDC_ACM device in Fenrir i/f *)
    if result and (path = '') then
      DebugWriteF('(Wildcarded, continuing)\n', [])
  end
end { usingSerial } ;


(* We know what kind of serial device is in the attached instrument etc., so
  look for that specific driver (Linux kernel module). The hint parameter, if
   >= 0, indicates a cache line that can be updated once we know a bit about a
  port (this was grafted on as an afterthought and could be done better).
*)
function selectAsDefault(const testPortName: string; const description: TPortDescription;
                                        hint: integer= -1): boolean;

begin
  result := true;

(* At least one of the tests is non-blank, so we can probably make some useful  *)
(* decisions. For example, we probably know something about the name of the     *)
(* port, e.g. it's going to be one of the /dev/ttyUSBn ports for a DSO112A so   *)
(* we don't have to waste time checking /dev/ttySn. If the port name is         *)
(* plausible then walk part of the /sys tree to try to find the driver name, in *)
(* the case of the DSO112A we know that this will be a Silicon Labs cp210x      *)
(* since it's internal to the device, and possibly continue by looking at the   *)
(* product description and serial number.                                       *)

(* This is the original implementation, it is fairly fast but since it doesn't  *)
(* investigate every port in depth it doesn't accumulate serial numbers etc.    *)

// It might be possible simply to assume that the usingXXX() functions "do the
// right thing" allowing us to dispense with assumeBlankIsWildcard

  if hint < 0 then begin
    Assert(description.baseName <> '', 'Internal error: basename blank.');
    DebugWriteF('Testing port %s against pattern %s\n', [testPortName, description.baseName]);
    if Pos(description.baseName, testPortName) = 0 then
      exit(false);
    assumeBlankIsWildcard := false; // TODO : Can we lose this?
    if description.idVendor <> '' then begin
      DebugWriteF('Testing against vendor ID %s\n', [description.idVendor]);
      if not usingidVendor(testPortName, description.idVendor, hint) then
        exit(false)
    end;
    if description.idProduct <> '' then begin
      DebugWriteF('Testing against product ID %s\n', [description.idProduct]);
      if not usingidProduct(testPortName, description.idProduct, hint) then
        exit(false)
    end;

    assumeBlankIsWildcard := true; // TODO : Can we lose this? // EXPERIMENTAL, MIGHT NOT WORK

    DebugWriteF('Testing against driver name %s\n', [description.driverName]);
    if not usingDriver(testPortName, description.driverName, hint) then
      exit(false);
    DebugWriteF('Testing against manufacturer description %s\n', [description.manufacturer]);
    if not usingManufacturer(testPortName, description.manufacturer, hint) then
      exit(false);
    DebugWriteF('Testing against product description %s\n', [description.product]);
    if not usingProduct(testPortName, description.product, hint) then
      exit(false);
    DebugWriteF('Testing against serial description %s\n', [description.serial]);
    if not usingSerial(testPortName, description.serial, hint) then
      exit(false)
  end else begin
    Assert(description.baseName <> '', 'Internal error: basename blank.');
    result := Pos(description.basename, testPortName) > 0;
    result := usingidVendor(testPortName, description.idVendor, hint) and result;
    result := usingidProduct(testPortName, description.idProduct, hint) and result;
    result := usingDriver(testPortName, description.driverName, hint) and result;
    result := usingManufacturer(testPortName, description.manufacturer, hint) and result;
    result := usingProduct(testPortName, description.product, hint) and result;
    result := usingSerial(testPortName, description.serial, hint) and result
  end;

(* If either there's no useful tests we can make or all tests have been         *)
(* successful, return true. The result of this will be that either the first    *)
(* device in the list or the last device that matches will have its radio       *)
(* button set.                                                                  *)

end { selectAsDefault } ;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  one with the described properties. Some types of serial device are used by
  more than one instrument (e.g. a CP210x may be found in both the Mastech
  2115B and the DSO112A) so it's not entirely foolproof... if it were a genuine
  FTDI chip it could be branded with the device serial number (or model number
  etc.) as a one-time operation which would make it more reliable.
*)
function FindPortByDescription(const description: TPortDescription;
                                        portScan: boolean= false): string;

var
  i: integer;
  portNames: TStringList;

begin
  result := '';

(* Initialise the menu from the known ports.                                    *)

  portNames := ListPorts();
  try
    for i := 0 to portNames.Count - 1 do begin

(* Let's assume that because we're adding devices in sequence, that the most    *)
(* recent that the OS has seen plugged in appears last. Select this as the      *)
(* device to be used if it matches certain critera.                             *)
(*                                                                              *)
(* If the portScan parameter is not set then we don't waste (lots of) time      *)
(* scanning device descriptions in the /sys tree.                               *)

      if not portScan then
        if selectAsDefault(portnames[i], description) then
          result := portnames[i]
        else begin end
      else
        if selectAsDefault(portnames[i], description, i) then
          result := portnames[i]
    end
  finally
    FreeAndNil(portNames)
  end
end { FindPortByDescription } ;


const
  invalidSocket: TSocket= -1;

var
  netLinkBuffer: array[0..65537] of ansichar;   (* 64K plus space for 2x \0     *)


(* Create a netlink socket for monitoring udev events. While the messages which
  describe these correspond to entries in the /sys tree, this is an underlying
  kernel mechanism rather than (as with inotify etc.) operating in the context
  of a specified filesystem.
*)
function prepareUdevSocket(): TSocket;

var
  nl: sockaddr_nl;
  flags: integer;

begin
  result := invalidSocket;
  FillByte(netLinkBuffer, SizeOf(netLinkBuffer), 0);
  try
    result := fpSocket(PF_NETLINK, SOCK_RAW, NETLINK_KOBJECT_UEVENT);
    if result >= 0 then begin
      flags := fpFcntl(result, F_GETFL, 0);
      fpFcntl(result, F_SETFL, flags or O_NONBLOCK);
      nl.nl_family := AF_NETLINK;
      nl.nl_pad := 0;
      nl.nl_pid := GetProcessID();
      nl.nl_groups := 1;
      if fpBind(result, @nl, sizeof(nl)) <> 0 then
        result := invalidSocket
    end
  except
    result := invalidSocket
  end
end { prepareUdevSocket } ;


(*  Return true if a device addition or removal has been detected.

  The hotplug (udev) events will be asynchronous to the actual addition or
  removal of entries in the /sys and /dev trees. As such a limited number are of
  actual use: a bind with the devicepath leading to vendor etc. information can
  be expected at about the same time this is stable in /sys etc., and a matching
  unbind shortly after the corresponding device has been removed.

  Note that the parameter's basename is the only field applicable to a device
  removal, and that the other parameters cannot be checked at removal.
*)
function PollHotplugEvents(const description: TPortDescription): boolean;

const
//  udevSocket: TSocket= invalidSocket;   (* Static variable                      *)
  udevSocket: TSocket= -1;              (* Static variable                      *)

var
  unparsed, verb, noun, scratch: string;
  parsed: TStringList;
  k: integer;
  debug: boolean;

begin
  result := false;
  if udevSocket = invalidSocket then
    udevSocket := prepareUdevSocket();
  if udevSocket <> invalidSocket then

(* Receive udev messages indicating hotplug etc. events. In principle these can *)
(* be up to 64K, note two extra bytes left unused in the zeroed buffer to make  *)
(* sure we have \0\0 as an unambiguous terminator.                              *)

    while fpRecv(udevSocket, @netlinkBuffer, SizeOf(netlinkBuffer) - 2, 0) > 0 do begin
      parsed := TStringList.Create;
      try

// https://stackoverflow.com/questions/22803469/uevent-sent-from-kernel-to-user-space-udev
//
// Expect messages to be something like (output from debugging lines below):
//
// --------------------------------------------------------------------------------
// unparsed: bind@/devices/pci0000:00/0000:00:1a.0/usb1/1-1/1-1.3/1-1.3.1
// ACTION=bind
// DEVPATH=/devices/pci0000:00/0000:00:1a.0/usb1/1-1/1-1.3/1-1.3.1
// SUBSYSTEM=usb
// MAJOR=189
// MINOR=63
// DEVNAME=bus/usb/001/064
// DEVTYPE=usb_device
// DRIVER=usb
// PRODUCT=1a86/7523/264
// TYPE=255/0/0
// BUSNUM=001
// DEVNUM=064
// SEQNUM=3283
// udev bind "/devices/pci0000:00/0000:00:1a.0/usb1/1-1/1-1.3/1-1.3.1"
// --------------------------------------------------------------------------------
// unparsed: remove@/devices/pci0000:00/0000:00:1a.0/usb1/1-1/1-1.3/1-1.3.1
// ACTION=remove
// DEVPATH=/devices/pci0000:00/0000:00:1a.0/usb1/1-1/1-1.3/1-1.3.1
// SUBSYSTEM=usb
// MAJOR=189
// MINOR=63
// DEVNAME=bus/usb/001/064
// DEVTYPE=usb_device
// PRODUCT=1a86/7523/264
// TYPE=255/0/0
// BUSNUM=001
// DEVNUM=064
// SEQNUM=3290
// udev remove "/devices/pci0000:00/0000:00:1a.0/usb1/1-1/1-1.3/1-1.3.1"
//
// First skim off the unparsed entire message, then take the path of least
// resistance and move the parsed part of the message forward in the buffer and
// save each component (which makes for the easiest debugging) noting that we
// are making sure to preserve the final repeated \0\0.

        unparsed := StrPas(@netLinkBuffer);
        k := SizeOf(netlinkBuffer) - 1; (* Look for final \0 in buffer          *)
        while (k > 0) and (netlinkBuffer[k - 1] = #0) do
          k -= 1;
        Move(netlinkBuffer[Length(unparsed) + 1], netlinkBuffer[0],
                                                        (k - Length(unparsed) + 1));
        while netlinkBuffer[0] <> #0 do begin
          scratch := StrPas(@netLinkBuffer);
          parsed.Add(scratch);
          Move(netlinkBuffer[Length(scratch) + 1], netlinkBuffer[0],
                                                        (k - Length(scratch) + 1))
        end;
        verb := parsed.Values['ACTION'];
        noun := parsed.Values['PRODUCT'];
        if Pos('_device', parsed.Values['DEVTYPE']) > 0 then begin

(* Assume that we want debug output for all bind/unbind events at the device    *)
(* level, irrespective of whether the description matches.                      *)

// TODO : Even minimal device description matching.

          case verb of
            'bind':   begin
                        debug := true;
                        result := true; // Provided vid:pid:bus match
                      end;
            'unbind': begin
                        debug := true;
                        result := true  // Provided basename no longer present
                      end
          otherwise
            debug := false;
            result := false
          end;
          if debug then begin
{             DebugWriteF('--------------------------------------------------------------------------------\n', []);
            DebugWriteF('unparsed: %s\n', [unparsed]);
            for k := 0 to parsed.Count - 1 do
              DebugWriteF('%s\n', [parsed[k]]); }
            DebugWriteF('udev %s %s\n', [verb, noun])
          end
        end
      finally
        FreeAndNil(parsed)
      end;
      FillByte(netLinkBuffer, SizeOf(netLinkBuffer), 0)
    end
end { PollHotplugEvents } ;


(*  Return true if a device addition or removal has been detected.

  The hotplug (udev) events will be asynchronous to the actual addition or
  removal of entries in the /sys and /dev trees. As such a limited number are of
  actual use: a bind with the devicepath leading to vendor etc. information can
  be expected at about the same time this is stable in /sys etc., and a matching
  unbind shortly after the corresponding device has been removed.

  It would be reasonable to test a port description against /sys at (or at least
  shortly after) the bind event, however by the time that the unbind is parsed
  the device will probably have been removed from the /sys and /dev trees. As
  such, for the moment at least, it's probably wisest to use this simply as a
  "something's changed" notification that prompts the main program to redo its
  scan for device types (e.g. serial interfaces) that interest it.
*)
function PollHotplugEvents(): boolean;

const
  anything: TPortDescription= (
            );

begin
  result := PollHotplugEvents(anything)
end { PollHotplugEvents } ;


(* Return the number of bytes buffered for reception by the operating system,
  or a -ve result on error. Since ioctl() returns an integer of indeterminate
  length via a pointer this has provision for investigating its actual size by
  presetting the return value to a non-zero bit pattern.
*)
function SerAvailable(handle: TSerialHandle; defaultResult: longint= 0): longint;

begin
  if handle < 0 then
    result := -1
  else begin
    result := defaultResult;            (* So we can see what bits get changed  *)

// WATCH IT: there might be a limit of e.g. 4K (0..4096) on this.

    if fpIoctl(handle, FIONREAD, @result) <> 0 then
      result := -1
  end
end { SerAvailable } ;


(* Return the number of bytes buffered for reception by the operating system,
  or a -ve result on error. Since ioctl() returns an integer of indeterminate
  length via a pointer this has provision for investigating its actual size by
  presetting the return value to a non-zero bit pattern.
*)
function serAvailable64(handle: TSerialHandle; defaultResult: int64= 0): int64;

begin
  if handle < 0 then
    result := -1
  else begin
    result := defaultResult;            (* So we can see what bits get changed  *)

// WATCH IT: there might be a limit of e.g. 4K (0..4096) on this.

    if fpIoctl(handle, FIONREAD, @result) <> 0 then
      result := -1
  end
end { serAvailable64 } ;


(* Like SerOpen(), but with a mandatory lock at the operating system level and
  explicitly setting a default line speed etc. with terminal modes zeroed.
*)
function SerOpenLocked(const deviceName: string): TSerialHandle;

var
  avail: longint;
  avail64: int64;

begin
  result := SerOpen(deviceName);
  {$ifdef UNIX }
  if result > 0 then begin
    if fpIoctl(result, TIOCEXCL, nil) <> 0 then begin   (* Mandatory lock,      *)
      SerClose(result);                 (* unlike flock() (if it even works in  *)
      result := InvalidSerialHandle     (* this context) or a lock file as used *)
    end;                                (* by various gettys etc.               *)
{$ifopt C+ Predicated on assertions generating code }
    avail := SerAvailable(result, $7fffffff);
    avail64 := SerAvailable64(result, $7ffffffffffffff);
    Assert(avail and $40000000 = 0, 'Internal error: SerAvailable() does not return 32 bits.');
    Assert(avail64 and $0000000100000000 <> 0, 'Internal error: SerAvailable() returns >32 bits.');
{$endif }
  end;
  {$endif UNIX }

(* These might not be the defaults we want, but if we don't do this then there  *)
(* is a real risk that e.g. ICRNL will be set which will badly mess up input CR *)
(* handling. As of 2021 there is a problem in SerSetParams() in that it doesn't *)
(* start off with a tcgetattr(), but fortuitously the all-reset state is about  *)
(* what we normally want.                                                       *)

  SerSetParams(result, 9600, 8, NoneParity, 1, [])
end { SerOpenLocked } ;


(* This is lifted from the DT800 MicroKernel. The parameter is set to represent
  the current time, for use only by Elapsed().
*)
procedure InitTimer(out td: TimerDescriptor);

begin
  td := GetTickCount64()                (* mSec counter with undefined granularity *)
end { InitTimer } ;


(* Return true if the interval (measured in ticks) has expired.
*)
function Elapsed(const td: TimerDescriptor; interval: int64): boolean;

begin
  if interval < 0 then
    result := true
  else begin
    interval := interval * (1000 div TimerResolution); (* Scale to mSec         *)
{$push }{$Q- }
    result := interval <= GetTickCount64() - td
{$pop        }
  end
end { Elapsed } ;


end.

