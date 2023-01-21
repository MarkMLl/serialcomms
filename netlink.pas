(* FPC 3.0.2 Lazarus 1.6.4 running on Debian on Raspberry Pi. FPC 3.0.2 Lazarus *)

unit netlink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets;

const
  NETLINK_KOBJECT_UEVENT= 15;           (* Need to move this to RTL             *)

type
  sockaddr_nl = record                  (* Need to move this to RTL             *)
      nl_family: sa_family_t;
      nl_pad: word;
      nl_pid: dword;
      nl_groups: dword;
    end;


implementation

end.

