(* Copyright (C) 2020-2021 Enrico Croce - AGPL >= 3.0
*
* This program is free software: you can redistribute it and/or modify it under the terms of the
* GNU Affero General Public License as published by the Free Software Foundation, either version 3
* of the License, or (at your option) any later version.
*
* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
* even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Affero General Public License for more details.
*
* You should have received a copy of the GNU Affero General Public License along with this program.
* If not, see <http://www.gnu.org/licenses/>.
*
*)
unit libTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

const
  MAX_ADDR = 64 * 1024 - 1;

const
  HEXNUM: TSysCharSet = ['0'..'9', 'A'..'F'];

type
  EDumpType = (
    dUnknown,
    dDAI, // Example line: 'BFF0 00 00 B8 36 00 00 AF 36 00 00 9F 36 00 00 80 36'
    dApple2, // Example line: 'BFF0 00: 00 B8 36 00 00 AF 36 00 00 9F 36 00 00 80 36'
    dMAME// Example line: 'BFF0 00:  00 B8 36 00 00 AF 36 00 00 9F 36 00 00 80 36 ................'
    );

  RSegment = record
    addr: uint16;
    len: uint16;
    entrypoint: uint16;
    segType: uint16;
    Data: array of byte;
  end;

procedure Segment_init(var seg: RSegment; const size: integer = (MAX_ADDR + 1));

function Dump_detect(const Lines: TStringList): EDumpType;
function Dump_decode(const Lines: TStringList; var seg: RSegment): boolean;

implementation

uses
  StrUtils, Math;

const
  MAX_LOOK_HEAD = 10;
  DUMP_NAME: array [EDumpType] of string = ('Invalid', 'DAI computer', 'Apple 2', 'MAME');
  DUMP_SIGN: array [EDumpType] of integer = (0, %11110111111111, %1111011011111111, %111101011011111111);
  DUMP_MASK: array [EDumpType] of integer = (0, %11111111111111, %1111111111111111, %111111111111111111);
  DUMP_ODAT: array [EDumpType] of integer = (0, 2, 3, 4);
  DUMP_MLEN: array [EDumpType] of integer = (0, $FFFF, $FFFF, 16);

procedure Segment_init(var seg: RSegment; const size: integer);
begin
  with seg do begin
    addr := 0;
    len := size;
    entrypoint := 0;
    segType := 0;
    SetLength(Data, size);
  end;
end;

// Returns True if `s` looks like a valid UT hex dump line.
function _isValidLine(const s: string): boolean;
var
  i: integer;
  c: char;
begin
  Result := False;
  if (Length(s) < 7) then begin
    exit;
  end;
  for i := 1 to 7 do begin
    c := s[i];
    if i = 5 then begin
      if (c <> ' ') then begin
        exit;
      end;
    end
    else if not CharInSet(c, HEXNUM) then begin
      exit;
    end;
  end;
  Result := True;
end;

function _sign_is(const sign: integer; t: EDumpType): boolean; inline;
begin
  if (sign and DUMP_MASK[t]) = DUMP_SIGN[t] then begin
    Result := True;
  end
  else begin
    Result := False;
  end;
end;

function _dump_signature(const s: string): integer;
var
  i, ln: integer;
  v: integer;
begin
  Result := 0;
  ln := Min(9, length(s));
  for i := 0 to ln - 1 do begin
    case (s[i + 1]) of
      '0'..'9', 'A'..'Z': begin
        v := 3;
      end;
      ':': begin
        v := 2;
      end;
      ' ': begin
        v := 1;
      end;
      else begin
        v := 0;
      end;
    end;
    Result := Result or (v shl (i * 2));
  end;
end;

function Dump_detect(const Lines: TStringList): EDumpType;
var
  i, n: integer;
  sign: integer;
  s: string;
  t: EDumpType;
begin
  Result := dUnknown;
  n := min(MAX_LOOK_HEAD, Lines.Count);
  for i := 0 to n - 1 do begin
    s := Lines[i];
    sign := _dump_signature(s);
    for t in EDumpType do begin
      if t = dUnknown then begin
        continue;
      end;
      if _sign_is(sign, t) then begin
        Result := t;
        exit;
      end;
    end;
  end;
end;

function _dump_decode(const Lines: TStringList; var seg: RSegment; dt: EDumpType): boolean;
var
  s: string;
  b, pos, len: integer;
  numByte, addr: integer;
  minAddr, maxAddr: integer;
  maxLen: integer;
  sign: integer;
begin
  minAddr := MaxInt;
  maxAddr := -1;
  try
    maxLen := DUMP_MLEN[dt];
    for s in Lines do begin
      sign := _dump_signature(s);
      if not _sign_is(sign, dt) then begin
        continue;
      end;
      len := Length(s);
      numByte := 0;
      addr := Hex2Dec(Copy(s, 1, 4));
      if (addr < minAddr) then begin
        minAddr := addr;
      end;
      if (dt = dMAME) then begin
        maxLen := 16 - (addr and $F); // Avoid to read ASCII if present
      end;
      pos := 4 + DUMP_ODAT[dt];
      while True do begin
        b := Hex2Dec(copy(s, pos, 2));
        Inc(numByte);
        seg.Data[addr] := b;
        Inc(pos, 3);
        if (addr = MAX_ADDR) then begin
          break;
        end;
        if (numByte > maxLen) then begin
          break;
        end;
        if (len - pos + 1) < 2 then begin
          break;
        end;
        Inc(addr);
      end;
      if (addr > maxAddr) then begin
        maxAddr := addr;
      end;
    end;
    seg.addr := minAddr;
    seg.len := maxAddr - minAddr + 1;
    seg.Data := Copy(seg.Data, seg.addr, seg.len);
    Result := False;
  except
    on E: EConvertError do begin
      SetLength(seg.Data, 0);
      seg.len := 0;
    end;
  end;
end;

function Dump_decode(const Lines: TStringList; var seg: RSegment): boolean;
var
  dt: EDumpType;
begin
  Result := False;
  Segment_init(seg);
  dt := Dump_detect(Lines);
  if dt <> dUnknown then begin
    Result := _dump_decode(Lines, seg, dt);
  end;
end;

end.
