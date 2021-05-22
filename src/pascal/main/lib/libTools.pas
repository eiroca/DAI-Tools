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
    Name: string;
    size: uint32;
    addr: uint16;
    entrypoint: uint16;
    segType: uint16;
    Data: array of byte;
    info: string;
  end;

procedure Segment_init(var seg: RSegment; const aSize: integer = (MAX_ADDR + 1));
procedure Segment_resize(var seg: RSegment; const aSize: integer);
procedure Segment_slice(var seg: RSegment; const minAddr, maxAddr: integer);
procedure Segment_writeMetadata(var seg: RSegment; const path: string);
procedure Segment_split(var seg: RSegment; var odd, even: RSegment);
function Segment_text(var seg: RSegment; Lines: TStringList; const headerFmt, prefixFmt, byteFmt, separator: string): boolean;

function Dump_detect(const Lines: TStringList): EDumpType;
function Dump_decode(const Lines: TStringList; var seg: RSegment): boolean;

implementation

uses
  StrUtils, Math, jsonConf;

const
  MAX_LOOK_HEAD = 10;
  DUMP_SIGN: array [EDumpType] of integer = (0, %11110111111111, %1111011011111111, %111101011011111111);
  DUMP_MASK: array [EDumpType] of integer = (0, %11111111111111, %1111111111111111, %111111111111111111);
  DUMP_ODAT: array [EDumpType] of integer = (0, 2, 3, 4);
  DUMP_MLEN: array [EDumpType] of integer = (0, $FFFF, $FFFF, 16);

const
  N_LOADADDR: UnicodeString = '/LoadAddress';
  N_ENTRYPOINT: UnicodeString = '/EntryPoint';
  N_TYPE: UnicodeString = '/Type';
  N_LENGTH: UnicodeString = '/Length';
  N_INFO: UnicodeString = '/Info';

procedure Segment_init(var seg: RSegment; const aSize: integer = (MAX_ADDR + 1));
begin
  with seg do begin
    addr := 0;
    size := aSize;
    entrypoint := 0;
    segType := 0;
    info := '';
    SetLength(Data, size);
  end;
end;

procedure Segment_resize(var seg: RSegment; const aSize: integer);
begin
  if (seg.size <> aSize) then begin
    with seg do begin
      size := aSize;
      SetLength(Data, size);
    end;
  end;
end;

procedure Segment_split(var seg: RSegment; var odd, even: RSegment);
var
  adr, len, pos: integer;
begin
  adr := seg.addr;
  len := seg.size + 1;
  if not odd(adr) then begin
    adr := adr + 1;
    len := len - 1;
  end;
  len := len div 2;
  odd.addr := adr;
  Segment_resize(odd, len);
  i := adr;
  pos := 0;
  pos:=0;
  while (pos<len) do begin
    odd.Data[pos] := seg[adr];
    Inc(pos);
    Inc(adr, 2);
  end;

end;

function Segment_text(var seg: RSegment; Lines: TStringList; const headerFmt, prefixFmt, byteFmt, separator: string): boolean;
var
  endAddr: integer;
  i, sAdr, len, cPos: integer;
  s: string;
begin
  Result := True;
  endAddr := seg.addr + seg.size - 1;
  sAdr := seg.addr;
  cPos := 0;
  Lines.Add(Format(headerFmt, [seg.addr, endAddr]));
  while (sAdr < endAddr) do begin
    s := Format(prefixFmt, [sAdr]);
    len := $10 - (sAdr and $000F);
    Inc(sAdr, len);
    if (sAdr > endAddr) then begin
      len := len - (sAdr - endAddr) + 1;
    end;
    for i := 0 to len - 1 do begin
      if (i > 0) then begin
        s := s + separator;
      end;
      s := s + Format(byteFmt, [seg.Data[cPos]]);
      Inc(cPos);
    end;
    Lines.Add(s);
  end;
end;

procedure Segment_slice(var seg: RSegment; const minAddr, maxAddr: integer);
begin
  seg.addr := minAddr;
  seg.size := maxAddr - minAddr + 1;
  seg.Data := Copy(seg.Data, seg.addr, seg.size);
end;

procedure Segment_writeMetadata(var seg: RSegment; const path: string);
var
  conf: TJsonConfig;
  aName: UnicodeString;
begin
  conf := TJSONConfig.Create(nil);
  try
    conf.Filename := path;
    conf.Formatted := True;
    aName := seg.Name;
    if (aName = '') then begin
      aName := 'segment';
    end;
    conf.SetValue(aName + N_LOADADDR, seg.addr);
    conf.SetValue(aName + N_LENGTH, seg.size);
    conf.SetDeleteValue(aName + N_ENTRYPOINT, seg.entrypoint, 0);
    conf.SetDeleteValue(aName + N_TYPE, seg.segType, 0);
    conf.SetDeleteValue(aName + N_INFO, seg.info, '');
  finally
    conf.Free;
  end;
end;

function _readBound(conf: TJsonConfig; Name: UnicodeString; def: integer; min: integer; max: integer): integer;
begin
  Result := conf.GetValue(Name, def);
  if (Result < min) then begin
    Result := min;
  end
  else if (Result > max) then begin
    Result := max;
  end;
end;

procedure Segment_loadMetadata(var seg: RSegment; const path: string);
var
  conf: TJsonConfig;
  aName: UnicodeString;
begin
  conf := TJSONConfig.Create(nil);
  try
    conf.Filename := path;
    conf.Formatted := True;
    aName := seg.Name;
    if (aName = '') then begin
      aName := 'segment';
    end;
    with seg do begin
      addr := _readBound(conf, aName + N_LOADADDR, addr, 0, MAX_ADDR);
      entrypoint := _readBound(conf, aName + N_ENTRYPOINT, entrypoint, 0, MAX_ADDR);
      segType := conf.GetValue(aName + N_TYPE, segType);
      size := _readBound(conf, aName + N_LENGTH, size, 0, MAX_ADDR + 1);
      SetLength(Data, size);
    end;
  finally
    conf.Free;
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
    Segment_slice(seg, minAddr, maxAddr);
    Result := False;
  except
    on E: EConvertError do begin
      SetLength(seg.Data, 0);
      seg.size := 0;
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
