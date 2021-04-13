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
unit libDAI;

{$mode objfpc}{$H+}

interface

uses
  libTools, libDAIGraph,
  Classes, SysUtils, Graphics;

type
  DAI_func = function(const outPath: string; var seg: RSegment): boolean;

function DAI_lastError: string;

function DAI_initFont(const path: string): boolean;

function DAI_loadBin(const outPath: string; var seg: RSegment): boolean;
function DAI_loadSBin(const outPath: string; var seg: RSegment): boolean;
function DAI_loadDump(const inPath: string; var seg: RSegment): boolean;

function DAI_saveBin(const outPath: string; var seg: RSegment): boolean;
function DAI_saveSBin(const outPath: string; var seg: RSegment): boolean;
function DAI_savePNG(const outPath: string; var seg: RSegment): boolean;
function DAI_saveFullPNG(const outPath: string; var seg: RSegment): boolean;
function DAI_saveDump(const outPath: string; var seg: RSegment): boolean;

function DAI_saveDAIbin(const outPath: string; var seg: RSegment): boolean;
function DAI_saveWAV(const outPath: string; var seg: RSegment): boolean;

implementation

threadvar
  lastError: string;

function _saveSegData(fs: TFileStream; var seg: RSegment): boolean;
var
  i: integer;
begin
  Result := False;
  try
    try
      for i := 0 to seg.len - 1 do begin
        fs.WriteByte(seg.Data[i]);
      end;
      Result := True;
    except
      on E: Exception do lastError := 'Expception: ' + E.Message;
    end;
  finally
    fs.Free;
  end;
end;

function _loadSegData(fs: TFileStream; sz: integer; var seg: RSegment): boolean;
var
  i: integer;
begin
  Result := False;
  SetLength(seg.Data, sz);
  seg.len := sz;
  try
    try
      for i := 0 to sz - 1 do begin
        seg.Data[i] := fs.ReadByte();
      end;
      Result := True;
    except
      on E: Exception do lastError := 'Expception: ' + E.Message;
    end;
  finally
    fs.Free;
  end;
end;

function DAI_initFont(const path: string): boolean;
var
  fs: TFileStream;
  i: integer;
begin
  Result := False;
  lastError := 'Unable to read font ' + path;
  fs := TFileStream.Create(Path, fmOpenRead);
  try
    try
      if (fs.Size <> FONTCHAR_SIZE) then begin
        exit;
      end;
      for i := 0 to FONTCHAR_SIZE - 1 do begin
        FONT[i] := fs.ReadByte;
      end;
      Result := True;
      lastError := '';
    except
      on E: Exception do lastError := 'Expception: ' + E.Message;
    end;
  finally
    fs.Free;
  end;
end;

function DAI_loadDump(const inPath: string; var seg: RSegment): boolean;
var
  Lines: TStringList;
begin
  Result := False;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(inPath);
    Dump_decode(Lines, seg);
    Result := True;
  finally
    Lines.Free;
  end;
end;

function DAI_saveDump(const outPath: string; var seg: RSegment): boolean;
var
  Lines: TStringList;
  endAddr: integer;
  i, sAdr, len, cPos: integer;
  s: string;
begin
  if (seg.len = 0) then begin
    Result := False;
    lastError := 'Invalid segment';
    exit;
  end;
  Result := True;
  endAddr := seg.addr + seg.len - 1;
  try
    Lines := TStringList.Create;
    Lines.Add(Format('>D%4x %x', [seg.addr, endAddr]));
    if (seg.entrypoint <> 0) then begin
      Lines.Add(Format('>Run Address = %4x', [seg.entrypoint]));
    end;
    sAdr := seg.addr;
    cPos := 0;
    while (sAdr < endAddr) do begin
      s := IntToHex(sAdr, 4);
      len := $10 - (sAdr and $000F);
      Inc(sAdr, len);
      if (sAdr > endAddr) then begin
        len := len - (sAdr - endAddr) + 1;
      end;
      for i := 0 to len - 1 do begin
        s := s + ' ' + IntToHex(seg.Data[cPos], 2);
        Inc(cPos);
      end;
      Lines.Add(s);
    end;
    try
      Lines.SaveToFile(outPath);
    except
      on E: Exception do begin
        Result := False;
        lastError := 'Expception: ' + E.Message;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function DAI_saveBin(const outPath: string; var seg: RSegment): boolean;
var
  fs: TFileStream;
begin
  Result := False;
  lastError := 'Unable to write ' + outPath;
  if (seg.len > 0) then begin
    fs := TFileStream.Create(outPath, fmCreate);
    if not _saveSegData(fs, seg) then begin
      exit;
    end;
    Result := True;
    lastError := '';
  end
  else begin
    lastError := 'Invalid segment';
  end;
end;

function DAI_saveSBin(const outPath: string; var seg: RSegment): boolean;
var
  fs: TFileStream;
begin
  Result := False;
  lastError := 'Unable to write ' + outPath;
  if (seg.len > 0) then begin
    fs := TFileStream.Create(outPath, fmCreate);
    try
      fs.WriteWord(seg.addr);
    except
      on E: Exception do begin
        lastError := 'Expception: ' + E.Message;
        fs.Free;
        exit;
      end;
    end;
    if not _saveSegData(fs, seg) then begin
      exit;
    end;
    Result := True;
    lastError := '';
  end
  else begin
    lastError := 'Invalid segment';
  end;
end;

function DAI_loadBin(const outPath: string; var seg: RSegment): boolean;
var
  fs: TFileStream;
  sz: integer;
begin
  Result := False;
  lastError := 'Unable to read ' + outPath;
  fs := TFileStream.Create(outPath, fmOpenRead);
  sz := fs.Size;
  if not _loadSegData(fs, sz, seg) then begin
    exit;
  end;
  Result := True;
  lastError := '';
end;

function DAI_loadSBin(const outPath: string; var seg: RSegment): boolean;
var
  fs: TFileStream;
  sz: integer;
begin
  Result := False;
  lastError := 'Unable to read ' + outPath;
  fs := TFileStream.Create(outPath, fmOpenRead);
  try
    sz := fs.Size - 2;
    seg.addr := fs.ReadWord;
  except
    on E: Exception do begin
      lastError := 'Expception: ' + E.Message;
      fs.Free;
      exit;
    end;
  end;
  if not _loadSegData(fs, sz, seg) then begin
    exit;
  end;
  Result := True;
  lastError := '';
end;

function DAI_lastError: string;
begin
  Result := lastError;
end;

function DAI_savePNG(const outPath: string; var seg: RSegment): boolean;
var
  curAddr: integer;
  B: TPortableNetworkGraphic;
  C: TCanvas;
  fbi: RFrameBufferInfo;
begin
  Result := False;
  lastError := 'Unable to write ' + outPath;
  if (seg.len < 4) then begin
    lastError := 'Invalid segment';
    exit;
  end;
  curAddr := -1;
  if (curAddr = -1) then begin
    curAddr := seg.len - 1;
  end;
  fbi.numCW:=0;
  DAI_infoFrameBuffer(seg, curAddr, fbi);
  seg.entrypoint := curAddr - fbi.sizeVis+1;
  B := TPortableNetworkGraphic.Create;
  B.SetSize(DAI_SCREEN_WIDTH, DAI_SCREEN_LINES);
  C := B.Canvas;
  if DAI_decodeFrameBuffer(seg, curAddr, C) then begin
    try
      B.SaveToFile(outPath);
      Result := True;
      lastError := '';
    except
      on E: Exception do begin
        lastError := 'Expception: ' + E.Message;
        exit;
      end;
    end;
  end
  else begin
    lastError := 'Invalid Frame Buffer';
  end;
end;

function DAI_saveFullPNG(const outPath: string; var seg: RSegment): boolean;
var
  curAddr: integer;
  B: TPortableNetworkGraphic;
  C: TCanvas;
  fbi: RFrameBufferInfo;
begin
  Result := False;
  lastError := 'Unable to write ' + outPath;
  if (seg.len < 4) then begin
    lastError := 'Invalid segment';
    exit;
  end;
  curAddr := -1;
  if (curAddr = -1) then begin
    curAddr := seg.len - 1;
  end;
  fbi.numCW:=0;
  DAI_infoFrameBuffer(seg, curAddr, fbi);
  seg.entrypoint := curAddr - fbi.sizeVis+1;
  B := TPortableNetworkGraphic.Create;
  B.SetSize(DAI_IMAGE_WIDTH, DAI_IMAGE_LINES);
  C := B.Canvas;
  if DAI_decodeFullFrameBuffer(seg, curAddr, C) then begin
    try
      B.SaveToFile(outPath);
      Result := True;
      lastError := '';
    except
      on E: Exception do begin
        lastError := 'Expception: ' + E.Message;
        exit;
      end;
    end;
  end
  else begin
    lastError := 'Invalid Frame Buffer';
  end;
end;

// Calculate the checksum following the DAI protocol
function Checksum(cs: integer; n: integer): integer;
var
  ret: integer;
begin
  ret := 0;
  cs := cs xor n;
  if cs >= 128 then begin
    ret := 1;
  end;
  cs := (cs shl 1) and $FF or ret;
  Result := cs;
end;

function DAI_saveDAIbin(const outPath: string; var seg: RSegment): boolean;
var
  FNLength: integer;
  fs: TFileStream;
  c, cs: integer;
  b: byte;
  car: char;
  addrL, addrH: integer;
  sizeL, sizeH: integer;
begin
  Result := False;
  fs := TFileStream.Create(outPath, fmCreate);
  try
    try
      // File Type = 49 (31H for Binary file to load in UT with "R" command)
      fs.WriteByte($31);
      // Name Length
      FNLength := Length(seg.Name);
      cs := 86;
      fs.writeByte(0);
      cs := Checksum(cs, 0);
      fs.writeByte(FNLength);
      cs := Checksum(cs, FNLength);
      fs.writeByte(cs);
      // Name
      cs := 86;
      for car in seg.Name do begin
        c := Ord(car);
        fs.writeByte(c);
        cs := Checksum(cs, c);
      end;
      fs.writeByte(cs);
      // Length of Address
      fs.writeByte(0);
      fs.writeByte(2);
      fs.writeByte(93);
      // Start Address
      cs := 86;
      addrL := seg.addr and $FF;
      addrH := (seg.addr shr 8) and $FF;
      fs.writeByte(addrL);
      cs := Checksum(cs, addrL);
      fs.writeByte(addrH);
      cs := Checksum(cs, addrH);
      fs.writeByte(cs);
      // Length of Content
      sizeL := seg.len and 255;
      sizeH := (seg.len shr 8) and 255;
      cs := 86;
      fs.writeByte(sizeH);
      cs := Checksum(cs, sizeH);
      fs.writeByte(sizeL);
      cs := Checksum(cs, sizeL);
      fs.writeByte(cs);
      // Content
      cs := 86;
      for b in seg.Data do begin
        fs.writeByte(b);
        cs := Checksum(cs, b);
      end;
      fs.writeByte(cs);
      Result := True;
      lastError := '';
    except
      on E: Exception do begin
        lastError := 'Expception: ' + E.Message;
        exit;
      end;
    end;
  finally
    fs.Free;
  end;
end;

procedure _writeSeg(fs: TFileStream; c: integer; b: array of byte);
var
  i, j: integer;
begin
  for i := 1 to c do begin
    for j := low(b) to high(b) do begin
      fs.WriteByte(b[j]);
    end;
  end;
end;

(* Write a BIT
 P (Pre-Leader): 1 x 00-80
 L (Leader): 7 x FF-7F / 7 x 00-80 / 7 x FF-7F / 7 x 00-80
 1 (bit 1): 22 x FF-7F / 22 x 00-80 / 14 x FF-7F / 14 x 00-80
 0 (bit 0): 14 x FF-7F / 14 x 00-80 / 22 x FF-7F / 22 x 00-80
 T (Trailer): 10 x FF-7F / 10 x 00-80 / 14 x FF-7F / 14 x 00-80
*)
procedure _writeWAVbit(fs: TFileStream; type_bit: char; var fileSize: integer);
var
  nbre_a, nbre_b: integer;
begin
  nbre_a := 0;
  nbre_b := 0;
  if type_bit = '1' then begin
    nbre_a := 11;
    nbre_b := 7;
  end
  else if type_bit = '0' then begin
    nbre_a := 7;
    nbre_b := 11;
  end
  else if type_bit = 'L' then begin //L For the ladder
    nbre_a := 7;
    nbre_b := 7;
  end
  else if type_bit = 'T' then begin //T For the Trailer
    nbre_a := 5;
    nbre_b := 7;
  end;
  if type_bit <> 'P' then begin
    _writeSeg(fs, nbre_a, [255, 127]);
    Inc(filesize, 2 * nbre_a);
    _writeSeg(fs, nbre_a, [0, 128]);
    Inc(filesize, 2 * nbre_a);
    _writeSeg(fs, nbre_b, [255, 127]);
    Inc(filesize, 2 * nbre_b);
    _writeSeg(fs, nbre_b, [0, 128]);
    Inc(filesize, 2 * nbre_b);
  end
  else begin // For the PreLeader
    _writeSeg(fs, 1, [0, 128]);
    Inc(filesize, 2);
  end;
end;

// Decompose a Byte in Bits String and write them
procedure _writeWAVbyte(fs: TFileStream; b: byte; var fileSize: integer);
var
  i: integer;
begin
  for i := 0 to 7 do begin
    if b and (1 shl (7 - i)) <> 0 then begin
      _writeWAVbit(fs, '1', fileSize);
    end
    else begin
      _writeWAVbit(fs, '0', fileSize);
    end;
  end;
end;

procedure _write4Char(fs: TFileStream; s: string);
var
  i: integer;
begin
  for i := 1 to 4 do begin
    fs.WriteByte(Ord(s[i]));
  end;
end;

procedure _writeInternal(fs: TFileStream; w: uint32; littleEnd: boolean; sz: integer); inline;
var
  b: array[0..3] of byte;
  i: integer;
begin
  for i := 0 to sz - 1 do begin
    b[i] := w and $FF;
    w := w shr 8;
  end;
  if (littleEnd) then begin
    for i := 0 to sz - 1 do begin
      fs.WriteByte(b[i]);
    end;
  end
  else begin
    for i := sz - 1 downto 0 do begin
      fs.WriteByte(b[i]);
    end;
  end;
end;

procedure _writeDWord(fs: TFileStream; w: uint32; littleEnd: boolean);
begin
  _writeInternal(fs, w, littleEnd, 4);
end;

procedure _writeWord(fs: TFileStream; w: uint32; littleEnd: boolean);
begin
  _writeInternal(fs, w, littleEnd, 2);
end;

const
  HEADER_RIFF = 'RIFF';
  HEADER_WAVE = 'WAVE';
  HEADER_FMT = 'fmt ';
  HEADER_DATA = 'data';

function DAI_saveWAV(const outPath: string; var seg: RSegment): boolean;
var
  fs: TFileStream;
  i, fileSize: integer;
  temp: uint32;
begin
  Result := False;
  fs := TFileStream.Create(outPath, fmCreate);
  try
    try
      filesize := 0;
      // Write the styandard WAV header
      // RIFF marker
      _write4Char(fs, HEADER_RIFF);
      // file-size (equals file-size - 8) (4 bytes)
      temp := 0;
      _writeDWord(fs, temp, True);
      // Mark it as type "WAVE"
      _write4Char(fs, HEADER_WAVE);
      // Mark the format section
      _write4Char(fs, HEADER_FMT);
      // Length of format data. Allways 16 (4 bytes)
      temp := 16;
      _writeDWord(fs, temp, True);
      // Wave type PCM (2 bytes)
      temp := 1;
      _writeWord(fs, temp, True);
      // 1 Channel (2 bytes)
      temp := 1;
      _writeWord(fs, temp, True);
      // kHz Sample Rate (4 bytes)
      temp := 8790;
      _writeDWord(fs, temp, False);
      // (Sample Rate * Bit Size * Channels) / 8 (4 bytes)
      temp := 17580;
      _writeDWord(fs, temp, False);
      // (Bit Size * Channels) / 8 (2 bytes)
      temp := 2;
      _writeWord(fs, temp, True);
      // Bits per sample (=Bit Size * Samples) (2 bytes)
      temp := 16;
      _writeWord(fs, temp, True);
      // "data" marker
      _write4Char(fs, HEADER_DATA);
      // data-size (equals file-size - 44)
      temp := 0;
      _writeDWord(fs, temp, True);
      // DAI tape pre-leader & leader
      for i := 1 to 45 do begin
        _writeWAVbit(fs, 'P', fileSize);
      end;
      for i := 1 to 1834 do begin
        _writeWAVbit(fs, 'L', fileSize);
      end;
      _writeWAVbit(fs, '1', fileSize);
      _writeWAVbyte(fs, 85, fileSize);
      for i := 0 to seg.len - 1 do begin
        _writeWAVbyte(fs, seg.Data[i], fileSize);
      end;
      // Write the DAI tape trailer
      for i := 1 to 74 do begin
        _writeWAVbit(fs, 'T', fileSize);
      end;
      _writeWAVbit(fs, '1', fileSize);
      // Write the filesize int the WAV header
      fs.Seek(4, soBeginning);
      temp := filesize - 8;
      _writeDWord(fs, temp, False);
      fs.Seek(40, soBeginning);
      temp := filesize - 44;
      _writeDWord(fs, temp, False);
      Result := True;
      lastError := '';
    except
      on E: Exception do begin
        lastError := 'Expception: ' + E.Message;
        exit;
      end;
    end;
  finally
    fs.Free;
  end;
end;

end.
