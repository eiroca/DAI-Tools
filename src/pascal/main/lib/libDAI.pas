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
  libTools, bufstream,
  Classes, SysUtils, FPImage, FPReadPNG, FPWritePNG;

type
  DAI_func = function(const outPath: string; var seg: RSegment): boolean;

function DAI_lastError: string;

function DAI_loadBin(const inPath: string; var seg: RSegment): boolean;
function DAI_loadSBin(const inPath: string; var seg: RSegment): boolean;
function DAI_loadDump(const inPath: string; var seg: RSegment): boolean;
function DAI_loadDAI(const inPath: string; var seg: RSegment): boolean;
function DAI_loadWAV(const inPath: string; var seg: RSegment): boolean;
function DAI_loadPNG(const inPath: string; var seg: RSegment): boolean;
function DAI_loadPNGOpt(const inPath: string; var seg: RSegment): boolean;

function DAI_loadPNG(const image: TFPCustomImage; optimize: boolean; var seg: RSegment): boolean;

function DAI_saveBin(const outPath: string; var seg: RSegment): boolean;
function DAI_saveSBin(const outPath: string; var seg: RSegment): boolean;
function DAI_saveDump(const outPath: string; var seg: RSegment): boolean;
function DAI_saveASM(const outPath: string; var seg: RSegment): boolean;
function DAI_savePNG(const outPath: string; var seg: RSegment): boolean;
function DAI_saveFullPNG(const outPath: string; var seg: RSegment): boolean;
function DAI_saveDAIbin(const outPath: string; var seg: RSegment): boolean;
function DAI_saveDAIbas(const outPath: string; var seg: RSegment): boolean;
function DAI_saveWAV(const outPath: string; var seg: RSegment): boolean;
function DAI_saveHRFB(const outPath: string; var seg: RSegment): boolean;

implementation

uses
  libGraph, libGraphFast, libGraphFull, libGraphFrame, libAudio;

const
  EXCEPTION_FMT = 'Exception: %s';
  ERR_INVALIDSEGMENT = 'Invalid segment';

threadvar
  lastError: string;

function DAI_lastError: string;
begin
  Result := lastError;
end;

function _saveSegData(const path: string; var seg: RSegment; const writeAddr: boolean): boolean;
var
  fs: TBufferedFileStream;
  i: integer;
begin
  Result := False;
  fs := TBufferedFileStream.Create(path, fmCreate);
  try
    try
      if writeAddr then begin
        fs.WriteWord(seg.addr);
      end;
      for i := 0 to seg.size - 1 do begin
        fs.WriteByte(seg.Data[i]);
      end;
      Result := True;
    except
      on E: Exception do lastError := Format(EXCEPTION_FMT, [E.Message]);
    end;
  finally
    fs.Free;
  end;
end;

function _loadSegData(const path: string; var seg: RSegment; const readAddr: boolean): boolean;
var
  fs: TBufferedFileStream;
  sz: integer;
  i: integer;
begin
  Result := False;
  fs := TBufferedFileStream.Create(path, fmOpenRead);
  if (readAddr) then begin
    sz := fs.Size - 2;
    seg.addr := fs.ReadWord;
  end
  else begin
    sz := fs.Size;
    seg.addr := 0;
  end;
  SetLength(seg.Data, sz);
  seg.size := sz;
  try
    try
      for i := 0 to sz - 1 do begin
        seg.Data[i] := fs.ReadByte();
      end;
      Result := True;
    except
      on E: Exception do lastError := 'Exception: ' + E.Message;
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
begin
  if (seg.size = 0) then begin
    Result := False;
    lastError := ERR_INVALIDSEGMENT;
    exit;
  end;
  Result := True;
  Lines := TStringList.Create;
  try
    Segment_text(seg, Lines, '>D%.4x %.4x', '%.4x ', '%.2x', ' ');
    if (seg.entrypoint <> 0) then begin
      Lines.Insert(1, Format('>Run Address = %4x', [seg.entrypoint]));
    end;
    try
      Lines.SaveToFile(outPath);
    except
      on E: Exception do begin
        Result := False;
        lastError := Format(EXCEPTION_FMT, [E.Message]);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function DAI_saveASM(const outPath: string; var seg: RSegment): boolean;
var
  Lines: TStringList;
begin
  if (seg.size = 0) then begin
    Result := False;
    lastError := ERR_INVALIDSEGMENT;
    exit;
  end;
  Result := True;
  Lines := TStringList.Create;
  try
    Segment_text(seg, Lines, #9'.org'#9'$%.4x', #9'.byte'#9, '$%.2x', ',');
    try
      Lines.SaveToFile(outPath);
    except
      on E: Exception do begin
        Result := False;
        lastError := Format(EXCEPTION_FMT, [E.Message]);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function DAI_saveBin(const outPath: string; var seg: RSegment): boolean;
begin
  Result := False;
  lastError := 'Unable to write ' + outPath;
  if (seg.size > 0) then begin
    if _saveSegData(outPath, seg, False) then begin
      Result := True;
      lastError := '';
    end;
  end
  else begin
    lastError := ERR_INVALIDSEGMENT;
  end;
end;

function DAI_saveSBin(const outPath: string; var seg: RSegment): boolean;
begin
  Result := False;
  lastError := 'Unable to write ' + outPath;
  if (seg.size > 0) then begin
    if _saveSegData(outPath, seg, True) then begin
      Result := True;
      lastError := '';
    end;
  end
  else begin
    lastError := ERR_INVALIDSEGMENT;
  end;
end;

function DAI_loadDAI(const inPath: string; var seg: RSegment): boolean;
begin
  Result := False;
  lastError := 'Unable to read ' + inPath;
  if _loadSegData(inPath, seg, False) then begin
    Result := True;
    lastError := '';
  end;
end;

function DAI_loadBin(const inPath: string; var seg: RSegment): boolean;
begin
  Result := False;
  lastError := 'Unable to read ' + inPath;
  if _loadSegData(inPath, seg, False) then begin
    Result := True;
    lastError := '';
  end;
end;

function DAI_loadSBin(const inPath: string; var seg: RSegment): boolean;
begin
  Result := False;
  lastError := 'Unable to read ' + inPath;
  if _loadSegData(inPath, seg, True) then begin
    Result := True;
    lastError := '';
  end;
end;

function DAI_savePNG(const outPath: string; var seg: RSegment): boolean;
var
  curAddr: integer;
  image: TFPCustomImage;
  writer: TFPCustomImageWriter;
  fbi: RFrameBufferInfo;
begin
  Result := False;
  lastError := 'Unable to write ' + outPath;
  if (seg.size < 4) then begin
    lastError := ERR_INVALIDSEGMENT;
    exit;
  end;
  curAddr := -1;
  if (curAddr = -1) then begin
    curAddr := seg.size - 1;
  end;
  fbi.numCW := 0;
  DAI_infoFrameBuffer(seg, curAddr, fbi);
  seg.entrypoint := curAddr - fbi.sizeVis + 1;
  image := nil;
  writer := nil;
  try
    image := TFPMemoryImage.Create(DAI_SCREEN_WIDTH, DAI_SCREEN_LINES);
    writer := TFPWriterPNG.Create;
    if DAI_decodeFrameBuffer(seg, curAddr, image) then begin
      try
        image.SaveToFile(outPath, writer);
        Result := True;
        lastError := '';
      except
        on E: Exception do begin
          lastError := Format(EXCEPTION_FMT, [E.Message]);
          exit;
        end;
      end;
    end
    else begin
      lastError := 'Invalid Frame Buffer';
    end;
  finally
    if (image <> nil) then begin
      FreeAndNil(image);
    end;
    if (writer <> nil) then begin
      FreeAndNil(writer);
    end;
  end;
end;

function DAI_saveFullPNG(const outPath: string; var seg: RSegment): boolean;
var
  curAddr: integer;
  image: TFPCustomImage;
  writer: TFPCustomImageWriter;
  fbi: RFrameBufferInfo;
begin
  Result := False;
  lastError := 'Unable to write ' + outPath;
  if (seg.size < 4) then begin
    lastError := ERR_INVALIDSEGMENT;
    exit;
  end;
  curAddr := -1;
  if (curAddr = -1) then begin
    curAddr := seg.size - 1;
  end;
  fbi.numCW := 0;
  DAI_infoFrameBuffer(seg, curAddr, fbi);
  seg.entrypoint := curAddr - fbi.sizeVis + 1;
  image := nil;
  writer := nil;
  try
    image := TFPMemoryImage.Create(DAI_SCREEN_WIDTH, DAI_SCREEN_LINES);
    writer := TFPWriterPNG.Create;
    if DAI_decodeFullFrameBuffer(seg, curAddr, image) then begin
      try
        image.SaveToFile(outPath, writer);
        Result := True;
        lastError := '';
      except
        on E: Exception do begin
          lastError := Format(EXCEPTION_FMT, [E.Message]);
          exit;
        end;
      end;
    end
    else begin
      lastError := 'Invalid Frame Buffer';
    end;
  finally
    if (image <> nil) then begin
      FreeAndNil(image);
    end;
    if (writer <> nil) then begin
      FreeAndNil(writer);
    end;
  end;
end;

function Checksum(cs: integer; n: integer): integer;
  // Calculate the checksum following the DAI protocol
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

procedure _writeByte(fs: TBufferedFileStream; const b: byte; var cs: integer); inline;
begin
  fs.writeByte(b);
  cs := Checksum(cs, b);
end;

procedure _writeString(fs: TBufferedFileStream; const s: string; var cs: integer); inline;
var
  car: char;
begin
  for car in s do begin
    _writeByte(fs, Ord(car), cs);
  end;
end;

function DAI_saveDAIbin(const outPath: string; var seg: RSegment): boolean;
var
  fs: TBufferedFileStream;
  cs: integer;
  b: byte;
  addrL, addrH: integer;
  sizeL, sizeH: integer;
begin
  Result := False;
  fs := TBufferedFileStream.Create(outPath, fmCreate);
  try
    try
      // File Type = 49 (31H for Binary file to load in UT with "R" command)
      fs.WriteByte($31);
      // Name Length
      cs := 86;
      _writeByte(fs, 0, cs);
      _writeByte(fs, Length(seg.Name), cs);
      fs.writeByte(cs);
      // Name
      cs := 86;
      _writeString(fs, seg.Name, cs);
      fs.writeByte(cs);
      // Length of Address
      cs := 86;
      _writeByte(fs, 0, cs);
      _writeByte(fs, 2, cs);
      fs.writeByte(cs);
      // Start Address
      addrL := seg.addr and $FF;
      addrH := (seg.addr shr 8) and $FF;
      cs := 86;
      _writeByte(fs, addrL, cs);
      _writeByte(fs, addrH, cs);
      fs.writeByte(cs);
      // Length of Content
      sizeL := seg.size and 255;
      sizeH := (seg.size shr 8) and 255;
      cs := 86;
      _writeByte(fs, sizeH, cs);
      _writeByte(fs, sizeL, cs);
      fs.writeByte(cs);
      // Content
      cs := 86;
      for b in seg.Data do begin
        _writeByte(fs, b, cs);
      end;
      fs.writeByte(cs);
      //
      Result := True;
      lastError := '';
    except
      on E: Exception do begin
        lastError := Format(EXCEPTION_FMT, [E.Message]);
        exit;
      end;
    end;
  finally
    fs.Free;
  end;
end;

function DAI_saveDAIbas(const outPath: string; var seg: RSegment): boolean;
var
  fs: TBufferedFileStream;
  cs: integer;
  b: byte;
  sizeL, sizeH: integer;
begin
  Result := False;
  fs := TBufferedFileStream.Create(outPath, fmCreate);
  try
    try
      // File Type = 30H for Basic
      fs.WriteByte($30);
      // Name Length
      cs := 86;
      _writeByte(fs, 0, cs);
      _writeByte(fs, Length(seg.Name), cs);
      fs.writeByte(cs);
      // Name
      cs := 86;
      _writeString(fs, seg.Name, cs);
      fs.writeByte(cs);
      // Program Block
      sizeL := seg.size and 255;
      sizeH := (seg.size shr 8) and 255;
      cs := 86;
      _writeByte(fs, sizeH, cs);
      _writeByte(fs, sizeL, cs);
      fs.writeByte(cs);
      cs := 86;
      for b in seg.Data do begin
        _writeByte(fs, b, cs);
      end;
      fs.writeByte(cs);
      // Simbol Block
      sizeL := 1;
      sizeH := 0;
      cs := 86;
      _writeByte(fs, sizeH, cs);
      _writeByte(fs, sizeL, cs);
      fs.writeByte(cs);
      cs := 86;
      _writeByte(fs, 0, cs);
      fs.writeByte(cs);
      //
      Result := True;
      lastError := '';
    except
      on E: Exception do begin
        lastError := Format(EXCEPTION_FMT, [E.Message]);
        exit;
      end;
    end;
  finally
    fs.Free;
  end;
end;

procedure _writeAudioSymbolP(var d: RAudio);
// P (Pre-Leader): 1 x 00-80
begin
  Audio_Append(d, False, 1);
end;

procedure _writeAudioSymbolL(var d: RAudio);
// L (Leader): 7 x FF-7F / 7 x 00-80 / 7 x FF-7F / 7 x 00-80
begin
  Audio_Append(d, True, 13);
  Audio_Append(d, False, 13);
  Audio_Append(d, True, 13);
  Audio_Append(d, False, 13);
end;

procedure _writeAudioSymbolT(var d: RAudio);
// T (Trailer): 10 x FF-7F / 10 x 00-80 / 14 x FF-7F / 14 x 00-80
begin
  Audio_Append(d, True, 9);
  Audio_Append(d, False, 9);
  Audio_Append(d, True, 12);
  Audio_Append(d, False, 13);
end;

procedure _writeAudioSymbol(var d: RAudio; b: boolean);
(* Write a BIT
 1 (bit 1): 22 x FF-7F / 22 x 00-80 / 14 x FF-7F / 14 x 00-80
 0 (bit 0): 14 x FF-7F / 14 x 00-80 / 22 x FF-7F / 22 x 00-80
*)
var
  nbre_a, nbre_b: integer;
begin
  if b then begin
    nbre_a := 21;
    nbre_b := 13;
  end
  else begin
    nbre_a := 13;
    nbre_b := 21;
  end;
  Audio_Append(d, True, nbre_a);
  Audio_Append(d, False, nbre_a);
  Audio_Append(d, True, nbre_b);
  Audio_Append(d, False, nbre_b);
end;

procedure _writeAudioByte(var d: RAudio; b: byte);
// Decompose a Byte in Bits String and write them
var
  i: integer;
  m: integer;
begin
  m := $80;
  for i := 0 to 7 do begin
    _writeAudioSymbol(d, (b and m) <> 0);
    m := m shr 1;
  end;
end;

function DAI_saveWAV(const outPath: string; var seg: RSegment): boolean;
var
  i: integer;
  d: RAudio;
begin
  Result := False;
  try
    d.len := 0;
    Audio_Init(d);
    // DAI tape
    // pre-leader
    for i := 1 to 89 do begin
      _writeAudioSymbolP(d);
    end;
    // leader
    for i := 1 to 1976 do begin
      _writeAudioSymbolL(d);
    end;
    _writeAudioSymbol(d, True);
    _writeAudioByte(d, $55);
    for i := 0 to seg.size - 1 do begin
      _writeAudioByte(d, seg.Data[i]);
    end;
    // Write the DAI tape trailer
    for i := 1 to 74 do begin
      _writeAudioSymbolT(d);
    end;
    _writeAudioSymbol(d, True);
    Audio_Trunc(d);
    Audio_Save(d, outPath);
    Result := True;
    lastError := '';
  except
    on E: Exception do begin
      lastError := Format(EXCEPTION_FMT, [E.Message]);
      exit;
    end;
  end;
end;

function _readAudioBit(d: RAudio; const len: integer; var p: integer; var b: boolean; var valid: boolean): boolean;
var
  L1, L2, L3, L4: integer;
begin
  Result := False;
  if (d.len - p) < 4 then begin
    exit;
  end;
  if not d.bit[p + 0] or not d.bit[p + 2] or d.bit[p + 1] or d.bit[p + 3] then begin
    exit;
  end;
  L1 := d.dur[p + 0];
  L2 := d.dur[p + 1];
  if (abs(L1 - L2) > 2) then begin
    exit;
  end;
  L3 := d.dur[p + 2];
  L4 := d.dur[p + 3];
  if (abs(L3 - L4) > 2) then begin
    exit;
  end;
  Inc(p, 4);
  b := (L1 + L2) > (L3 + L4);
  valid := (L1 + L2 + L3 + L4) > len;
  Result := True;
end;

function _readAudioByte(d: RAudio; const len: integer; var p: integer; var by: byte; var valid: boolean): boolean;
var
  m: integer;
  vb, b: boolean;
  i: integer;
begin
  Result := False;
  valid := False;
  vb := False;
  b := False;
  m := $80;
  by := 0;
  for i := 0 to 7 do begin
    if not _readAudioBit(d, len, p, b, vb) then begin
      exit;
    end;
    if not vb then begin // end of bytes, start of epilogue (probably)
      if (i = 0) then begin
        valid := False;
        Result := True;
      end;
      exit;
    end;
    if b then begin
      by := by or m;
    end;
    m := m shr 1;
  end;
  Valid := True;
  Result := True;
end;

function _readAudioLeader(d: RAudio; var i: integer; var bitL: integer): boolean;
var
  L1, L2, L3, L4: integer;
  cntL, sumL: integer;
begin
  sumL := 0;
  cntL := 0;
  Result := False;
  while True do begin
    if (d.len - i) < 4 then begin
      exit;
    end;
    if not d.bit[i + 0] or not d.bit[i + 2] or d.bit[i + 1] or d.bit[i + 3] then begin
      exit;
    end;
    L1 := d.dur[i + 0];
    L2 := d.dur[i + 1];
    L3 := d.dur[i + 2];
    L4 := d.dur[i + 3];
    if abs((L1 + L2) - (L3 + L4)) > 4 then begin
      break;
    end;
    SumL := sumL + L1 + L2 + L3 + L4;
    Inc(cntL);
    Inc(i, 4);
  end;
  bitL := sumL div cntL;
  Result := True;
end;

function _readAudioTrailer(d: RAudio; var i: integer; const bitL: integer): boolean;
var
  L1, L2, L3, L4: integer;
begin
  Result := False;
  while True do begin
    if (d.len - i) < 4 then begin
      exit;
    end;
    if not d.bit[i + 0] or not d.bit[i + 2] or d.bit[i + 1] or d.bit[i + 3] then begin
      exit;
    end;
    L1 := d.dur[i + 0];
    L2 := d.dur[i + 1];
    L3 := d.dur[i + 2];
    L4 := d.dur[i + 3];
    if (L1 + L2) < (L3 + L4) then begin
      break;
    end;
    if (L1 + L2 + L3 + L4) > bitL then begin
      break;
    end;
    Inc(i, 4);
  end;
  Result := True;
end;

function DAI_loadWAV(const inPath: string; var seg: RSegment): boolean;
var
  i, j: integer;
  bitL: integer;
  cntB: integer;
  eb, vb, vr, b: boolean;
  by: byte;
  d: RAudio;
begin
  Result := False;
  try
    d.len := 0;
    if not Audio_Load(inPath, d) then begin
      lastError := 'Invalid data in WAV';
      exit;
    end;
    if d.len < 6 then begin
      lastError := 'Invalid codification';
      exit;
    end;
    i := 0;
    // Pre Leader
    if d.bit[i] <> False then begin
      lastError := 'Invalid Pre-Leader';
      exit;
    end;
    Inc(i);
    // Leader
    bitL := 0;
    if not _readAudioLeader(d, i, bitL) then begin
      lastError := 'Invalid Leader';
      exit;
    end;
    eb := True;
    b := False;
    vb := True;
    for j := 1 to 9 do begin
      if not _readAudioBit(d, bitL, i, b, vb) or (b <> eb) or not vb then begin
        lastError := 'Invalid Preamble';
        exit;
      end;
      eb := not eb;
    end;
    Segment_init(seg);
    cntB := 0;
    by := 0;
    while True do begin
      vr := _readAudioByte(d, bitL, i, by, vb);
      if (vr and not vb) then begin
        Segment_Resize(seg, cntB);
        break; // valid result but not valid byte -> end of stream
      end;
      if (not vr or not vb) then begin
        lastError := 'Invalid Data';
        exit;
      end;
      seg.Data[cntB] := by;
      Inc(cntB);
      if (cntB >= seg.size) then begin
        lastError := 'Too big';
        exit;
      end;
    end;
    if not _readAudioTrailer(d, i, bitL) then begin
      lastError := 'Invalid Tailer';
      exit;
    end;
    Result := True;
    lastError := '';
  except
    on E: Exception do begin
      lastError := Format(EXCEPTION_FMT, [E.Message]);
      exit;
    end;
  end;
end;

function _loadPNG(image: TFPCustomImage; var seg: RSegment; optimize: boolean): boolean;
begin
  Result := False;
  Segment_init(seg, $C000);
  try
    if (image.Width <> DAI_SCREEN_WIDTH) or (image.Height <> DAI_SCREEN_LINES) then begin
      lastError := Format('Image must be (%d,%d)', [DAI_SCREEN_WIDTH, DAI_SCREEN_LINES]);
      exit;
    end;
    if (optimize) then begin
      if not DAI_createFrameOpt(seg, image) then begin
        lastError := 'Unable to crate FrameBuffer';
        exit;
      end;
    end
    else begin
      if not DAI_createFrame(seg, image) then begin
        lastError := 'Unable to crate FrameBuffer';
        exit;
      end;
    end;
    Result := True;
    lastError := '';
  except
    on E: Exception do begin
      lastError := Format(EXCEPTION_FMT, [E.Message]);
      exit;
    end;
  end;
end;

function _loadPNG(const inPath: string; var seg: RSegment; optimize: boolean): boolean;
var
  image: TFPCustomImage;
  reader: TFPCustomImageReader;
begin
  Result := False;
  lastError := 'Unable to read ' + inPath;
  image := nil;
  reader := nil;
  try
    try
      image := TFPMemoryImage.Create(0, 0);
      reader := TFPReaderPNG.Create;
      image.LoadFromFile(inPath, reader);
    except
      on E: Exception do begin
        lastError := Format(EXCEPTION_FMT, [E.Message]);
        exit;
      end;
    end;
    Result := _loadPNG(image, seg, optimize);
  finally
    if (image <> nil) then begin
      FreeAndNil(image);
    end;
    if (reader <> nil) then begin
      FreeAndNil(reader);
    end;
  end;
end;

function DAI_loadPNG(const image: TFPCustomImage; optimize: boolean; var seg: RSegment): boolean;
begin
  Result := _loadPNG(image, seg, optimize);
end;

function DAI_loadPNG(const inPath: string; var seg: RSegment): boolean;
begin
  Result := _loadPNG(inPath, seg, False);
end;

function DAI_loadPNGOpt(const inPath: string; var seg: RSegment): boolean;
begin
  Result := _loadPNG(inPath, seg, True);
end;

function DAI_saveHRFB(const outPath: string; var seg: RSegment): boolean;
var
  Lines: TStringList;
  curAddr: integer;
begin
  if (seg.size < 4) then begin
    Result := False;
    lastError := ERR_INVALIDSEGMENT;
    exit;
  end;
  Result := True;
  Lines := TStringList.Create;
  try
    curAddr := -1;
    if (curAddr = -1) then begin
      curAddr := seg.size - 1;
    end;
    if not DAI_FrameBufferToText(seg, curAddr, Lines) then begin
      Result := False;
      lastError := ERR_INVALIDSEGMENT;
      exit;
    end;
    try
      Lines.SaveToFile(outPath);
    except
      on E: Exception do begin
        Result := False;
        lastError := Format(EXCEPTION_FMT, [E.Message]);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

end.
