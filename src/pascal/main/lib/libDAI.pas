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
  Classes, SysUtils, Graphics;

const
  DAI_SCEREEN_WIDTH_MAX = 352;
  DAI_SCEREEN_HEIGTH_MAX = 260;
  DAI_SCAN_LINES = 604;

  IMAGE_WIDTH_MAX = DAI_SCEREEN_WIDTH_MAX * 3;
  IMAGE_HEIGTH_MAX = DAI_SCAN_LINES;

  FONTCHAR_SIZE = 4 * 1024;

var
  //  Palette for 16 color gfx.
  DAI_PALETTE: array[0..15] of TColor;
  DAI_COLORREG: array[0..3] of byte;
  FONT: array[0..FONTCHAR_SIZE - 1] of byte;

const
  RES_HEIGHT: array[0..3] of integer = (260, 260, 260, 240);
  RES_WIDTH: array[0..3] of integer = (88, 176, 352, 528);
  RES_SCALE: array[0..3] of integer = (4, 2, 1, 0);
  RES_COLS: array[0..3] of integer = (11, 22, 44, 66);
  RES_XRES: array[0..3] of integer = (12, 6, 3, 2);

type
  TSegment = record
    addr: uint16;
    len: uint16;
    entrypoint: uint16;
    segType: uint16;
    Data: array of byte;
  end;

type
  ControlWord = record
    resolution: integer;    // 2 bits
    mode: integer;          // 2 bits
    repLines: integer;      // 4 bits
    enable_change: boolean;
    unit_color: boolean;
    color_reg: integer;
    color_sel: integer;
    //
    data_size: integer;
    line_width: integer;
    line_pxlHei: integer;
    line_colCnt: integer;
    line_pxlWdt: integer;
  end;

type
  DAI_func = function(const outPath: string; var seg: TSegment): boolean;

function DAI_lastError: string;

function DAI_initFont(const path: string): boolean;

function DAI_decodeDump(const Lines: TStringList; var seg: TSegment): boolean;

function DAI_loadBin(const outPath: string; var seg: TSegment): boolean;
function DAI_loadSBin(const outPath: string; var seg: TSegment): boolean;
function DAI_loadDump(const inPath: string; var seg: TSegment): boolean;

function DAI_saveBin(const outPath: string; var seg: TSegment): boolean;
function DAI_saveSBin(const outPath: string; var seg: TSegment): boolean;
function DAI_savePNG(const outPath: string; var seg: TSegment): boolean;

implementation

uses
  StrUtils;

threadvar
  lastError: string;

const
  MAX_ADDR = 64 * 1024 - 1;
  HEXNUM: TSysCharSet = ['0'..'9', 'A'..'F'];

function _saveSegData(fs: TFileStream; var seg: TSegment): boolean;
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

function _loadSegData(fs: TFileStream; sz: integer; var seg: TSegment): boolean;
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

// Decode `lines` which is in UT hex dump format. Return the binary data and the last address seen.
//  Example line:
// 'BFF0 00 00 B8 36 00 00 AF 36 00 00 9F 36 00 00 80 36'
function DAI_decodeDump(const Lines: TStringList; var seg: TSegment): boolean;
var
  s: string;
  b, i, pos: integer;
  numByte, saddr, eaddr: integer;
  minAddr, maxAddr: integer;
begin
  Result := False;
  minAddr := MaxInt;
  maxAddr := -1;
  seg.addr := 0;
  seg.len := MAX_ADDR;
  setLength(seg.Data, MAX_ADDR);
  try
    for s in Lines do begin
      if not _isValidLine(s) then begin
        continue;
      end;
      numByte := (length(s) - 4) div 3;
      saddr := Hex2Dec(Copy(s, 1, 4));
      eaddr := saddr + numByte - 1;
      if (eaddr > MAX_ADDR) then begin
        lastError := 'Invalid Address: ' + s;
        exit;
      end;
      pos := 6;
      if (saddr < minAddr) then begin
        minAddr := saddr;
      end;
      if (eaddr > maxAddr) then begin
        maxAddr := eaddr;
      end;
      for i := saddr to eaddr do begin
        b := Hex2Dec(copy(s, pos, 2));
        Inc(pos, 3);
        seg.Data[i] := b;
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
      lastError := 'Invalid HEX in dump file (' + s + ')';
    end;
  end;
  Result := True;
end;

function DAI_loadDump(const inPath: string; var seg: TSegment): boolean;
var
  Lines: TStringList;
begin
  Result := False;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(inPath);
    DAI_decodeDump(Lines, seg);
    Result := True;
  finally
    Lines.Free;
  end;
end;

function DAI_saveBin(const outPath: string; var seg: TSegment): boolean;
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

function DAI_saveSBin(const outPath: string; var seg: TSegment): boolean;
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

function DAI_loadBin(const outPath: string; var seg: TSegment): boolean;
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

function DAI_loadSBin(const outPath: string; var seg: TSegment): boolean;
var
  fs: TFileStream;
  sz: integer;
begin
  Result := False;
  lastError := 'Unable to read ' + outPath;
  fs := TFileStream.Create(outPath, fmOpenRead);
  try
    seg.addr := fs.ReadWord;
  except
    on E: Exception do begin
      lastError := 'Expception: ' + E.Message;
      fs.Free;
      exit;
    end;
  end;
  sz := fs.Size;
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

function _decodeControlWord(var seg: TSegment; var curAddr: integer): ControlWord;
var
  b: byte;
begin
  with Result do begin
    b := seg.Data[curAddr];
    Dec(curAddr);
    // Bits:
    //  7 - 1=16col 0=4col
    //  6 - text=1 graphic=0
    //  5, 4 - resolution control
    //  3, 2, 1, 0 - line repeat count
    resolution := (b shr 4) and $03;
    mode := (b shr 6) and $03;
    repLines := b and $0F;
    b := seg.Data[curAddr];
    Dec(curAddr);
    // Low address byte (color byte)
    enable_change := (b and $80) <> 0;
    unit_color := (b and $40) = 0;
    color_reg := (b shr 4) and $03;
    color_sel := b and $0F;
    if (enable_change) then begin
      DAI_COLORREG[color_reg] := color_sel;
    end;
    //... computations
    line_width := RES_WIDTH[resolution];
    line_colCnt := RES_COLS[resolution];
    line_pxlWdt := RES_XRES[resolution];
    line_pxlHei := (repLines + 1) * 2;
    if unit_color then begin
      data_size := 2;
    end
    else begin
      data_size := line_colCnt * 2;
    end;
  end;
end;

procedure _drawBlockGraph4(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k, l: integer;
  col: TColor;
  colIdx: integer;
begin
  for j := 0 to yl - 1 do begin
    for k := 0 to 7 do begin
      colIdx := (((data1 shr (7 - k)) and $01) shl 1) or ((data2 shr (7 - k)) and $01);
      col := DAI_PALETTE[DAI_COLORREG[colIdx]];
      for l := 0 to xl - 1 do begin
        C.Pixels[(i * 8 + k) * xl + l, curScanLine + j] := col;
      end;
    end;
  end;
end;

procedure _drawBlockText4(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k, l: integer;
  col: TColor;
  charData, colIdx: integer;
begin
  for j := 0 to yl - 1 do begin
    charData := FONT[data1 * 16 + j];
    for k := 0 to 7 do begin
      colIdx := (((charData shr k) and $01) shl 1) or ((data2 shr (7 - k)) and $01);
      col := DAI_PALETTE[DAI_COLORREG[colIdx]];
      for l := 0 to xl - 1 do begin
        C.Pixels[(i * 8 + k) * xl + l, curScanLine + j] := col;
      end;
    end;
  end;
end;

procedure _fillColor4(var seg: TSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  data1 := seg.Data[curAddr];
  Dec(curAddr);
  data2 := seg.Data[curAddr];
  Dec(curAddr);
  for  i := 0 to xc - 1 do begin
    _drawBlockGraph4(data1, data2, i, curScanLine, xl, yl, C);
  end;
end;

procedure _fillText4(var seg: TSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  data1 := seg.Data[curAddr];
  Dec(curAddr);
  data2 := seg.Data[curAddr];
  Dec(curAddr);
  for  i := 0 to xc - 1 do begin
    _drawBlockText4(data1, data2, i, curScanLine, xl, yl, C);
  end;
end;

procedure _decodeGraph4(var seg: TSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  for  i := 0 to xc - 1 do begin
    data1 := seg.Data[curAddr];
    Dec(curAddr);
    data2 := seg.Data[curAddr];
    Dec(curAddr);
    _drawBlockGraph4(data1, data2, i, curScanLine, xl, yl, C);
  end;
end;

procedure _decodeText4(var seg: TSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  for  i := 0 to xc - 1 do begin
    data1 := seg.Data[curAddr];
    Dec(curAddr);
    data2 := seg.Data[curAddr];
    Dec(curAddr);
    _drawBlockText4(data1, data2, i, curScanLine, xl, yl, C);
  end;
end;

procedure _drawBlockGraph16(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k, l: integer;
  col: TColor;
  colIdx: integer;
begin
  for j := 0 to yl - 1 do begin
    for k := 0 to 7 do begin
      if ((data1 shr (7 - k)) and $01) <> 0 then begin
        colIdx := (data2 shr 4) and $0F;
      end
      else begin
        colIdx := data2 and $0F;
      end;
      col := DAI_PALETTE[colIdx];
      for l := 0 to xl - 1 do begin
        C.Pixels[(i * 8 + k) * xl + l, curScanLine + j] := col;
      end;
    end;
  end;
end;

procedure _drawBlockText16(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k, l: integer;
  col: TColor;
  charData, colIdx: integer;
begin
  for j := 0 to yl - 1 do begin
    charData := FONT[data1 * 16 + j];
    for k := 0 to 7 do begin
      if ((charData shr k) and $01) <> 0 then begin
        colIdx := (data2 shr 4) and $0F;
      end
      else begin
        colIdx := data2 and $0F;
      end;
      col := DAI_PALETTE[colIdx];
      for l := 0 to xl - 1 do begin
        C.Pixels[(i * 8 + k) * xl + l, curScanLine + j] := col;
      end;
    end;
  end;
end;

procedure _fillColor16(var seg: TSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  data1 := seg.Data[curAddr];
  Dec(curAddr);
  data2 := seg.Data[curAddr];
  Dec(curAddr);
  for  i := 0 to xc - 1 do begin
    _drawBlockGraph16(data1, data2, i, curScanLine, xl, yl, C);
  end;
end;

procedure _fillText16(var seg: TSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  data1 := seg.Data[curAddr];
  Dec(curAddr);
  data2 := seg.Data[curAddr];
  Dec(curAddr);
  for  i := 0 to xc - 1 do begin
    _drawBlockText16(data1, data2, i, curScanLine, xl, yl, C);
  end;
end;

procedure _decodeGraph16(var seg: TSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  for  i := 0 to xc - 1 do begin
    data1 := seg.Data[curAddr];
    Dec(curAddr);
    data2 := seg.Data[curAddr];
    Dec(curAddr);
    _drawBlockGraph16(data1, data2, i, curScanLine, xl, yl, C);
  end;
end;

procedure _decodeText16(var seg: TSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  for  i := 0 to xc - 1 do begin
    data1 := seg.Data[curAddr];
    Dec(curAddr);
    data2 := seg.Data[curAddr];
    Dec(curAddr);
    _drawBlockText16(data1, data2, i, curScanLine, xl, yl, C);
  end;
end;

function DAI_decodeFrameBuffer(var seg: TSegment; curAddr: integer; C: TCanvas): boolean;
var
  curLin: integer;
  CW: ControlWord;
begin
  Result := False;
  curLin := 0;
  while (curLin < DAI_SCAN_LINES) do begin
    if (curAddr < 1) then begin
      lastError := 'Invalid frame buffer';
      exit;
    end;
    CW := _decodeControlWord(seg, curAddr);
    if (curAddr < (CW.data_size - 1)) then begin
      lastError := 'Invalid frame buffer';
      exit;
    end;
    if CW.unit_color then begin
      case CW.mode of
        %00: begin
          _fillColor4(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, C);
        end;
        %01: begin
          _fillColor4(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, C);
        end;
        %10: begin
          _fillColor16(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, C);
        end;
        %11: begin
          _fillColor16(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, C);
        end;
        else begin
        end;
      end;
    end
    else begin
      case CW.mode of
        %00: begin
          _decodeGraph4(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, C);
        end;
        %01: begin
          _decodeText4(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, C);
        end;
        %10: begin
          _decodeGraph16(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, C);
        end;
        %11: begin
          _decodeText16(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, C);
        end;
        else begin
        end;
      end;
    end;
    Inc(curLin, CW.line_pxlHei);
  end;
  Result := True;
end;

function DAI_savePNG(const outPath: string; var seg: TSegment): boolean;
var
  curAddr: integer;
  B: TPortableNetworkGraphic;
  C: TCanvas;
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
  B := TPortableNetworkGraphic.Create;
  B.SetSize(IMAGE_WIDTH_MAX, IMAGE_HEIGTH_MAX);
  C := B.Canvas;
  if DAI_decodeFrameBuffer(seg, curAddr, C) then begin
    try
      B.SaveToFile(outPath);
    except
      on E: Exception do begin
        lastError := 'Expception: ' + E.Message;
        exit;
      end;
    end;
  end;
  Result := True;
  lastError := '';
end;

initialization
  DAI_PALETTE[$0] := RGBToColor($00, $00, $00); //  0 Black
  DAI_PALETTE[$1] := RGBToColor($00, $00, $8b); //  1 Dark Blue
  DAI_PALETTE[$2] := RGBToColor($b1, $00, $95); //  2 Purple Red
  DAI_PALETTE[$3] := RGBToColor($ff, $00, $00); //  3 Red
  DAI_PALETTE[$4] := RGBToColor($75, $2e, $50); //  4 Purple Brown
  DAI_PALETTE[$5] := RGBToColor($00, $b2, $38); //  5 Emerald Green
  DAI_PALETTE[$6] := RGBToColor($98, $62, $00); //  6 Kakhi Brown
  DAI_PALETTE[$7] := RGBToColor($ae, $7a, $00); //  7 Mustard Brown
  DAI_PALETTE[$8] := RGBToColor($89, $89, $89); //  8 Grey
  DAI_PALETTE[$9] := RGBToColor($a1, $6f, $ff); //  9 Middle Blue
  DAI_PALETTE[$A] := RGBToColor($ff, $a5, $00); // 10 Orange
  DAI_PALETTE[$B] := RGBToColor($ff, $99, $ff); // 11 Pink
  DAI_PALETTE[$C] := RGBToColor($9e, $f4, $ff); // 12 Light Blue
  DAI_PALETTE[$D] := RGBToColor($b3, $ff, $bb); // 13 Light Green
  DAI_PALETTE[$E] := RGBToColor($ff, $ff, $28); // 14 Light Yellow
  DAI_PALETTE[$F] := RGBToColor($ff, $ff, $ff); // 15 White
  DAI_COLORREG[0] := 0;
  DAI_COLORREG[1] := 0;
  DAI_COLORREG[2] := 0;
  DAI_COLORREG[3] := 0;
end.

