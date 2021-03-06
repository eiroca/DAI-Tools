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
unit libGraph;

{$mode objfpc}{$H+}

interface

uses
  libTools,
  Classes, SysUtils, StrUtils, FPImage;

const
  PAL_SCANLINES = 604;
  PAL_SCANLINES_VISIBLE = 528;

  DAI_SCREEN_WIDTH = 352;
  DAI_SCREEN_LINES = PAL_SCANLINES_VISIBLE div 2;
  DAI_SCAN_LINES = PAL_SCANLINES div 2;

  DAI_IMAGE_WIDTH = DAI_SCREEN_WIDTH * 3;
  DAI_IMAGE_LINES = PAL_SCANLINES div 2 * 3;

  FONTCHAR_SIZE = 4 * 1024;

const
  RES_HEIGHT: array[0..3] of integer = (260, 260, 260, 240);
  RES_WIDTH: array[0..3] of integer = (88, 176, 352, 528);
  RES_COLS: array[0..3] of integer = (11, 22, 44, 66);
  RES_PXL: array[0..3] of integer = (4, 2, 1, 1);
  RES_DOT: array[0..3] of integer = (12, 6, 3, 2);

type
  ColorTable = array[0..3] of integer;

var
  //  Palette for 16 color gfx.
  DAI_PALETTE: TFPPalette;
  DAI_COLORREG: ColorTable;
  FONT: array[0..FONTCHAR_SIZE - 1] of byte;

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
    line_colCnt: integer;
    line_dotHei: integer;
    line_dotWdt: integer;
    line_pxlHei: integer;
    line_pxlWdt: integer;
  end;

type
  EMode = (mText, mGraph, mFill);

  RModeInfo = record
    min: integer;
    max: integer;
    col_04: boolean;
    col_16: boolean;
  end;

  RFrameBufferInfo = record
    stat: array [EMode] of RModeInfo;
    numCW: integer;
    sizeVis, sizeFull: integer;
  end;

function DAI_initFont(const path: string): boolean;
function DAI_infoFrameBuffer(var seg: RSegment; curAddr: integer; var fbi: RFrameBufferInfo): boolean;
function DAI_encodeControlWord(resolution: integer; mode: integer; repLines: integer; enable_change: boolean; unit_color: boolean; color_reg: integer; color_sel: integer): word;
function DAI_decodeControlWord(var seg: RSegment; var curAddr: integer): ControlWord;
function DAI_FrameBufferToText(var seg: RSegment; curAddr: integer; L: TStringList): boolean;
function DAI_TextToFrameBuffer(L: TStringList; out seg: RSegment; out msg: string): boolean;

implementation

const
  ENABLE_CHANGE_val: array [boolean] of integer = (0, $80);
  UNIT_COLOR_val: array [boolean] of integer = ($40, 0);
  BOOL2INT: array[boolean] of integer = (0, 1);


function DAI_encodeControlWord(resolution: integer; mode: integer; repLines: integer; enable_change: boolean; unit_color: boolean; color_reg: integer; color_sel: integer): word;
begin
  Result := mode shl 14 + resolution shl 12 + repLines shl 8 + ENABLE_CHANGE_val[enable_change] + UNIT_COLOR_val[unit_color] + color_sel shl 4 + color_reg;
end;

function DAI_decodeControlWord(var seg: RSegment; var curAddr: integer): ControlWord;
var
  b1, b2: byte;
begin
  with Result do begin
    b1 := seg.Data[curAddr];
    Dec(curAddr);
    // Bits:
    //  7 - 1=16col 0=4col
    //  6 - text=1 graphic=0
    //  5, 4 - resolution control
    //  3, 2, 1, 0 - line repeat count
    resolution := (b1 shr 4) and $03;
    mode := (b1 shr 6) and $03;
    repLines := b1 and $0F;
    b2 := seg.Data[curAddr];
    Dec(curAddr);
    // Low address byte (color byte)
    enable_change := (b2 and $80) <> 0;
    unit_color := (b2 and $40) = 0;
    color_reg := (b2 shr 4) and $03;
    color_sel := b2 and $0F;
    if (enable_change) then begin
      DAI_COLORREG[color_reg] := color_sel;
    end;
    //... computations
    line_width := RES_WIDTH[resolution];
    line_colCnt := RES_COLS[resolution];
    line_pxlHei := (repLines + 1);
    line_pxlWdt := RES_PXL[resolution];
    line_dotHei := (repLines + 1) * 3;
    line_dotWdt := RES_DOT[resolution];
    if unit_color then begin
      data_size := 2;
    end
    else begin
      data_size := line_colCnt * 2;
    end;
  end;
end;

function DAI_infoFrameBuffer(var seg: RSegment; curAddr: integer; var fbi: RFrameBufferInfo): boolean;
var
  curLin: integer;
  CW: ControlWord;
  e: EMode;
  v: integer;
  col16: boolean;
begin
  Result := False;
  curLin := 0;
  with fbi do begin
    for e in EMode do begin
      with  stat[e] do begin
        min := MaxInt;
        max := 0;
        col_16 := False;
        col_04 := False;
      end;
    end;
    numCW := 0;
    sizeFull := 0;
    sizeVis := 0;
    while (curLin < PAL_SCANLINES) do begin
      if (curAddr < 1) then begin
        exit;
      end;
      CW := DAI_decodeControlWord(seg, curAddr);
      Inc(numCW);
      if (curAddr < (CW.data_size - 1)) then begin
        exit;
      end;
      Dec(curAddr, CW.data_size);
      if CW.unit_color then begin
        e := mFill;
        v := CW.line_width;
        case CW.mode of
          %00: begin
            col16 := False;
          end;
          %01: begin
            col16 := True;
          end;
          %10: begin
            col16 := False;
          end;
          else begin // %11
            col16 := True;
          end;
        end;
      end
      else begin
        case CW.mode of
          %00: begin
            col16 := False;
            v := CW.line_width;
            e := mGraph;
          end;
          %01: begin
            col16 := True;
            v := CW.line_colCnt;
            e := mText;
          end;
          %10: begin
            col16 := False;
            v := CW.line_width;
            e := mGraph;
          end;
          else begin // %11
            col16 := True;
            v := CW.line_colCnt;
            e := mText;
          end;
        end;
      end;
      with stat[e] do begin
        if (col16) then begin
          col_16 := True;
        end
        else begin
          col_04 := True;
        end;
        if min > v then begin
          min := v;
        end;
        if max < v then begin
          max := v;
        end;
      end;
      Inc(sizeFull, 2 + CW.data_size);
      if (curLin < PAL_SCANLINES_VISIBLE) then begin
        Inc(sizeVis, 2 + CW.data_size);
      end;
      Inc(curLin, CW.line_dotHei);
    end;
    for e in EMode do begin
      with  stat[e] do begin
        if (min > max) then begin
          min := max;
        end;
      end;
    end;
  end;
  Result := True;
end;

function DAI_initFont(const path: string): boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(Path, fmOpenRead);
  try
    try
      if (fs.Size <> FONTCHAR_SIZE) then begin
        exit;
      end;
      fs.Read(FONT, FONTCHAR_SIZE);
      Result := True;
    except
      Result := False;
    end;
  finally
    fs.Free;
  end;
end;

const
  FMT_WORD = ' $%1:.2x%0:.2x';

function DAI_FrameBufferToText(var seg: RSegment; curAddr: integer; L: TStringList): boolean;
var
  curLin: integer;
  CW: ControlWord;
  rows, i, k: integer;
  offset, data1, data2: integer;
  s: string;
  md: string;
  c1, c2: string;
  palette: boolean;
begin
  Result := False;
  curLin := 0;
  rows := PAL_SCANLINES;
  offset := $BFFF - curAddr;
  while (curLin < rows) do begin
    if (curAddr < 1) then begin
      exit;
    end;
    s := Format('[A:%.4x][L:%.3d]', [curAddr + offset, curLin div 2]);
    CW := DAI_decodeControlWord(seg, curAddr);
    if (curAddr < (CW.data_size - 1)) then begin
      exit;
    end;
    palette := False;
    if CW.unit_color then begin
      case CW.mode of
        %00: begin
          md := 'F4';
          palette := True;
        end;
        %01: begin
          md := 'R4';
          palette := True;
        end;
        %10: begin
          md := 'F';
        end;
        else begin
          md := 'R';
        end;
      end;
    end
    else begin
      case CW.mode of
        %00: begin
          md := 'G4';
          palette := True;
        end;
        %01: begin
          md := 'T4';
          palette := True;
        end;
        %10: begin
          md := 'G';
        end;
        else begin
          md := 'T';
        end;
      end;
    end;
    if (CW.enable_change) then begin
      DAI_COLORREG[CW.color_reg] := CW.color_sel;
    end;
    if palette then begin
      s := s + Format('[P:%x,%x,%x,%x]', [DAI_COLORREG[0], DAI_COLORREG[1], DAI_COLORREG[2], DAI_COLORREG[3]]);
    end;
    s := s + Format(' %dx%d %s', [CW.line_width, CW.line_pxlHei, md]);
    if (CW.enable_change) then begin
      s := s + Format(' P%x=%x', [CW.color_reg, CW.color_sel]);
    end;
    if (md = 'F') or (md = 'R') then begin
      data1 := seg.Data[curAddr];
      Dec(curAddr);
      data2 := seg.Data[curAddr];
      Dec(curAddr);
      s := s + Format(FMT_WORD, [data1, data2]);
    end
    else if (md = 'G') then begin
      for i := 0 to CW.line_colCnt - 1 do begin
        data1 := seg.Data[curAddr];
        Dec(curAddr);
        data2 := seg.Data[curAddr];
        Dec(curAddr);
        if not palette then begin
          s := s + ' ';
          c1 := IntToHex(data2 and $0F, 1);
          c2 := IntToHex((data2 shr 4) and $0F, 1);
          for k := 0 to 7 do begin
            if ((data1 shr (7 - k)) and $01) <> 0 then begin
              s := s + c2;
            end
            else begin
              s := s + c1;
            end;
          end;
        end
        else begin
          s := s + Format(FMT_WORD, [data1, data2]);
        end;
      end;
    end
    else begin
      for i := 0 to CW.line_colCnt - 1 do begin
        data1 := seg.Data[curAddr];
        Dec(curAddr);
        data2 := seg.Data[curAddr];
        Dec(curAddr);
        s := s + Format(FMT_WORD, [data1, data2]);
      end;
    end;
    L.Add(s);
    Inc(curLin, CW.line_pxlHei * 2);
  end;
  Result := True;
end;

function _getToken(s: string; var pos: longint): string;
var
  start, off, l: integer;
begin
  Result := '';
  l := Length(s);
  while (pos <= l) do begin
    if (s[pos] = ' ') or (s[pos] = #9) then begin
      Inc(pos);
    end
    else if (s[pos] = '[') then begin
      while (s[pos] <> ']') and (pos <= l) do begin
        Inc(pos);
      end;
      Inc(pos);
    end
    else begin
      start := pos;
      Inc(pos);
      off := 1;
      while (pos <= l) do begin
        if (s[pos] = ' ') then begin
          off := 0;
          break;
        end;
        Inc(pos);
      end;
      Result := copy(s, start, (pos - start + off));
      Break;
    end;
  end;
end;

function _getWord(s: string; var pos: longint; out w: integer): boolean;
var
  t: string;
  c: char;
  v, r, v1, v2: integer;
  bit: boolean;
  i: integer;
begin
  w := -1;
  t := _getToken(s, pos);
  if (t <> '') then begin
    try
      v1 := -1;
      v2 := -1;
      if length(t) = 8 then begin
        r := 0;
        for i := 1 to 8 do begin
          c := t[i];
          v := Hex2Dec(c);
          if (v1 = -1) or (v = v1) then begin
            v1 := v;
            bit := true;
          end
          else if (v2 = -1) or (v = v2) then begin
            v2 := v;
            bit := false;
          end
          else begin
            break;
          end;
          r := r shl 1 + BOOL2INT[bit];
        end;
        if (v2 = -1) then begin
          v2 := v1;
        end;
        if (v2 < v1) then begin
          r := r xor $FF;
          i := v1;
          v1 := v2;
          v2 := i;
        end;
        w := v1 shl 4 + v2 + r shl 8;
      end
      else if (t[1] = '$') then begin
        w := Hex2Dec(copy(t, 2, length(t) - 1));
      end
      else begin
        w := StrToInt(t);
      end;
    except
      on E: EConvertError do ;
    end;
  end;
  Result := (w >= $0000) and (w <= $FFFF);
end;

procedure _writeWord(var seg: RSegment; var addr: integer; const w: word);
begin
  seg.Data[addr] := (w shr 8) and $FF;
  Dec(addr);
  seg.Data[addr] := w and $FF;
  Dec(addr);
end;

function DAI_TextToFrameBuffer(L: TStringList; out seg: RSegment; out msg: string): boolean;
var
  i, j: integer;
  x, y: integer;
  s: string;
  curPos: integer;
  sepPos: integer;
  t: string;
  r, m: integer;
  w, h, c: integer;
  cw: word;
  addr: integer;
  palette, Text, fill: boolean;
begin
  Result := False;
  msg := '';
  Segment_init(seg, $C000);
  addr := $BFFF;
  y := 0;
  for i := 0 to L.Count - 1 do begin
    s := L[i];
    if s = '' then begin
      continue;
    end;
    if (s[1] = ';') or (s[1] = '#') then begin
      continue;
    end;
    Inc(y);
    if (y > DAI_SCAN_LINES) then begin
      msg := 'Too much scanlines';
      exit;
    end;
    curPos := 1;
    t := _getToken(s, curPos);
    if (t = '') then begin
      msg := 'Wrong Resolution: ' + s;
      exit;
    end;
    sepPos := Pos('x', t);
    if (sepPos < 2) then begin
      msg := 'Wrong Resolution: ' + s;
      exit;
    end;
    w := StrToIntDef(Copy(t, 1, sepPos - 1), -1);
    h := StrToIntDef(Copy(t, sepPos + 1, length(t) - sepPos), -1);
    r := -1;
    for j := Low(RES_WIDTH) to High(RES_WIDTH) do begin
      if (w = RES_WIDTH[j]) then begin
        r := j;
        break;
      end;
    end;
    if (h < 1) or (h > 16) or (r = -1) then begin
      msg := 'Wrong Resolution: ' + s;
      exit;
    end;
    t := _getToken(s, curPos);
    if (t = '') or (Length(t) > 2) then begin
      msg := 'Wrong Command: ' + s;
      exit;
    end;
    c := Pos(t, 'FRGT');
    if (c = 0) then begin
      msg := 'Wrong Command: ' + s;
      exit;
    end;
    palette := length(t) = 2;
    fill := (c = 1) or (c = 2);
    Text := (c = 2) or (c = 4);
    m := BOOL2INT[not Text] * 2 + BOOL2INT[palette];
    cw := DAI_encodeControlWord(r, m, h - 1, False, fill, 0, 0);
    _writeWord(seg, addr, cw);
    case c of
      1: begin // F
        if not _getWord(s, curPos, w) then begin
          msg := 'Wrong Data: ' + s;
          exit;
        end;
        _writeWord(seg, addr, w);
      end;
      2: begin // R
        if not _getWord(s, curPos, w) then begin
          msg := 'Wrong Data: ' + s;
          exit;
        end;
        _writeWord(seg, addr, w);
      end;
      3: begin // G
        for x := 1 to RES_COLS[r] do begin
          if not _getWord(s, curPos, w) then begin
            msg := 'Wrong Data: ' + s;
            exit;
          end;
          _writeWord(seg, addr, w);
        end;
      end;
      else begin // T
        for x := 1 to RES_COLS[r] do begin
          if not _getWord(s, curPos, w) then begin
            msg := 'Wrong Data: ' + s;
            exit;
          end;
          _writeWord(seg, addr, w);
        end;
      end;
    end;
  end;
  Segment_slice(seg, addr, $BFFF);
  Result := True;
end;

function RGBToColor(const R, G, B: byte): TFPColor;
begin
  Result.Red := (R shl 8) + R;
  Result.Green := (G shl 8) + G;
  Result.Blue := (B shl 8) + B;
  Result.Alpha := $FFFF;
end;

procedure InitColor();
begin
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
end;

procedure InitColorReg();
begin
  DAI_COLORREG[0] := 0;
  DAI_COLORREG[1] := 0;
  DAI_COLORREG[2] := 0;
  DAI_COLORREG[3] := 0;
end;

procedure InitFont();
begin
  FillByte(FONT, sizeOf(FONT), 0);
end;

initialization
  DAI_PALETTE := TFPPalette.Create(16);
  InitColor();
  InitColorReg();
  InitFont();

finalization
  FreeAndNil(DAI_PALETTE);
end.
