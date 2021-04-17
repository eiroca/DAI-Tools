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
  Classes, SysUtils,
  Graphics;

const
  PAL_SCANLINES = 604;
  PAL_SCANLINES_VISIBLE = 528;

  DAI_SCREEN_WIDTH = 352;
  DAI_SCREEN_LINES = PAL_SCANLINES_VISIBLE div 2;

  DAI_IMAGE_WIDTH = DAI_SCREEN_WIDTH * 3;
  DAI_IMAGE_LINES = PAL_SCANLINES div 2 * 3;

  FONTCHAR_SIZE = 4 * 1024;

const
  RES_HEIGHT: array[0..3] of integer = (260, 260, 260, 240);
  RES_WIDTH: array[0..3] of integer = (88, 176, 352, 528);
  RES_COLS: array[0..3] of integer = (11, 22, 44, 66);
  RES_PXL: array[0..3] of integer = (4, 2, 1, 1);
  RES_DOT: array[0..3] of integer = (12, 6, 3, 2);

var
  //  Palette for 16 color gfx.
  DAI_PALETTE: array[0..15] of TColor;
  DAI_COLORREG: array[0..3] of byte;
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
function DAI_decodeControlWord(var seg: RSegment; var curAddr: integer): ControlWord;

implementation

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
  InitColor();
  InitFont();
end.
