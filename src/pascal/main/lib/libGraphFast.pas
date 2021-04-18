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
unit libGraphFast;

{$mode objfpc}{$H+}

interface

uses
  libGraph, libTools,
  Classes, SysUtils,
  Graphics;

function DAI_decodeFrameBuffer(var seg: RSegment; curAddr: integer; C: TCanvas): boolean;

implementation

procedure _fastDrawBlockGraph4(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k, s: integer;
  col: TColor;
  colIdx: integer;
  posX, posY: integer;
begin
  posY := curScanLine;
  for j := 0 to yl - 1 do begin
    posX := (i * 8) * xl;
    for k := 0 to 7 do begin
      colIdx := (((data1 shr (7 - k)) and $01) shl 1) or ((data2 shr (7 - k)) and $01);
      col := DAI_PALETTE[DAI_COLORREG[colIdx]];
      for s := 0 to xl - 1 do begin
        C.Pixels[posX, posY] := col;
        Inc(posX);
      end;
    end;
    Inc(posY);
  end;
end;

// Skip pixel 4 and 8 to resize to 352 pixel width
procedure _fastDrawBlockGraph4_528(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k: integer;
  col: TColor;
  colIdx: integer;
  posX, posY: integer;
begin
  posY := curScanLine;
  for j := 0 to yl - 1 do begin
    posX := (i * 6) * xl;
    for k := 0 to 6 do begin
      if (k = 4) then begin
        continue;
      end;
      colIdx := (((data1 shr (7 - k)) and $01) shl 1) or ((data2 shr (7 - k)) and $01);
      col := DAI_PALETTE[DAI_COLORREG[colIdx]];
      C.Pixels[posX, posY] := col;
      Inc(posX);
    end;
    Inc(posY);
  end;
end;

procedure _fastDrawBlockText4(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k, s: integer;
  col: TColor;
  charData, colIdx: integer;
  posX, posY, posC: integer;
begin
  posC := data1 * 16;
  posY := curScanLine;
  for j := 0 to yl - 1 do begin
    posX := (i * 8) * xl;
    charData := FONT[posC];
    Inc(posC);
    for k := 0 to 7 do begin
      colIdx := ((charData shr k) and $01) or (((data2 shr k) and $01) shl 1);
      col := DAI_PALETTE[DAI_COLORREG[colIdx]];
      for s := 0 to xl - 1 do begin
        C.Pixels[posX, posY] := col;
        Inc(posX);
      end;
    end;
    Inc(posY);
  end;
end;

procedure _fastDrawBlockText4_528(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k: integer;
  col: TColor;
  charData, colIdx: integer;
  posX, posY, posC: integer;
begin
  posC := data1 * 16;
  posY := curScanLine;
  for j := 0 to yl - 1 do begin
    posX := (i * 6) * xl;
    charData := FONT[posC];
    Inc(posC);
    for k := 2 to 7 do begin
      colIdx := ((charData shr k) and $01) or (((data2 shr k) and $01) shl 1);
      col := DAI_PALETTE[DAI_COLORREG[colIdx]];
      C.Pixels[posX, posY] := col;
      Inc(posX);
    end;
    Inc(posY);
  end;
end;

procedure _fastDrawBlockGraph16(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k, s: integer;
  col: TColor;
  colIdx: integer;
  posX, posY: integer;
  c1, c2: integer;
begin
  posY := curScanLine;
  c1 := data2 and $0F;
  c2 := (data2 shr 4) and $0F;
  for j := 0 to yl - 1 do begin
    posX := (i * 8) * xl;
    for k := 0 to 7 do begin
      if ((data1 shr (7 - k)) and $01) <> 0 then begin
        colIdx := c2;
      end
      else begin
        colIdx := c1;
      end;
      col := DAI_PALETTE[colIdx];
      for s := 0 to xl - 1 do begin
        C.Pixels[posX, posY] := col;
        Inc(posX);

      end;
    end;
    Inc(posY);
  end;
end;

procedure _fastDrawBlockGraph16_528(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k: integer;
  col: TColor;
  colIdx: integer;
  posX, posY: integer;
  c1, c2: integer;
begin
  posY := curScanLine;
  c1 := data2 and $0F;
  c2 := (data2 shr 4) and $0F;
  for j := 0 to yl - 1 do begin
    posX := (i * 6) * xl;
    for k := 0 to 6 do begin
      if (k = 4) then begin
        continue;
      end;
      if ((data1 shr (7 - k)) and $01) <> 0 then begin
        colIdx := c2;
      end
      else begin
        colIdx := c1;
      end;
      col := DAI_PALETTE[colIdx];
      C.Pixels[posX, posY] := col;
      Inc(posX);
    end;
    Inc(posY);
  end;
end;


procedure _fastDrawBlockText16(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k, s: integer;
  col: TColor;
  charData, colIdx: integer;
  posX, posY, posC: integer;
  c1, c2: integer;
begin
  posY := curScanLine;
  posC := data1 * 16;
  c1 := data2 and $0F;
  c2 := (data2 shr 4) and $0F;
  for j := 0 to yl - 1 do begin
    posX := (i * 8) * xl;
    charData := FONT[posC];
    if (j mod 3) = 2 then begin
      Inc(posC);
    end;
    for k := 0 to 7 do begin
      if ((charData shr k) and $01) <> 0 then begin
        colIdx := c2;
      end
      else begin
        colIdx := c1;
      end;
      col := DAI_PALETTE[colIdx];
      for s := 0 to xl - 1 do begin
        C.Pixels[posX, posY] := col;
        Inc(posX);
      end;
    end;
    Inc(posY);
  end;
end;

procedure _fastDrawBlockText16_528(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k: integer;
  col: TColor;
  charData, colIdx: integer;
  posX, posY, posC: integer;
  c1, c2: integer;
begin
  posY := curScanLine;
  posC := data1 * 16;
  c1 := data2 and $0F;
  c2 := (data2 shr 4) and $0F;
  for j := 0 to yl - 1 do begin
    posX := (i * 6) * xl;
    charData := FONT[posC];
    if (j mod 3) = 2 then begin
      Inc(posC);
    end;
    for k := 2 to 7 do begin
      if ((charData shr k) and $01) <> 0 then begin
        colIdx := c2;
      end
      else begin
        colIdx := c1;
      end;
      col := DAI_PALETTE[colIdx];
      C.Pixels[posX, posY] := col;
      Inc(posX);
    end;
    Inc(posY);
  end;
end;

procedure _fastFillColor4(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  data1 := seg.Data[curAddr];
  Dec(curAddr);
  data2 := seg.Data[curAddr];
  Dec(curAddr);
  for  i := 0 to xc - 1 do begin
    if rescale then begin
      _fastDrawBlockGraph4_528(data1, data2, i, curScanLine, xl, yl, C);
    end
    else begin
      _fastDrawBlockGraph4(data1, data2, i, curScanLine, xl, yl, C);
    end;
  end;
end;

procedure _fastFillText4(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  data1 := seg.Data[curAddr];
  Dec(curAddr);
  data2 := seg.Data[curAddr];
  Dec(curAddr);
  for  i := 0 to xc - 1 do begin
    if rescale then begin
      _fastDrawBlockText4_528(data1, data2, i, curScanLine, xl, yl, C);
    end
    else begin
      _fastDrawBlockText4(data1, data2, i, curScanLine, xl, yl, C);
    end;
  end;
end;

procedure _fastDecodeGraph4(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  for  i := 0 to xc - 1 do begin
    data1 := seg.Data[curAddr];
    Dec(curAddr);
    data2 := seg.Data[curAddr];
    Dec(curAddr);
    if rescale then begin
      _fastDrawBlockGraph4_528(data1, data2, i, curScanLine, xl, yl, C);
    end
    else begin
      _fastDrawBlockGraph4(data1, data2, i, curScanLine, xl, yl, C);
    end;
  end;
end;

procedure _fastDecodeText4(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  for  i := 0 to xc - 1 do begin
    data1 := seg.Data[curAddr];
    Dec(curAddr);
    data2 := seg.Data[curAddr];
    Dec(curAddr);
    if rescale then begin
      _fastDrawBlockText4_528(data1, data2, i, curScanLine, xl, yl, C);
    end
    else begin
      _fastDrawBlockText4(data1, data2, i, curScanLine, xl, yl, C);
    end;
  end;
end;

procedure _fastFillColor16(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  data1 := seg.Data[curAddr];
  Dec(curAddr);
  data2 := seg.Data[curAddr];
  Dec(curAddr);
  for  i := 0 to xc - 1 do begin
    if rescale then begin
      _fastDrawBlockGraph16_528(data1, data2, i, curScanLine, xl, yl, C);
    end
    else begin
      _fastDrawBlockGraph16(data1, data2, i, curScanLine, xl, yl, C);
    end;
  end;
end;

procedure _fastFillText16(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  data1 := seg.Data[curAddr];
  Dec(curAddr);
  data2 := seg.Data[curAddr];
  Dec(curAddr);
  for  i := 0 to xc - 1 do begin
    if rescale then begin
      _fastDrawBlockText16_528(data1, data2, i, curScanLine, xl, yl, C);
    end
    else begin
      _fastDrawBlockText16(data1, data2, i, curScanLine, xl, yl, C);
    end;
  end;
end;

procedure _fastDecodeGraph16(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  for  i := 0 to xc - 1 do begin
    data1 := seg.Data[curAddr];
    Dec(curAddr);
    data2 := seg.Data[curAddr];
    Dec(curAddr);
    if rescale then begin
      _fastDrawBlockGraph16_528(data1, data2, i, curScanLine, xl, yl, C);
    end
    else begin
      _fastDrawBlockGraph16(data1, data2, i, curScanLine, xl, yl, C);
    end;
  end;
end;

procedure _fastDecodeText16(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas);
var
  data1, data2: integer;
  i: integer;
begin
  for  i := 0 to xc - 1 do begin
    data1 := seg.Data[curAddr];
    Dec(curAddr);
    data2 := seg.Data[curAddr];
    Dec(curAddr);
    if rescale then begin
      _fastDrawBlockText16_528(data1, data2, i, curScanLine, xl, yl, C);
    end
    else begin
      _fastDrawBlockText16(data1, data2, i, curScanLine, xl, yl, C);
    end;
  end;
end;

function DAI_decodeFrameBuffer(var seg: RSegment; curAddr: integer; C: TCanvas): boolean;
var
  curLin: integer;
  CW: ControlWord;
  rows: integer;
  rescale: boolean;
begin
  Result := False;
  curLin := 0;
  rows := DAI_SCREEN_LINES;
  while (curLin < rows) do begin
    if (curAddr < 1) then begin
      exit;
    end;
    CW := DAI_decodeControlWord(seg, curAddr);
    if (curAddr < (CW.data_size - 1)) then begin
      exit;
    end;
    rescale := CW.line_width = 528;
    if CW.unit_color then begin
      case CW.mode of
        %00: begin
          _fastFillColor4(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, rescale, C);
        end;
        %01: begin
          _fastFillText4(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, rescale, C);
        end;
        %10: begin
          _fastFillColor16(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, rescale, C);
        end;
        else begin
          _fastFillText16(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, rescale, C);
        end;
      end;
    end
    else begin
      case CW.mode of
        %00: begin
          _fastDecodeGraph4(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, rescale, C);
        end;
        %01: begin
          _fastDecodeText4(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, rescale, C);
        end;
        %10: begin
          _fastDecodeGraph16(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, rescale, C);
        end;
        else begin
          _fastDecodeText16(seg, curAddr, curLin, CW.line_colCnt, CW.line_pxlWdt, CW.line_pxlHei, rescale, C);
        end;
      end;
    end;
    Inc(curLin, CW.line_pxlHei);
  end;
  Result := True;
end;

end.

