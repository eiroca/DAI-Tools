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
unit libGraphFrame;

{$mode objfpc}{$H+}

interface

uses
  libTools, libGraph,
  Classes, SysUtils, FPImage;

function DAI_createFrame(var seg: RSegment; const C: TFPCustomImage): boolean;
function DAI_createFrameOpt(var seg: RSegment; const C: TFPCustomImage): boolean;

implementation

const
  ENABLE_CHANGE_val: array [boolean] of integer = (0, $80);
  UNIT_COLOR_val: array [boolean] of integer = ($40, 0);

type
  RColArr = array[0..15] of integer;
  RPxlArr = array[0..DAI_SCREEN_WIDTH - 1] of byte;

  RRowDef = record
    yp: integer;
    w: integer;
    h: integer;
    colCnt: integer;
    md: integer;
    ct: ColorTable;
    frq: RColArr;
    idx: RColArr;
    pxl: RPxlArr;
  end;

  RScreen = array[0..DAI_SCREEN_LINES - 1] of RRowDef;

function DAI_encodeControlWord(resolution: integer; mode: integer; repLines: integer; enable_change: boolean; unit_color: boolean; color_reg: integer; color_sel: integer): word;
begin
  Result := mode shl 14 + resolution shl 12 + repLines shl 8 + ENABLE_CHANGE_val[enable_change] + UNIT_COLOR_val[unit_color] + color_sel shl 4 + color_reg;
end;

procedure _findColorPair(freq: RColArr; out C1, C2: integer);
var
  idx: array[0..15] of integer;
  i: integer;
  newn, n: integer;
  e1, e2: integer;
begin
  for i := 0 to 15 do begin
    idx[i] := i;
  end;
  n := 15;
  repeat
    newn := 0;
    for i := 1 to n do begin
      e1 := idx[i - 1];
      e2 := idx[i];
      if freq[e1] > freq[e2] then begin
        idx[i - 1] := e2;
        idx[i] := e1;
        newn := i;
      end;
    end;
    n := newn;
  until (n < 14); //(n = 0);
  C1 := idx[15];
  C2 := idx[14];
  if (freq[c2] = 0) then begin // 1 color only
    c2 := c1;
  end;
end;

procedure RedGreenBlue(const col: TFPColor; var R, G, B: byte); inline;
begin
  R := col.Red shr 8;
  G := col.Green shr 8;
  B := col.Blue shr 8;
end;

function _findBestColor(const col: TFPColor; const pal: TFPPalette): integer;
var
  i, err: integer;
  daiCol: TFPColor;
  R1, G1, B1: byte;
  R2, G2, B2: byte;
  dR, dG, dB: integer;
  minErr: integer;
begin
  Result := 0;
  minErr := MaxInt;
  RedGreenBlue(col, R1, G1, B1);
  for i := 0 to pal.Count - 1 do begin
    daiCol := pal[i];
    RedGreenBlue(daiCol, R2, G2, B2);
    dR := (R1 - R2);
    dG := (G1 - G2);
    dB := (B1 - B2);
    err := dR * dR + dG * dG + dB * dB;
    if (err < minErr) then begin
      Result := i;
      minErr := err;
      if (err = 0) then begin
        break;
      end;
    end;
  end;
end;

function _compareRows(const row1, row2: RRowDef): boolean;
var
  i, s: integer;
begin
  s := DAI_SCREEN_WIDTH - 1;
  Result := True;
  for i := 0 to s do begin
    if row1.pxl[i] <> row2.pxl[i] then begin
      Result := False;
      break;
    end;
  end;
end;

procedure _sortColors(freq: RColArr; out idx: RColArr);
var
  i: integer;
  newn, n: integer;
  e1, e2: integer;
begin
  for i := 0 to 15 do begin
    idx[i] := i;
  end;
  n := 15;
  repeat
    newn := 0;
    for i := 1 to n do begin
      e1 := idx[i - 1];
      e2 := idx[i];
      if freq[e1] < freq[e2] then begin
        idx[i - 1] := e2;
        idx[i] := e1;
        newn := i;
      end;
    end;
    n := newn;
  until (n = 0);
end;

procedure _emitFooter(var seg: RSegment; var addr: integer);
var
  y: integer;
begin
  // Append some data for invisible line 3 black bar of 96 scan lines
  for y := 1 to 3 do begin
    seg.Data[addr] := $AF;
    Dec(addr);
    seg.Data[addr] := $00;
    Dec(addr);
    seg.Data[addr] := $00;
    Dec(addr);
    seg.Data[addr] := $00;
    Dec(addr);
  end;
end;

procedure _emitFill(var seg: RSegment; var addr: integer; const h: integer; const lin: RRowDef);
var
  CW: word;
  c16: integer;
begin
  CW := DAI_encodeControlWord(2, 2, (h - 1), False, True, 0, 0);
  c16 := lin.idx[0];
  seg.Data[addr] := (CW shr 8) and $FF;
  Dec(addr);
  seg.Data[addr] := CW and $FF;
  Dec(addr);
  seg.Data[addr] := 0;
  Dec(addr);
  seg.Data[addr] := c16 shl 4 + c16;
  Dec(addr);
end;

procedure _emitGraph16(var seg: RSegment; var addr: integer; const h: integer; const lin: RRowDef);
var
  xc, x: integer;
  posX: integer;
  blkCol: RColArr;
  col: TFPColor;
  tC, C1, C2, c16: integer;
  pal: TFPPalette;
  CW: word;
  v, m: integer;
begin
  pal := TFPPalette.Create(2);
  posX := 0;
  CW := DAI_encodeControlWord(2, 2, (h - 1), False, False, 0, 0);
  seg.Data[addr] := (CW shr 8) and $FF;
  Dec(addr);
  seg.Data[addr] := CW and $FF;
  Dec(addr);
  blkCol[0] := 0;
  for xc := 0 to 44 - 1 do begin
    FillByte(blkCol, SizeOf(blkCol), 0);
    for x := 0 to 7 do begin
      c16 := lin.pxl[PosX];
      Inc(blkCol[c16]);
      Inc(posX);
    end;
    _findColorPair(blkCol, C1, C2);
    if (C1 > C2) then begin
      tC := C1;
      C1 := C2;
      C2 := tC;
    end;
    pal[0] := DAI_PALETTE[C1];
    pal[1] := DAI_PALETTE[C2];
    v := 0;
    if (C1 <> C2) then begin
      m := $80;
      Dec(posX, 8);
      for x := 0 to 7 do begin
        col := DAI_PALETTE[lin.pxl[PosX]];
        if _findBestColor(col, pal) = 0 then begin
          v := v or m;
        end;
        m := m shr 1;
        Inc(posX);
      end;
    end;
    seg.Data[addr] := v;
    Dec(addr);
    seg.Data[addr] := C1 shl 4 + C2;
    Dec(addr);
  end;
  FreeAndNil(pal);
end;

function DAI_createFrameOpt(var seg: RSegment; const C: TFPCustomImage): boolean;
var
  i, x, y: integer;
  curLin, nxtLin: integer;
  col: TFPColor;
  nc, ht, hh, c16: integer;
  addr: integer;
  Screen: RScreen;
  cct: ColorTable;
begin
  Segment_resize(seg, $C000);
  Result := False;
  // Step 1 -> convert to 16 col
  for i := 0 to 3 do begin
    cct[i] := -1;
  end;
  for y := 0 to DAI_SCREEN_LINES - 1 do begin
    with Screen[y] do begin
      yp := y;
      w := DAI_SCREEN_WIDTH;
      h := 1;
      colCnt := 0;
      md := 0;
      ct := cct;
      FillByte(frq, SizeOf(frq), 0);
      for x := 0 to DAI_SCREEN_WIDTH - 1 do begin
        col := C.Colors[x, y];
        c16 := _findBestColor(col, DAI_PALETTE);
        pxl[x] := c16;
        Inc(frq[c16]);
        if (frq[c16] = 1) then begin
          Inc(colCnt);
        end;
      end;
      _sortColors(frq, idx);
    end;
  end;
  // Step 2 -> merge equal row
  curLin := 0;
  nxtLin := 1;
  while (nxtLin < (DAI_SCREEN_LINES - 1)) do begin
    if _compareRows(Screen[curLin], Screen[nxtLin]) then begin
      Inc(Screen[curLin].h, Screen[nxtLin].h);
      Screen[nxtLin].h := 0;
    end
    else begin
      curLin := nxtLin;
    end;
    Inc(nxtLin);
  end;
  // Step 3 -> generate frame buffer
  addr := $BFFF;
  curLin := 0;
  while (curLin < (DAI_SCREEN_LINES - 1)) do begin
    ht := screen[curLin].h;
    nc := screen[curLin].colCnt;
    while (ht > 0) do begin
      hh := ht;
      if (hh > 16) then begin
        hh := 16;
      end;
      if (nc = 1) then begin
        _emitFill(seg, addr, hh, screen[curLin]);
      end
      else begin
        _emitGraph16(seg, addr, hh, screen[curLin]);
      end;
      Dec(ht, hh);
    end;
    Inc(curLin);
  end;
  _emitFooter(seg, addr);
  Segment_slice(seg, addr + 1, $BFFF);
  Result := True;
end;

function DAI_createFrame(var seg: RSegment; const C: TFPCustomImage): boolean;
var
  xc, x, y: integer;
  posX, PosY: integer;
  blkCol: RColArr;
  col: TFPColor;
  tC, C1, C2, c16: integer;
  pal: TFPPalette;
  CW: word;
  addr: integer;
  v, m: integer;
begin
  Segment_resize(seg, $C000);
  Result := False;
  posY := 0;
  posX := 0;
  blkCol[0] := 0;
  addr := $BFFF;
  pal := TFPPalette.Create(2);
  for y := 0 to DAI_SCREEN_LINES - 1 do begin
    posX := 0;
    CW := DAI_encodeControlWord(2, 2, 0, False, False, 0, 0);
    seg.Data[addr] := (CW shr 8) and $FF;
    Dec(addr);
    seg.Data[addr] := CW and $FF;
    Dec(addr);
    for xc := 0 to 44 - 1 do begin
      FillByte(blkCol, SizeOf(blkCol), 0);
      for x := 0 to 7 do begin
        col := C.Colors[posX, PosY];
        c16 := _findBestColor(col, DAI_PALETTE);
        Inc(blkCol[c16]);
        Inc(posX);
      end;
      _findColorPair(blkCol, C1, C2);
      if (C1 > C2) then begin
        tC := C1;
        C1 := C2;
        C2 := tC;
      end;
      pal[0] := DAI_PALETTE[C1];
      pal[1] := DAI_PALETTE[C2];
      v := 0;
      if (C1 <> C2) then begin
        m := $80;
        Dec(posX, 8);
        for x := 0 to 7 do begin
          col := C.Colors[posX, PosY];
          if _findBestColor(col, pal) = 0 then begin
            v := v or m;
          end;
          m := m shr 1;
          Inc(posX);
        end;
      end;
      seg.Data[addr] := v;
      Dec(addr);
      seg.Data[addr] := C1 shl 4 + C2;
      Dec(addr);
    end;
    Inc(posY);
  end;
  _emitFooter(seg, addr);
  Segment_slice(seg, addr + 1, $BFFF);
  FreeAndNil(pal);
  Result := True;
end;

end.
