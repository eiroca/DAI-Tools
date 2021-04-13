procedure _fastDrawBlockGraph4(data1, data2: integer; i, curScanLine, xl, yl: integer; C: TCanvas); inline;
var
  j, k: integer;
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
      C.Pixels[posX, posY] := col;
      Inc(posX);
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
  j, k: integer;
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
      C.Pixels[posX, posY] := col;
      Inc(posX);
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
    posX := (i * 8) * xl;
    for k := 0 to 7 do begin
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
      C.Pixels[posX, posY] := col;
      Inc(posX);
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

procedure _fastFillColor4(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas); inline;
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

procedure _fastFillText4(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas); inline;
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

procedure _fastDecodeGraph4(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas); inline;
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

procedure _fastDecodeText4(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas); inline;
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

procedure _fastFillColor16(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas); inline;
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
    end
    else begin
      _fastDrawBlockGraph16_528(data1, data2, i, curScanLine, xl, yl, C);
    end;
  end;
end;

procedure _fastFillText16(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas); inline;
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

procedure _fastDecodeGraph16(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas); inline;
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

procedure _fastDecodeText16(var seg: RSegment; var curAddr: integer; curScanLine, xc, xl, yl: integer; rescale: boolean; C: TCanvas); inline;
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

