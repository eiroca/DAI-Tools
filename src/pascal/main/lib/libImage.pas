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
unit libImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  libTools, libGraph,
  FPImage, FPReadPNG, FPWritePNG, FPDitherer, FPQuantizer, FPCanvas, FPImgCanv;

function prepareImage(const inPath: string; quCol: integer; dither: boolean): TFPCustomImage;

implementation

procedure Swap(var s, d: TFPCustomImage); inline;
var
  t: TFPCustomImage;
begin
  t := s;
  s := d;
  d := t;
end;

function prepareImage(const inPath: string; quCol: integer; dither: boolean): TFPCustomImage;
var
  image1: TFPCustomImage;
  image2: TFPCustomImage;
  reader: TFPCustomImageReader;
  canvas: TFPCustomCanvas;
  dither1: TFPFloydSteinbergDitherer;
  dither2: TFPFloydSteinbergDitherer;
  quantizer: TFPOctreeQuantizer;
  palette: TFPPalette;
  srcI, dstI: TFPCustomImage;
  change: boolean;
begin
  Result := nil;
  image1 := nil;
  image2 := nil;
  reader := nil;
  dither1 := nil;
  dither2 := nil;
  quantizer := nil;
  palette := nil;
  canvas := nil;
  change := False;
  try
    // Step 1 - Read image
    image2 := TFPMemoryImage.Create(DAI_SCREEN_WIDTH, DAI_SCREEN_LINES);
    image1 := TFPMemoryImage.Create(0, 0);
    reader := TFPReaderPNG.Create;
    image1.LoadFromFile(inPath, reader);
    srcI := image1;
    dstI := image2;
    // Step 2 - Resize the image
    if (image1.Width <> DAI_SCREEN_WIDTH) or (image1.Height <> DAI_SCREEN_LINES) then begin
      canvas := TFPImageCanvas.Create(image2);
      canvas.StretchDraw(0, 0, DAI_SCREEN_WIDTH, DAI_SCREEN_LINES, image1);
      srcI := image2;
      dstI := image1;
      change := True;
    end;
    // Step 3 - Color quantization
    if (quCol > 0) then begin
      quantizer := TFPOctreeQuantizer.Create();
      quantizer.ColorNumber := quCol;
      quantizer.Add(image1);
      palette := quantizer.Quantize();
      dither1 := TFPFloydSteinbergDitherer.Create(palette);
      dither1.UseAlpha := False;
      dither1.Dither(srcI, dstI);
      Swap(srcI, dstI);
      change := True;
    end;
    // Step 4 - Dithering
    if (dither) then begin
      dither2 := TFPFloydSteinbergDitherer.Create(DAI_PALETTE);
      dither2.UseAlpha := False;
      dither2.Dither(srcI, dstI);
      Swap(srcI, dstI);
      change := True;
    end;
    if (not change) or (dstI = image1) then begin
      Result := image1;
      image1 := nil;
    end
    else begin
      Result := image2;
      image2 := nil;
    end;
  finally
    if (reader <> nil) then begin
      FreeAndNil(reader);
    end;
    if (image1 <> nil) then begin
      FreeAndNil(image1);
    end;
    if (image2 <> nil) then begin
      FreeAndNil(image2);
    end;
    if (canvas <> nil) then begin
      FreeAndNil(canvas);
    end;
    if (palette <> nil) then begin
      FreeAndNil(palette);
    end;
    if (quantizer <> nil) then begin
      FreeAndNil(quantizer);
    end;
    if (dither1 <> nil) then begin
      FreeAndNil(dither1);
    end;
    if (dither2 <> nil) then begin
      FreeAndNil(dither2);
    end;
  end;
end;

end.
