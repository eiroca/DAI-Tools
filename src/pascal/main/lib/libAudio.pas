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
unit libAudio;

{$mode objfpc}{$H+}

interface

uses
  Classes, bufstream;

type
  FCS = array[1..4] of char;

const
  HEADER_RIFF: FCS = 'RIFF';
  HEADER_WAVE: FCS = 'WAVE';
  HEADER_FRMT: FCS = 'fmt ';
  HEADER_DATA: FCS = 'data';

  WAVE_PCM: uint16 = 1;

type
  RRIFFHeader = record
    RIFF: FCS;      // RIFF marker
    size: uint32;   // file-size (equals file-size - 8) (4 bytes)
    WAVE: FCS;      // Mark it as type "WAVE"
  end;

  RWAVEfmt = record
    Name: FCS;               // mark the format section
    size: uint32;            // length of format data. Allways 16 (4 bytes)
    wFormatTag: uint16;      // format type
    nChannels: uint16;       // number of channels (i.e. mono, stereo...)
    nSamplesPerSec: uint32;  // sample rate
    nAvgBytesPerSec: uint32; // for buffer estimation
    nBlockAlign: uint16;     // block size of data
    wBitsPerSample: uint16;  // bits per sample
  end;

  RWAVEdata = record
    Name: FCS;     // mark the data section
    size: uint32;  // length of data
  end;

const
  BIT2SAMPLE: array[boolean] of word = ($8000, $7FFF);

type
  RAudio = record
    len: integer;
    durTot: integer;
    bit: array of boolean;
    dur: array of integer;
  end;

procedure Audio_Init(var w: RAudio);
procedure Audio_Trunc(var w: RAudio);
procedure Audio_Grow(var w: RAudio; s: integer);
procedure Audio_Append(var w: RAudio; v: boolean; d: integer);
function Audio_Save(var w: RAudio; const outPath: string): boolean;
function Audio_Load(const inPath: string; var w: RAudio): boolean;

implementation

function FCS2int(s: string): uint32; inline;
begin
  Result := Ord(s[1]) + Ord(s[2]) * 256 + Ord(s[3]) * 256 * 256 + Ord(s[4]) * 256 * 256 * 256;
end;

procedure Audio_Init(var w: RAudio);
begin
  with w do begin
    len := 0;
    durTot := 0;
    setLength(bit, 0);
    setLength(dur, 0);
  end;
end;

procedure Audio_Trunc(var w: RAudio);
begin
  with w do begin
    setLength(bit, len);
    setLength(dur, len);
  end;
end;

procedure Audio_Grow(var w: RAudio; s: integer);
var
  newSize: integer;
begin
  with w do begin
    newSize := (len + s);
    if (newSize > Length(bit)) then begin
      newSize := (len + s + 4095) and $FFFFF000;
      setLength(bit, newSize);
      setLength(dur, newSize);
    end;
  end;
end;

procedure Audio_Append(var w: RAudio; v: boolean; d: integer);
var
  p: integer;
begin
  with w do begin
    p := len - 1;
    if (p < 0) or (bit[p] <> v) then begin
      Audio_Grow(w, 1);
      bit[len] := v;
      dur[len] := d;
      Inc(len);
    end
    else begin
      Inc(dur[p], d);
    end;
    Inc(durTot, d);
  end;
end;

function Audio_Save(var w: RAudio; const outPath: string): boolean;
var
  fs: TBufferedFileStream;
  i, j: integer;
  hdr: RRIFFHeader;
  fmt: RWAVEfmt;
  dat: RWAVEdata;
begin
  Result := False;
  fs := TBufferedFileStream.Create(outPath, fmCreate);
  try
    hdr.RIFF := HEADER_RIFF;
    hdr.size := 12 + 24 + 8 + w.durTot * 2;
    hdr.WAVE := HEADER_WAVE;
    fmt.Name := HEADER_FRMT;
    fmt.size := 16;
    fmt.wFormatTag := WAVE_PCM;
    fmt.nChannels := 1;
    fmt.nSamplesPerSec := 44100; // kHz Sample Rate
    fmt.nAvgBytesPerSec := 44100 * 2 * 1; // (Sample Rate * Bit Size * Channels) / 8
    fmt.nBlockAlign := 2;
    fmt.wBitsPerSample := 16;
    dat.Name := HEADER_DATA;
    dat.size := w.durTot * 2;
    fs.Write(hdr, sizeof(hdr));
    fs.Write(fmt, sizeof(fmt));
    fs.Write(dat, sizeof(dat));
    for i := 0 to w.len - 1 do begin
      with w do begin
        for j := 0 to dur[i] - 1 do begin
          fs.WriteWord(BIT2SAMPLE[bit[i]]);
        end;
      end;
    end;
    Result := True;
  finally
    fs.Free;
  end;
end;

function Audio_Load(const inPath: string; var w: RAudio): boolean;
var
  fs: TBufferedFileStream;
  i: integer;
  hdr: RRIFFHeader;
  fmt: RWAVEfmt;
  dat: RWAVEdata;
  cnt: integer;
  v: int16;
begin
  Result := False;
  Audio_Init(w);
  hdr.size := 0;
  fmt.size := 0;
  dat.size := 0;
  fs := TBufferedFileStream.Create(inPath, fmOpenRead);
  try
    while True do begin
      if (fs.Size < 44) then begin
        break;
      end;
      if fs.Read(hdr, SizeOf(hdr)) <> SizeOf(hdr) then begin
        break;
      end;
      if fs.Read(fmt, SizeOf(fmt)) <> SizeOf(fmt) then begin
        break;
      end;
      if fs.Read(dat, SizeOf(dat)) <> SizeOf(dat) then begin
        break;
      end;
      if (hdr.RIFF <> HEADER_RIFF) then begin
        break;
      end;
      if (hdr.WAVE <> HEADER_WAVE) then begin
        break;
      end;
      if fmt.Name <> HEADER_FRMT then begin
        break;
      end;
      if fmt.size <> 16 then begin
        break;
      end;
      if fmt.wFormatTag <> WAVE_PCM then begin
        break;
      end;
      if fmt.nChannels <> 1 then begin
        break;
      end;
      if fmt.nSamplesPerSec <> 44100 then begin
        break;
      end;
      if fmt.nAvgBytesPerSec <> 44100 * 2 * 1 then begin
        break;
      end;
      if fmt.nBlockAlign <> 2 then begin
        break;
      end;
      if fmt.wBitsPerSample <> 16 then begin
        break;
      end;
      if dat.Name <> HEADER_DATA then begin
        break;
      end;
      cnt := dat.size div 2;
      for i := 0 to cnt - 1 do begin
        v := int16(fs.ReadWord());
        Audio_Append(w, (v >= 0), 1);
      end;
      Result := True;
      break;
    end;
  finally
    fs.Free;
  end;
end;

end.
