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
unit uFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libDAI;

type
  PFilter = ^RFilter;

  RFilter = record
    ext: string;
    displayName: string;
    proc: DAI_func;
  end;

var
  FONT_PATHNAME: array of string = ('DAI\ROMS\nch.bin', 'DAI\nch.bin', 'nch.bin');


function FindLoadFilter(const idx: integer): PFilter;
function FindSaveFilter(const idx: integer): PFilter;

function FindLoadFilter(const Name: string): PFilter;
function FindSaveFilter(const Name: string): PFilter;

function IndexOfLoadFilter(const Name: string): integer;
function IndexOfSaveFilter(const Name: string): integer;

procedure ListLoadFilters(const S: TStrings);
procedure ListSaveFilters(const S: TStrings);


var
  LOAD_FILTERS: array of RFilter;
  SAVE_FILTERS: array of RFilter;

implementation

function NewFilter(ext: string; displayName: string; proc: DAI_func): RFilter; inline;
begin
  Result.ext := ext;
  Result.displayName := displayName;
  Result.proc := proc;
end;

function _FindFilter(const idx: integer; const FILTERS: array of RFilter): PFilter;
begin
  if (idx >= Low(FILTERS)) and (idx <= High(FILTERS)) then begin
    Result := @FILTERS[idx];
  end
  else begin
    Result := nil;
  end;
end;

procedure _ListFilters(const S: TStrings; const FILTERS: array of RFilter);
var
  i: integer;
begin
  S.Clear;
  for i := 0 to High(FILTERS) do begin
    S.Add(FILTERS[i].displayName);
  end;
end;

function _IndexOfFilter(Name: string; const FILTERS: array of RFilter): integer;
var
  i: integer;
begin
  Result := -1;
  if (Name = '') then begin
    exit;
  end;
  Name := LowerCase(Name);
  Name := StringReplace(Name, '_', ' ', [rfReplaceAll]);
  for i := 0 to High(FILTERS) do begin
    with FILTERS[i] do begin
      if (CompareText(displayName, Name) = 0) then begin
        Result := i;
        exit;
      end;
    end;
  end;
  Result := StrToIntDef(Name, -1);
end;

function FindLoadFilter(const idx: integer): PFilter;
begin
  Result := _FindFilter(idx, LOAD_FILTERS);
end;

function FindSaveFilter(const idx: integer): PFilter;
begin
  Result := _FindFilter(idx, SAVE_FILTERS);
end;

function FindLoadFilter(const Name: string): PFilter;
begin
  Result := _FindFilter(_IndexOfFilter(Name, LOAD_FILTERS), LOAD_FILTERS);
end;

function FindSaveFilter(const Name: string): PFilter;
begin
  Result := _FindFilter(_IndexOfFilter(Name, SAVE_FILTERS), SAVE_FILTERS);
end;

function IndexOfLoadFilter(const Name: string): integer;
begin
  Result := _IndexOfFilter(Name, LOAD_FILTERS);
end;

function IndexOfSaveFilter(const Name: string): integer;
begin
  Result := _IndexOfFilter(Name, SAVE_FILTERS);
end;

procedure ListLoadFilters(const S: TStrings);
begin
  _ListFilters(S, LOAD_FILTERS);
end;

procedure ListSaveFilters(const S: TStrings);
begin
  _ListFilters(S, SAVE_FILTERS);
end;

initialization
  LOAD_FILTERS := [//
    newFilter('bin', 'BIN', @DAI_loadBin),//
    newFilter('sbin', 'SBIN', @DAI_loadSBin),//
    newFilter('dump', 'dump', @DAI_loadDump),//
    newFilter('dai', 'DAI', @DAI_loadDAI),//
    newFilter('wav', 'WAV', @DAI_loadWAV),//
    newFilter('hrfb', 'HRFB', @DAI_loadHRFB),//
    newFilter('png', 'PNG', @DAI_loadPNGOpt),//
    newFilter('png', 'PNG (big)', @DAI_loadPNG)//
    ];
  SAVE_FILTERS := [//
    newFilter('bin', 'BIN', @DAI_saveBin),//
    newFilter('sbin', 'SBIN', @DAI_saveSBin),//
    newFilter('dump', 'dump', @DAI_saveDump),//
    newFilter('asm', 'asm', @DAI_saveASM),//
    newFilter('png', 'PNG (full)', @DAI_saveFullPNG),//
    newFilter('png', 'PNG (fast)', @DAI_savePNG),//
    newFilter('hrfb', 'HRFB', @DAI_saveHRFB),//
    newFilter('dai', 'DAI (bin)', @DAI_saveDAIbin),//
    newFilter('dai', 'DAI (basic)', @DAI_saveDAIbas),//
    newFilter('bas', 'BAS', @DAI_saveBAS),//
    newFilter('wav', 'WAV', @DAI_saveWAV)//
    ];
end.
