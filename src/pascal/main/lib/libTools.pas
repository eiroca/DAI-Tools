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
unit libTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

const
  MAX_ADDR = 64 * 1024 - 1;

const
  HEXNUM: TSysCharSet = ['0'..'9', 'A'..'F'];

type
  TSegment = record
    addr: uint16;
    len: uint16;
    entrypoint: uint16;
    segType: uint16;
    Data: array of byte;
  end;

procedure Segment_init(var seg: TSegment; const size: integer = (MAX_ADDR + 1));

implementation

procedure Segment_init(var seg: TSegment; const size: integer);
begin
  with seg do begin
    addr := 0;
    len := size;
    entrypoint := 0;
    segType := 0;
    SetLength(Data, size);
  end;
end;

end.
