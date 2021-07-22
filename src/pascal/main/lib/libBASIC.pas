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
unit libBASIC;

{$mode objfpc}{$H+}

interface

uses
  libTools,
  Classes, Math, SysUtils;

const
  BASIC_TAPEHEADER = $30;

const
  R_OK = 0;
  R_INVALIDPROG = 1; // Invalid Program
  R_INVALIDLINE = 2; // Invalid Line

function BASIC_decode(const codeSeg, dataSeg: RSegment; var src: TStrings): integer;
function BASIC_encode(const src: TStrings; out codeSeg, dataSeg: RSegment): integer;

implementation

{$include basic_formats.inc}

type

  PVariableVal = ^VariableVal;
  VariableVal = record
    case integer of
      0: (b: array[0..3] of byte);
      1: (i: Int32);
      2: (s: single);
  end;

  PVariableRef = ^VariableRef;

  VariableRef = record
    offset: integer;
    typ: integer;
    Name: string;
    Count: integer;
    hasVal: boolean;
    val: VariableVal;
    ref: integer;
  end;

  PLineRef = ^LineRef;

  LineRef = record
    lineNR: integer;
    addr: integer;
  end;

  PBasicContext = ^RBasicContext;

  RBasicContext = record
    codePos: integer;
    codeLen: integer;
    code: array of byte;
    dataLen: integer;
    Data: array of byte;
    Lines: array of LineRef;
    vars: array of VariableRef;
  end;

type
  TokenDecoder = function(var ctx: RBasicContext): string;

type
  RToken = record
    Name: string;
    decoder: TokenDecoder;
  end;

  RFunction = record
    Name: string;
    retType: integer;
    args: array of integer;
  end;

  ROperator = record
    Name: string;
    Code: integer;
    Pri: integer;
    Uni: boolean;
  end;

var
  IMP: array['A'..'Z'] of byte;

var
  TOKENS: array[$81..$BE] of RToken;
  FUNCS: array[0..39] of RFunction;
  OPERS: array[0..$1F] of ROperator;

function decodeStatement(var ctx: RBasicContext): string; forward;
function decodeExpression(var ctx: RBasicContext): string; forward;

function peekCode(var ctx: RBasicContext): integer; inline;
begin
  with ctx do begin
    if (codePos < codeLen) then begin
      Result := code[codePos];
    end
    else begin
      Result := 0;
    end;
  end;
end;

function getByte(var ctx: RBasicContext): integer; inline;
begin
  with ctx do begin
    Result := peekCode(ctx);
    Inc(codePos);
  end;
end;

function getWord(var ctx: RBasicContext): integer;
begin
  with ctx do begin
    if ((codePos + 1) < codeLen) then begin
      Result := code[codePos] * 256 + code[codePos + 1];
    end
    else begin
      Result := 0;
    end;
    Inc(codePos, 2);
  end;
end;

function getVarVal(var ctx: RBasicContext): VariableVal;
var
  i: integer;
begin
  with ctx do begin
    if ((codePos + 3) < codeLen) then begin
      for i := 0 to 3 do begin
        Result.b[3 - i] := code[codePos + i];
      end;
    end
    else begin
      Result.i := 0;
    end;
    Inc(codePos, 4);
  end;
end;

function getPString(var ctx: RBasicContext): string;
var
  i, l: integer;
begin
  Result := '';
  l := getByte(ctx);
  if l > 0 then begin
    with ctx do begin
      SetLength(Result, l);
      for i := 1 to l do begin
        Result[i] := chr(getByte(ctx));
      end;
    end;
  end;
end;

procedure decodeSymbolTable(var ctx: RBasicContext);
var
  addr: integer;
  numVars: integer;
  t, len, i: integer;
begin
  with ctx do begin
    addr := 0;
    numVars := 0;
    while True do begin
      if (dataLen - addr) < 1 then begin
        break;
      end;
      t := Data[addr];
      Inc(addr);
      len := t and $0F;
      if (len = 0) then begin
        break;
      end;
      if (dataLen - addr) < len then begin
        break;
      end;
      Inc(numVars);
      if (numVars > Length(vars)) then begin
        SetLength(vars, numVars + 100);
      end;
      with vars[numVars - 1] do begin
        Count := 0;
        typ := t and $F0 shr 4;
        SetLength(Name, len);
        for i := 1 to len do begin
          Name[i] := chr(Data[addr]);
          Inc(addr);
        end;
        offset := addr;
        len := Data[addr] and $0F;
        Inc(addr);
        if (dataLen - addr) < len then begin
          break;
        end;
        if len = 4 then begin
          hasVal := True;
          for i := 0 to 3 do begin
            val.b[i] := Data[addr];
            Inc(addr);
          end;
        end
        else begin
          hasVal := False;
          ref := Data[addr] * 256 + Data[addr + 1];
          addr := addr + len;
        end;
      end;
    end;
    SetLength(vars, NumVars);
  end;
end;

function findSymbol(var ctx: RBasicContext; aOffset: integer): PVariableRef;
var
  i: integer;
begin
  Result := nil;
  with ctx do begin
    for i := low(vars) to High(vars) do begin
      with vars[i] do begin
        if (offset = aOffset) then begin
          Result := @vars[i];
          Inc(Count);
          break;
        end;
      end;
    end;
  end;
end;

function decodeDouble(const v: VariableVal): double;
var
  s: boolean;
  e, m: integer;
begin
  s := (v.b[3] and $80) <> 0;
  e := v.b[3] and $7f;
  if (e > $40) then begin
    e := -(e and $3F);
  end;
  e := e - 24;
  m := v.i and $00FFFFFF;
  Result := m * IntPower(2, e);
  if (s) then begin
    Result := -Result;
  end;
end;

function decodeLineNRs(var ctx: RBasicContext): string;
var
  n, i, linNR: integer;
begin
  n := getByte(ctx);
  Result := '';
  for i := 1 to n do begin
    if (i > 1) then begin
      Result := Result + SEP_ARG;
    end;
    linNR := getWord(ctx);
    Result := Result + Format(FMT_LINENR, [linNR]);
  end;
end;

function decodeType(typ: integer; impl: char): string;
var
  t: integer;
begin
  t := typ and $03;
  if (impl >= 'A') and (impl <= 'Z') then begin
    if t = IMP[impl] then begin
      t := -1;
    end;
  end;
  case (t) of
    $0: begin
      Result := TYPE_FLOAT;
    end;
    $1: begin
      Result := TYPE_INTEGER;
    end;
    $2: begin
      Result := TYPE_STRING;
    end;
    else begin
      Result := '';
    end;
  end;
end;

function FormatFPT(cs: double): string;
begin
  Result := UpperCase(Format('%g', [cs]));
  if (pos('.', Result) = 0) and (pos('E', Result) = 0) then begin
    Result := Result + '.0';
  end;
end;

function decodeValue(cType: longint; val: VariableVal): string;
begin
  case cTYpe of
    $0: begin
      Result := FormatFPT(decodeDouble(val));
    end;
    $1: begin
      Result := Format(FMT_INTEGER, [val.i]);
    end;
    else begin
      Result := '';
    end;
  end;
end;

function decodeConstant(var ctx: RBasicContext): string;
var
  cType: integer;
  ci: integer;
  cf: double;
  cs: string;
begin
  with ctx do begin
    cType := getByte(ctx);
    case cTYpe of
      $10: begin
        cf := decodeDouble(getVarVal(ctx));
        Result := FormatFPT(cf);
      end;
      $14: begin
        ci := getVarVal(ctx).i;
        Result := Format(FMT_INTEGER, [ci]);
      end;
      $15: begin
        ci := getVarVal(ctx).i;
        Result := Format(FMT_HEX, [ci]);
      end;
      $18: begin // Quoted String
        cs := getPString(ctx);
        Result := Format(FMT_QSTRING, [cs]);
      end;
      $19: begin // Unquoted String
        cs := getPString(ctx);
        Result := Format(FMT_STRING, [cs]);
      end;
      else begin
        Result := '';
      end;
    end;
  end;
end;

function decodeVarRef(var ctx: RBasicContext; withArg: boolean): string;
var
  offset: integer;
  VR: PVariableRef;
  numArg, i: integer;
  typeC: string;
begin
  offset := getWord(ctx) and $3FFF;
  VR := findSymbol(ctx, offset);
  if (VR <> nil) then begin
    typeC := decodeType(VR^.typ, VR^.Name[1]);
    Result := VR^.Name + typeC;
    if (withArg) and ((VR^.typ and $4) <> 0) then begin
      Result := Result + '(';
      numArg := getByte(ctx);
      for i := 1 to numArg do begin
        getByte(ctx);
        if (i > 1) then begin
          Result := Result + SEP_ARG;
        end;
        Result := Result + decodeExpression(ctx);
      end;
      Result := Result + ')';
    end;
  end
  else begin
    Result := '';
  end;
end;

function decodeExpression(var ctx: RBasicContext): string;
var
  opcode, i: integer;
  tmp, le, re: string;
begin
  Result := '';
  with ctx do begin
    opcode := getByte(ctx);
    if opcode >= $80 then begin // Operator
      opcode := opcode and $1F;
      with OPERS[opcode] do begin
        if Uni then begin
          if Name = '(' then begin
            Result := '(' + decodeExpression(ctx) + ')';
          end
          else begin
            Result := Name + decodeExpression(ctx);
          end;
        end
        else begin
          if Name <> '' then begin
            if CharInSet(Name[1], ['A'..'Z']) then begin
              tmp := Format(FMT_OPERALT, [Name]);
            end
            else begin
              tmp := Format(FMT_OPER, [Name]);
            end;
          end
          else begin
            tmp := '';
          end;
          le := decodeExpression(ctx);
          re := decodeExpression(ctx);
          Result := le + tmp + re;
        end;
      end;
    end
    else if opcode >= $40 then begin // Variable reference
      Dec(codePos);
      Result := decodeVarRef(ctx, True);
    end
    else if opcode >= $20 then begin // Function call
      opcode := getByte(ctx);
      if opcode < Length(FUNCS) then begin
        with FUNCS[opcode] do begin
          Result := Name;
          if Length(args) > 0 then begin
            Result := Result + '(';
            for i := 0 to Length(args) - 1 do begin
              if (i > 0) then begin
                Result := Result + SEP_ARG;
              end;
              Result := Result + decodeExpression(ctx);
            end;
            Result := Result + ')';
          end;
        end;
      end;
    end
    else begin // Constants
      Dec(codePos);
      Result := decodeConstant(ctx);
    end;
  end;
end;

function TokenDecoder01(var ctx: RBasicContext): string;
  // (01) linenr
var
  lnr: integer;
begin
  with ctx do begin
    lnr := getWord(ctx);
    if (lnr <> 0) then begin
      Result := Format(FMT_LINENR, [lnr]);
    end
    else begin
      Result := '';
    end;
  end;
end;

function TokenDecoder02(var ctx: RBasicContext): string;
  // (02) linenr linenr (not used)
var
  lin1, lin2: integer;
begin
  lin1 := getWord(ctx);
  lin2 := getWord(ctx);
  Result := Format(FMT_LINENR_RANGE, [lin1, lin2]);
end;

function TokenDecoder03(var ctx: RBasicContext): string;
  // (03) unquoted string
begin
  Result := getPString(ctx);
end;

function TokenDecoder04(var ctx: RBasicContext): string;
  // (04) E (E=expr)
begin
  Result := decodeExpression(ctx);
end;

function TokenDecoder05(var ctx: RBasicContext): string;
  // (05) E, E
begin
  Result := decodeExpression(ctx) + SEP_ARG + decodeExpression(ctx);
end;

function TokenDecoder06(var ctx: RBasicContext): string;
  // (06) E E
begin
  Result := decodeExpression(ctx) + SEP_SPACE + decodeExpression(ctx);
end;

function TokenDecoder07(var ctx: RBasicContext): string;
  // (07) E,E E
begin
  Result := decodeExpression(ctx) + SEP_ARG + decodeExpression(ctx) + SEP_SPACE + decodeExpression(ctx);
end;

function TokenDecoder08(var ctx: RBasicContext): string;
  // (08) E,E E,E E
begin
  Result := decodeExpression(ctx) + SEP_ARG + decodeExpression(ctx) + SEP_SPACE + decodeExpression(ctx) + SEP_ARG + decodeExpression(ctx) + SEP_SPACE + decodeExpression(ctx);
end;

function TokenDecoder09(var ctx: RBasicContext): string;
  // (09) E E E E
begin
  Result := decodeExpression(ctx) + SEP_SPACE + decodeExpression(ctx) + SEP_SPACE + decodeExpression(ctx) + SEP_SPACE + decodeExpression(ctx);
end;

function TokenDecoder0A(var ctx: RBasicContext): string;
  // (0A) E
var
  eol: integer;
begin
  Result := decodeExpression(ctx) + SEP_ARG + decodeExpression(ctx);
  repeat
    eol := peekCode(ctx);
    if eol <> $FF then begin
      Result := Result + SEP_ARG + decodeExpression(ctx);
    end
    else begin
      Inc(ctx.codePos);
    end;
  until eol = $FF;
end;

function TokenDecoder0B(var ctx: RBasicContext): string;
  // (0E) linenr-linenr
var
  lin1, lin2: integer;
begin
  lin1 := getWord(ctx);
  lin2 := getWord(ctx);
  Result := Format(FMT_LINENR_RANGEALT, [lin1, lin2]);
end;

function TokenDecoder0C(var ctx: RBasicContext): string;
  // (0C) sound
var
  eol: integer;
begin
  eol := peekCode(ctx);
  if (eol = $FF) then begin
    Result := 'OFF';
    Inc(ctx.codePos);
  end
  else begin
    Result := decodeExpression(ctx) + SEP_SPACE;
    eol := peekCode(ctx);
    if (eol = $FF) then begin
      Result := Result + 'OFF';
      Inc(ctx.codePos);
    end
    else begin
      Result := Result + TokenDecoder09(ctx);
    end;
  end;
end;

function TokenDecoder0D(var ctx: RBasicContext): string;
  // (0D) noise
var
  eol: integer;
begin
  eol := peekCode(ctx);
  if (eol = $FF) then begin
    Result := 'OFF';
    Inc(ctx.codePos);
  end
  else begin
    Result := TokenDecoder06(ctx);
  end;
end;

function TokenDecoder0E(var ctx: RBasicContext): string;
  // (0E) envelope
var
  n, i, eol: integer;
begin
  Result := decodeExpression(ctx) + SEP_SPACE;
  n := getByte(ctx);
  for i := 1 to n do begin
    Result := Result + TokenDecoder05(ctx) + SEP_LIST;
  end;
  eol := peekCode(ctx);
  if (eol = $FF) then begin
    Inc(ctx.codePos);
  end
  else begin
    Result := Result + decodeExpression(ctx);
  end;
end;

function TokenDecoder0F(var ctx: RBasicContext): string;
  // (0F) mode
var
  m: integer;
  a: boolean;
begin
  m := getByte(ctx);
  if (m = $FF) then begin
    Result := '0';
  end
  else begin
    a := (m and $01) <> 0;
    m := m shr 1;
    Result := chr(Ord('0') + m);
    if a then begin
      Result := Result + 'A';
    end;
  end;
end;

function TokenDecoder11(var ctx: RBasicContext): string;
  // (11) input/read/dim
var
  numVar, i: integer;
begin
  numVar := getByte(ctx);
  Result := '';
  for i := 1 to numVar do begin
    if (i > 1) then begin
      Result := Result + SEP_ARG;
    end;
    Result := Result + decodeVarRef(ctx, True);
  end;
end;

function TokenDecoder10(var ctx: RBasicContext): string;
  // (10) input <string>
begin
  Result := decodeExpression(ctx) + SEP_LIST + TokenDecoder11(ctx);
end;

function TokenDecoder12(var ctx: RBasicContext): string;
  // (12) (not used [*])
begin
  with ctx do begin
    Result := '';
  end;
end;

function TokenDecoder13(var ctx: RBasicContext): string;
  // (13) let
begin
  Result := decodeVarRef(ctx, True) + SEP_EQUAL + decodeExpression(ctx);
end;

function TokenDecoder14(var ctx: RBasicContext): string;
  // (14) if then <E>
begin
  Result := decodeExpression(ctx) + ' THEN ';
  getByte(ctx);
  Result := Result + decodeStatement(ctx);
end;

function TokenDecoder15(var ctx: RBasicContext): string;
  // (15) if goto <linenr>
var
  linNR: integer;
begin
  Result := decodeExpression(ctx) + ' GOTO ';
  linNR := getWord(ctx);
  Result := Result + Format(FMT_LINENR, [linNR]);
end;

function TokenDecoder16(var ctx: RBasicContext): string;
  // (16) if then <linenr>
var
  linNR: integer;
begin
  Result := decodeExpression(ctx) + ' THEN ';
  linNR := getWord(ctx);
  Result := Result + Format(FMT_LINENR, [linNR]);
end;

function TokenDecoder17(var ctx: RBasicContext): string;
  // (17) for to step
var
  eol: integer;
begin
  Result := TokenDecoder13(ctx) + ' TO ' + decodeExpression(ctx);
  eol := peekCode(ctx);
  if (eol = $FF) then begin
    Inc(ctx.codePos);
  end
  else begin
    Result := Result + ' STEP ' + decodeExpression(ctx);
  end;
end;

function TokenDecoder18(var ctx: RBasicContext): string;
  // (18) next
begin
  Result := decodeExpression(ctx);
end;

function TokenDecoder19(var ctx: RBasicContext): string;
  // (19) print
var
  n, i, b: integer;
begin
  Result := '';
  n := getByte(ctx);
  for i := 1 to n do begin
    Result := Result + decodeExpression(ctx);
    b := getByte(ctx);
    if (b = $FF) then begin
      break;
    end;
    Result := Result + chr(b);
  end;
end;

function TokenDecoder1A(var ctx: RBasicContext): string;
  // (1A) on goto
begin
  Result := decodeExpression(ctx) + ' GOTO ' + decodeLineNRs(ctx);
end;

function TokenDecoder1B(var ctx: RBasicContext): string;
  // (1B) on gosub
begin
  Result := decodeExpression(ctx) + ' GOSUB ' + decodeLineNRs(ctx);
end;

function TokenDecoder1C(var ctx: RBasicContext): string;
  // (1C) callm
var
  eol: integer;
begin
  Result := decodeExpression(ctx);
  repeat
    eol := peekCode(ctx);
    if eol <> $FF then begin
      Result := Result + SEP_ARG + decodeExpression(ctx);
    end
    else begin
      Inc(ctx.codePos);
    end;
  until eol = $FF;
end;

function TokenDecoder1D(var ctx: RBasicContext): string;
  // (1D) (not used [*])
begin
  with ctx do begin
    Result := '';
  end;
end;

function TokenDecoder1E(var ctx: RBasicContext): string;
  // (1E) savea/loada
begin
  Result := decodeVarRef(ctx, False) + SEP_SPACE + decodeExpression(ctx);
end;

function decodeStatement(var ctx: RBasicContext): string;
var
  token: integer;
begin
  token := getByte(ctx);
  Result := '?';
  if (token < Low(TOKENS)) or (token > High(TOKENS)) then begin
    exit;
  end;
  with  TOKENS[token] do begin
    Result := Name;
    if (decoder <> nil) then begin
      if (Result <> '') then begin
        Result := Result + SEP_SPACE;
      end;
      Result := Result + decoder(ctx);
    end;
  end;
end;

function BASIC_decode(const codeSeg, dataSeg: RSegment; var src: TStrings): integer;
var
  ctx: RBasicContext;
  lineLen, lineNr: integer;
  lineNRStr, outLine: string;
  nxtCmd, curLine: integer;
  numLin, i: integer;
  token, s: string;
begin
  with ctx do begin
    codeLen := codeSeg.size;
    dataLen := dataSeg.size;
    if (codeLen < 1) or (dataLen < 1) then begin
      Result := R_INVALIDPROG;
      exit;
    end;
    codePos := 0;
    code := codeSeg.Data;
    Data := dataSeg.Data;
    SetLength(Lines, 0);
    SetLength(vars, 0);
    decodeSymbolTable(ctx);
  end;
  numLin := 0;
  with ctx do begin
    while (codePos < (codeLen - 1)) do begin
      lineLen := getByte(ctx);
      if (lineLen = 0) then begin
        break;
      end;
      curLine := codePos - 1;
      Inc(numLin);
      outLine := '';
      if ((codeLen - codePos) < lineLen + 1) then begin
        Result := R_INVALIDLINE;
        exit;
      end;
      lineNr := getWord(ctx);
      lineNRStr := Format(FMT_LINENR_START, [lineNr]);
      if (numLin > Length(Lines)) then begin
        SetLength(Lines, numLin + 1000);
      end;
      Lines[numLin - 1].lineNR := lineNR;
      Lines[numLin - 1].addr := curLine;
      outLine := lineNRStr;
      repeat
        token := decodeStatement(ctx);
        if token = '?' then begin
          Result := R_INVALIDPROG;
          exit;
        end;
        outline := outline + token;
        nxtCmd := peekCode(ctx);
        if (nxtCmd > $80) then begin
          outLine := outline + SEP_STMT;
        end;
      until (nxtCmd <= $80);
      src.add(outLine);
    end;
    SetLength(Lines, numLin);
  end;
  with ctx do begin
    if Length(vars) > 0 then begin
      src.add(';DATA SECTION');
      for i := low(vars) to High(vars) do begin
        with vars[i] do begin
          s := Format('%.4x@%s%s %d reference(s)', [offset, Name, decodeType(typ, #0), Count]);
          src.add('; ' + s);
        end;
      end;
    end;
  end;
  Result := R_OK;
end;

function BASIC_encode(const src: TStrings; out codeSeg, dataSeg: RSegment): integer;
begin
  Segment_init(codeSeg);
  Segment_init(dataSeg);
  Result := R_OK;
end;

var
  ch: char;

initialization
  {$include basic_tokens.inc}
  for ch := 'A' to 'Z' do begin
    IMP[ch] := 0;
  end;
end.