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

type
  RBasicSettings = record
    //
    SEP_SPACE: string;
    SEP_ARG: string;
    SEP_LIST: string;
    SEP_STMT: string;
    SEP_EQUAL: string;
    //
    TYPE_FLOAT: string;
    TYPE_INTEGER: string;
    TYPE_STRING: string;
    //
    FMT_LINENR: string;
    FMT_LINENR_START: string;
    FMT_LINENR_RANGE: string;
    FMT_LINENR_RANGEALT: string;
    //
    FMT_OPER: string;
    FMT_OPERALT: string;
    //
    FMT_INTEGER: string;
    FMT_HEX: string;
    FMT_STRING: string;
    FMT_QSTRING: string;
    //
    FPT_DAILIKE: boolean;
  end;

var
  Settings: RBasicSettings;

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
  m := v.i and $00FFFFFF;
  if (e > $40) then begin
    e := -(e xor $7F + 1);
  end;
  e := e - 24;
  Result := m * IntPower(2, e);
  if (s) then begin
    Result := -Result;
  end;
end;

function decodeSingle(const v: VariableVal): single;
var
  s: boolean;
  e, m: integer;
begin
  s := (v.b[3] and $80) <> 0;
  e := v.b[3] and $7f;
  m := v.i and $00FFFFFF;
  if (e > $40) then begin
    e := -(e xor $7F + 1);
  end;
  e := e - 24;
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
      Result := Result + Settings.SEP_ARG;
    end;
    linNR := getWord(ctx);
    Result := Result + Format(Settings.FMT_LINENR, [linNR]);
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
      Result := Settings.TYPE_FLOAT;
    end;
    $1: begin
      Result := Settings.TYPE_INTEGER;
    end;
    $2: begin
      Result := Settings.TYPE_STRING;
    end;
    else begin
      Result := '';
    end;
  end;
end;

type
  RDECBUF = record
    S: char;
    dp: integer;
    dg: array[0..9] of char;
  end;

var
  CST_V: array[0..6] of double;
  CST_P: array[0..6] of integer = (0, 0, 1, 2, 4, 9, 19);

procedure decodeFPT(const v: VariableVal; out sm, se: boolean; out e, c: integer; out m: double);
var
  f: boolean;
  i: integer;
begin
  sm := (v.b[3] and $80) <> 0;
  e := v.b[3] and $7f;
  c := 0;
  se := (e > $40);
  if (se) then begin
    e := (e xor $7F) + 1;
  end;
  m := (v.i and $00FFFFFF) / IntPower(2, 24);
  for i := 0 to 6 do begin
    f := (e and $01) <> 0;
    e := e shr 1;
    if (f) then begin
      c := c + CST_P[i];
      if se then begin
        m := m / CST_V[i];
      end
      else begin
        m := m * CST_V[i];
      end;
    end;
  end;
  if not se then begin
    while (m >= 1.0) do begin
      m := m * 0.1;
      Inc(c);
    end;
  end
  else begin
    c := -c;
    while (m < 0.1) do begin
      m := m * 10.0;
      Inc(c);
    end;
  end;
end;

procedure trimZeros(var s: string; keepZero: boolean);
var
  len: integer;
begin
  len := length(s);
  while (len > 0) and (s[len] = '0') do begin
    Dec(len);
  end;
  if (len > 0) and (s[len] = '.') then begin
    if (keepZero) then begin
      Inc(len);
    end
    else begin
      Dec(len);
    end;
  end;
  SetLength(s, len);
end;

function decodeDigits(m: double; c, n: integer): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to n do begin
    m := frac(m) * 10;
    Result := Result + chr(Ord('0') + trunc(m));
    if (c = i) then begin
      Result := Result + '.';
    end;
  end;
end;

function DAI_FPT(const v: VariableVal): string;
var
  sm, se: boolean;
  e, c: integer;
  m: double;
  n: integer;
begin
  if v.i = 0 then begin
    Result := '0.0';
    exit;
  end;
  decodeFPT(v, sm, se, e, c, m);
  if (sm) then begin
    Result := '-';
  end
  else begin
    Result := '';
  end;
  n := 7;
  if (c >= 0) and (c <= 6) then begin
    if (c <= 0) then begin
      Result := Result + '0.';
    end;
    while ((c < 0) and (n > 1)) do begin
      Result := Result + '0';
      Inc(c);
      Dec(n);
    end;
    Result := Result + decodeDigits(m, c, n);
    trimZeros(Result, True);
  end
  else begin
    Result := Result + decodeDigits(m, 1, n);
    trimZeros(Result, False);
    Result := Result + 'E';
    Result := Result + IntToStr(c - 1);
  end;
end;

function FormatFPT(const v: VariableVal): string;
var
  cs: single;
begin
  if (Settings.FPT_DAILIKE) then begin
    Result := DAI_FPT(v);
  end
  else begin
    cs := decodeDouble(v);
    Result := UpperCase(Format('%g', [cs]));
  end;
end;

function decodeValue(cType: longint; const val: VariableVal): string;
begin
  case cTYpe of
    $0: begin
      Result := FormatFPT(val);
    end;
    $1: begin
      Result := Format(Settings.FMT_INTEGER, [val.i]);
    end;
    else begin
      Result := '';
    end;
  end;
end;

function decodeConstant(var ctx: RBasicContext): string;
var
  cType: integer;
begin
  with ctx do begin
    cType := getByte(ctx);
    case cType of
      $10: begin
        Result := FormatFPT(getVarVal(ctx));
      end;
      $14: begin
        Result := Format(Settings.FMT_INTEGER, [getVarVal(ctx).i]);
      end;
      $15: begin
        Result := Format(Settings.FMT_HEX, [getVarVal(ctx).i]);
      end;
      $18: begin // Quoted String
        Result := Format(Settings.FMT_QSTRING, [getPString(ctx)]);
      end;
      $19: begin // Unquoted String
        Result := Format(Settings.FMT_STRING, [getPString(ctx)]);
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
          Result := Result + Settings.SEP_ARG;
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
              tmp := Format(Settings.FMT_OPERALT, [Name]);
            end
            else begin
              tmp := Format(Settings.FMT_OPER, [Name]);
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
                Result := Result + Settings.SEP_ARG;
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
      Result := Format(Settings.FMT_LINENR, [lnr]);
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
  Result := Format(Settings.FMT_LINENR_RANGE, [lin1, lin2]);
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
  Result := decodeExpression(ctx) + Settings.SEP_ARG + decodeExpression(ctx);
end;

function TokenDecoder06(var ctx: RBasicContext): string;
  // (06) E E
begin
  Result := decodeExpression(ctx) + Settings.SEP_SPACE + decodeExpression(ctx);
end;

function TokenDecoder07(var ctx: RBasicContext): string;
  // (07) E,E E
begin
  Result := TokenDecoder05(ctx);
  Result := Result + Settings.SEP_SPACE + decodeExpression(ctx);
end;

function TokenDecoder08(var ctx: RBasicContext): string;
  // (08) E,E E,E E
begin
  Result := TokenDecoder05(ctx);
  Result := Result + Settings.SEP_SPACE + TokenDecoder07(ctx);
end;

function TokenDecoder09(var ctx: RBasicContext): string;
  // (09) E E E E
begin
  Result := TokenDecoder06(ctx);
  Result := Result + Settings.SEP_SPACE + TokenDecoder06(ctx);
end;

function TokenDecoder0A(var ctx: RBasicContext): string;
  // (0A) E
var
  eol: integer;
begin
  Result := decodeExpression(ctx) + Settings.SEP_ARG + decodeExpression(ctx);
  repeat
    eol := peekCode(ctx);
    if eol <> $FF then begin
      Result := Result + Settings.SEP_ARG + decodeExpression(ctx);
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
  Result := Format(Settings.FMT_LINENR_RANGEALT, [lin1, lin2]);
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
    Result := decodeExpression(ctx) + Settings.SEP_SPACE;
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
  Result := decodeExpression(ctx) + Settings.SEP_SPACE;
  n := getByte(ctx);
  for i := 1 to n do begin
    Result := Result + TokenDecoder05(ctx) + Settings.SEP_LIST;
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
    Result := chr(Ord('0') + m + 1);
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
      Result := Result + Settings.SEP_ARG;
    end;
    Result := Result + decodeVarRef(ctx, True);
  end;
end;

function TokenDecoder10(var ctx: RBasicContext): string;
  // (10) input <string>
begin
  Result := decodeExpression(ctx) + Settings.SEP_LIST + TokenDecoder11(ctx);
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
  Result := decodeVarRef(ctx, True) + Settings.SEP_EQUAL + decodeExpression(ctx);
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
  Result := Result + Format(Settings.FMT_LINENR, [linNR]);
end;

function TokenDecoder16(var ctx: RBasicContext): string;
  // (16) if then <linenr>
var
  linNR: integer;
begin
  Result := decodeExpression(ctx) + ' THEN ';
  linNR := getWord(ctx);
  Result := Result + Format(Settings.FMT_LINENR, [linNR]);
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
  n, i, b, fType: integer;
begin
  Result := '';
  n := getByte(ctx);
  for i := 1 to n do begin
    fType := getByte(ctx);
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
      Result := Result + Settings.SEP_ARG + decodeExpression(ctx);
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
  Result := decodeVarRef(ctx, False) + Settings.SEP_SPACE + decodeExpression(ctx);
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
        Result := Result + Settings.SEP_SPACE;
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
  token, s, Nam: string;
  v: VariableRef;
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
      lineNRStr := Format(Settings.FMT_LINENR_START, [lineNr]);
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
          outLine := outline + Settings.SEP_STMT;
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
        v := vars[i];
        with v do begin
          Nam := Name + decodeType(typ, #0);
          if (v.typ and $04) <> 0 then begin
            Nam := Nam + '()';
          end;
          s := Format('%.4x %-15s %6d reference(s)', [offset, Nam, Count]);
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
  Settings_DAIOriginal(Settings);
  for ch := 'A' to 'Z' do begin
    IMP[ch] := 0;
  end;
  CST_V[0] := IntPower(2, 1) / IntPower(10, 0);
  CST_V[1] := IntPower(2, 2) / IntPower(10, 0);
  CST_V[2] := IntPower(2, 4) / IntPower(10, 1);
  CST_V[3] := IntPower(2, 8) / IntPower(10, 2);
  CST_V[4] := IntPower(2, 16) / IntPower(10, 4);
  CST_V[5] := IntPower(2, 32) / IntPower(10, 9);
  CST_V[6] := IntPower(2, 64) / IntPower(10, 19);
end.

