program DTcli;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  libDAI,
  libTools,
  libImage,
  libGraphFrame,
  FPImage,
  uFilters;

type
  CommandCall = function(const paths: TStrings; const param: string): boolean of object;

  ROption = record
    sName: char;
    lName: string;
    suffix: string;
    desc: string;
  end;

  RCommand = record
    sName: char;
    lName: string;
    suffix: string;
    desc: string;
    opts: array of ROption;
    call: CommandCall;
  end;

  { TDAIToolCLI }

  TDAIToolCLI = class(TCustomApplication)
  private
    CMDS: array[0..3] of RCommand;
    helpIdx: integer;
    procedure createParam(out sNames: string; lNames: TStrings);
    function _has(const o: ROption): boolean;
    function _get(const o: ROption): string;
    function _getInt(const o: ROption; defVal, noVal: integer): integer;
    function _getOutType(const param: string): integer;
  protected
    procedure WriteError(const msg: string = '');
    procedure DoRun; override;
  protected
    function cmdHelp(const paths: TStrings; const param: string): boolean;
    function cmdConvertFile(const paths: TStrings; const param: string): boolean;
    function cmdConvertGraphic(const paths: TStrings; const param: string): boolean;
    function cmdConvertFrame(const paths: TStrings; const param: string): boolean;
  private
    function getMetadataPath(const outPath: string): string;
    function getOutPath(const outPath: string; const defPath, defName, defExt: string; force: boolean): string;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  OPT_OUTPUT: ROption = (sName: 'o'; lName: 'output'; suffix: ':'; desc: 'output file name (if missing is input file with extension changed)');
  OPT_INTYPE: ROption = (sName: 't'; lName: 'input-type'; suffix: ':'; desc: 'input file type (if missing is input file extension)');
  OPT_QUANTIZE: ROption = (sName: 'q'; lName: 'quantize'; suffix: '::'; desc: 'quantize[:numer_of_colors]');
  OPT_DITHER: ROption = (sName: 'd'; lName: 'dither'; suffix: ''; desc: 'dither ON/OFF');
  OPT_OPTIMIZE: ROption = (sName: 'z'; lName: 'optimize'; suffix: ''; desc: 'use DAI tricks to reduce the image size');
  OPT_XOR: ROption = (sName: 'x'; lName: 'xor'; suffix: ''; desc: 'apply xor to frames');
  OPT_NOTRIM: ROption = (sName: 't'; lName: 'no-trim'; suffix: ''; desc: 'skip trim of common parts');

  { TDAIToolCLI }
  procedure TDAIToolCLI.WriteError(const msg: string = '');
  begin
    if (msg = '') then begin
      writeln('Invalid combination of parameters, use -h for help');
    end
    else begin
      writeln(msg);
    end;
    Terminate(1);
  end;

  procedure TDAIToolCLI.createParam(out sNames: string; lNames: TStrings);
  var
    i, o: integer;
    os: set of char;
  begin
    lNames.Clear;
    sNames := '';
    os := [];
    for i := low(CMDS) to High(CMDS) do begin
      with CMDS[i] do begin
        sNames := sNames + sName + suffix;
        lNames.Add(lName + suffix);
        for o := Low(opts) to high(opts) do begin
          if not CharInSet(opts[o].sName, os) then begin
            Include(os, opts[o].sName);
            sNames := sNames + opts[o].sName + opts[o].suffix;
            lNames.Add(lName + opts[o].lName + opts[o].suffix);
          end;
        end;
      end;
    end;
  end;

  procedure TDAIToolCLI.DoRun;
  var
    ErrorMsg: string;
    opts, argPaths, paths: TStringList;
    path, inPath, inDir: string;
    i: integer;
    SR: TSearchRec;
    sNames: string;
    param: string;
    lNames: TStringList;
    ok: boolean;
  begin
    paths := TStringList.Create;
    argPaths := TStringList.Create;
    opts := TStringList.Create;
    lNames := TStringList.Create;
    try
      // Pre processing of parameters
      createParam(sNames, lNames);
      ErrorMsg := CheckOptions(snames, lNames, opts, argPaths);
      if ErrorMsg <> '' then begin
        WriteError(ErrorMsg);
        Exit;
      end;
      // Target file(s)
      for i := 0 to argPaths.Count - 1 do begin
        inPath := argPaths[i];
        inDir := ExtractFileDir(inPath);
        if FindFirst(inPath, faAnyFile and not faDirectory, SR) = 0 then begin
          repeat
            if (inDir <> '') then begin
              path := inDir + '\' + SR.Name;
            end
            else begin
              path := SR.Name;
            end;
            paths.Add(path);
          until FindNext(SR) <> 0;
          findclose(SR);
        end;
      end;
      // Find the command
      ok := False;
      for i := low(CMDS) to High(CMDS) do begin
        with CMDS[i] do begin
          if HasOption(sName, lName) then begin
            param := GetOptionValue(sName, lName);
            ok := call(paths, param);
            if (ok) then begin
              Writeln('done!');
            end
            else begin
              Writeln('KO!');
            end;
            break;
          end;
        end;
      end;
      if not ok then begin
        CMDS[helpIdx].call(nil, '');
      end;
      //
      Terminate(0);
    finally
      FreeAndNil(argPaths);
      FreeAndNil(opts);
      FreeAndNil(paths);
      FreeAndNil(lNames);
    end;
  end;

  function TDAIToolCLI.getMetadataPath(const outPath: string): string;
  begin
    Result := ExtractFileDir(outPath);
    if (Result <> '') then begin
      Result := Result + '\';
    end;
    Result := Result + 'metadata.json';
  end;

  function TDAIToolCLI.getOutPath(const outPath: string; const defPath, defName, defExt: string; force: boolean): string;
  var
    base: string;
    i: integer;
    filePath, fileName, fileExt: string;
  begin
    if (outPath = '') then begin
      filePath := defPath;
      fileName := defName;
      fileExt := '.' + defExt;
    end
    else if (outPath[Length(outPath)] = '\') then begin
      filePath := outPath;
      fileName := defName;
      fileExt := '.' + defExt;
      force := False;
    end
    else begin
      filePath := ExtractFilePath(outPath);
      fileName := ChangeFileExt(ExtractFileName(outPath), '');
      fileExt := ExtractFileExt(outPath);
    end;
    base := filePath + fileName + '.%.3d' + fileExt;
    if not force then begin
      Result := filePath + fileName + fileExt;
    end;
    for i := 0 to 999 do begin
      if (i <> 0) or force then begin
        Result := Format(base, [i]);
      end;
      if not FileExists(Result) then begin
        break;
      end;
    end;
  end;

  function TDAIToolCLI._has(const o: ROption): boolean; inline;
  begin
    Result := HasOption(o.sName, o.lName);
  end;

  function TDAIToolCLI._get(const o: ROption): string; inline;
  begin
    Result := GetOptionValue(o.sName, o.lName);
  end;

  function TDAIToolCLI._getInt(const o: ROption; defVal, noVal: integer): integer;
  var
    t: string;
  begin
    Result := noVal;
    if HasOption(o.sName, o.lName) then begin
      Result := defVal;
      t := GetOptionValue(o.sName, o.lName);
      if (t <> '') then begin
        Result := StrToIntDef(t, defVal);
      end;
    end;
  end;

  function TDAIToolCLI._getOutType(const param: string): integer;
  begin
    Result := IndexOfSaveFilter(param);
    if (Result < 0) then begin
      WriteError('Invalid input type');
      exit;
    end;
  end;

  function TDAIToolCLI.cmdConvertFile(const paths: TStrings; const param: string): boolean;
  var
    inType: integer;
    outPath: string;
    outType: integer;
    ext, outName: string;
    loadFilter, saveFilter: PFilter;
    s: RSegment;
    i: integer;
    inPath: string;
  begin
    Result := False;
    outType := _getOutType(param);
    if (outType < 0) then begin
      exit;
    end;
    saveFilter := FindSaveFilter(outType);
    inType := IndexOfLoadFilter(GetOptionValue(OPT_INTYPE.sName, OPT_INTYPE.lName));
    outPath := _get(OPT_OUTPUT);
    for i := 0 to paths.Count - 1 do begin
      inPath := paths[i];
      if (inType < 0) then begin
        ext := ExtractFileExt(inPath);
        if (length(ext) > 1) then begin
          ext := copy(ext, 2, length(ext) - 1);
        end;
        inType := IndexOfLoadFilter(ext);
        if (inType < 0) then begin
          Result := False;
          Exit;
        end;
      end;
      loadFilter := FindLoadFilter(inType);
      outName := getOutPath(outPath, ExtractFilePath(inPath), ChangeFileExt(ExtractFileName(inPath), ''), saveFilter^.ext, paths.Count > 1);
      WriteLn('Converting ' + inPath + ' to ' + outName);
      s.size := 0;
      s.Name := ChangeFileExt(ExtractFileName(outName), '');
      Result := loadFilter^.proc(inPath, s);
      if not Result then begin
        Writeln(DAI_lastError());
        Exit;
      end;
      Result := saveFilter^.proc(outName, s);
      if not Result then begin
        Writeln(DAI_lastError());
        Exit;
      end;
      Segment_writeMetadata(s, getMetadataPath(outPath));
    end;
    Result := paths.Count > 0;
  end;

  function TDAIToolCLI.cmdConvertGraphic(const paths: TStrings; const param: string): boolean;
  var
    outPath: string;
    outType: integer;
    quantize: integer;
    dither: boolean;
    optimize: boolean;
    outName: string;
    saveFilter: PFilter;
    s: RSegment;
    image: TFPCustomImage;
    i: integer;
    inPath: string;
  begin
    Result := False;
    outType := _getOutType(param);
    if (outType < 0) then begin
      exit;
    end;
    saveFilter := FindSaveFilter(outType);
    outPath := _get(OPT_OUTPUT);
    optimize := _has(OPT_OPTIMIZE);
    dither := _has(OPT_DITHER);
    quantize := _getInt(OPT_QUANTIZE, 256, 0);
    for i := 0 to paths.Count - 1 do begin
      inPath := paths[i];
      outName := getOutPath(outPath, ExtractFilePath(inPath), ChangeFileExt(ExtractFileName(inPath), ''), saveFilter^.ext, paths.Count > 1);
      WriteLn('Converting ' + inPath + ' to ' + outName);
      s.size := 0;
      s.Name := ChangeFileExt(ExtractFileName(outName), '');
      image := nil;
      try
        image := prepareImage(inPath, quantize, dither);
        Result := DAI_loadPNG(image, optimize, s);
        if not Result then begin
          Writeln(DAI_lastError());
          Exit;
        end;
        Result := saveFilter^.proc(outName, s);
        if not Result then begin
          Writeln(DAI_lastError());
          Exit;
        end;
        Segment_writeMetadata(s, getMetadataPath(outPath));
      finally
        if (image <> nil) then begin
          FreeAndNil(image);
        end;
      end;
    end;
    Result := True;
  end;

  function TDAIToolCLI.cmdConvertFrame(const paths: TStrings; const param: string): boolean;
  var
    outPath: string;
    outType: integer;
    doXor, skipTrim: boolean;
    outName: string;
    saveFilter: PFilter;
    s: array[0..1] of RSegment;
    res: RSegment;
    dst: PSegment;
    idx: integer;
    sa, ea: integer;
    image: TFPCustomImage;
    i: integer;
    inPath: string;
  begin
    Result := False;
    outType := _getOutType(param);
    if (outType < 0) then begin
      exit;
    end;
    saveFilter := FindSaveFilter(outType);
    outPath := _get(OPT_OUTPUT);
    doXor := _has(OPT_XOR);
    skipTrim := _has(OPT_NOTRIM);
    idx := 0;
    for i := 0 to paths.Count - 1 do begin
      inPath := paths[i];
      outName := getOutPath(outPath, ExtractFilePath(inPath), ChangeFileExt(ExtractFileName(inPath), ''), saveFilter^.ext, paths.Count > 1);
      WriteLn('Frame ', i: 4, '-', inPath);
      image := nil;
      try
        dst := @s[idx];
        dst^.size := 0;
        dst^.Name := ChangeFileExt(ExtractFileName(outName), '');
        Result := DAI_loadBIN(inPath, dst^);
        if not Result then begin
          Writeln(DAI_lastError());
          Exit;
        end;
        if (i <> 0) then begin
          dst := @res;
          if (doXor) then begin
            Segment_xor(s[1 - idx], s[idx], res);
          end
          else begin
            Segment_copy(s[idx], res);
          end;
          if not skipTrim then begin
            Segment_diff(s[1 - idx], s[idx], sa, ea);
            Segment_slice(res, sa, ea);
          end;
        end;
        Result := saveFilter^.proc(outName, dst^);
        Segment_writeMetadata(dst^, getMetadataPath(outPath));
        if not Result then begin
          Writeln(DAI_lastError());
          Exit;
        end;
        idx := 1 - idx;
      finally
        if (image <> nil) then begin
          FreeAndNil(image);
        end;
      end;
    end;
    Result := True;
  end;

  constructor TDAIToolCLI.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
    with CMDS[0] do begin
      sName := 'c';
      lName := 'convert-file';
      suffix := ':';
      desc := 'convert between DAI file formats';
      call := @cmdConvertFile;
      SetLength(opts, 2);
      opts[0] := OPT_OUTPUT;
      opts[1] := OPT_INTYPE;
    end;
    with CMDS[1] do begin
      sName := 'g';
      lName := 'convert-graphic';
      desc := 'convert a PNG in different DAI file formats';
      suffix := ':';
      call := @cmdConvertGraphic;
      SetLength(opts, 4);
      opts[0] := OPT_OUTPUT;
      opts[1] := OPT_QUANTIZE;
      opts[2] := OPT_DITHER;
      opts[3] := OPT_OPTIMIZE;
    end;
    with CMDS[2] do begin
      sName := 'f';
      lName := 'convert-frame';
      suffix := ':';
      desc := 'generated a delta sequence';
      call := @cmdConvertFrame;
      SetLength(opts, 3);
      opts[0] := OPT_OUTPUT;
      opts[1] := OPT_XOR;
      opts[2] := OPT_NOTRIM;
    end;
    with CMDS[3] do begin
      sName := 'h';
      lName := 'help';
      suffix := '';
      call := @cmdHelp;
    end;
    helpIdx := 3;
  end;

  destructor TDAIToolCLI.Destroy;
  begin
    inherited Destroy;
  end;

  function TDAIToolCLI.cmdHelp(const paths: TStrings; const param: string): boolean;
  var
    i, j: integer;
  begin
    { add your help code here }
    WriteLn('Usage:');
    WriteLn('  ', ExtractFileName(ExeName), ' [-h|--help]');
    for i := low(CMDS) to high(CMDS) do begin
      if i = helpIdx then begin
        continue;
      end;
      with CMDS[i] do begin
        WriteLn('  ', ExtractFileName(ExeName), ' [-', sName, '|--', lName, '] save_type [options] inputfile1 [inoutfile2 [...]]');
      end;
    end;
    for i := low(CMDS) to high(CMDS) do begin
      if i = helpIdx then begin
        continue;
      end;
      WriteLn();
      with CMDS[i] do begin
        Writeln('  Command: --', lName, ' ', desc);
        for j := low(opts) to high(opts) do begin
          Write(' ');
          if (suffix <> '') then begin
            Write('[');
          end;
          Write('-', opts[j].sName, '|--', opts[j].lName);
          if (suffix = '::') then begin
            Write('[:val]');
          end;
          if (suffix <> '') then begin
            Write(']');
          end;
          Writeln(' ', opts[j].desc);
        end;
      end;
    end;
    WriteLn();
    WriteLn('Input types allowed:');
    for i := low(LOAD_FILTERS) to high(LOAD_FILTERS) do begin
      writeln('  ', LOAD_FILTERS[i].ext, ' -> ', StringReplace(LOAD_FILTERS[i].displayName, ' ', '_', [rfReplaceAll]));
    end;
    WriteLn();
    WriteLn('Output types allowed:');
    for i := low(SAVE_FILTERS) to high(SAVE_FILTERS) do begin
      WriteLn('  ', StringReplace(SAVE_FILTERS[i].displayName, ' ', '_', [rfReplaceAll]), ' -> ', SAVE_FILTERS[i].ext);
    end;
    Result := True;
  end;

var
  Application: TDAIToolCLI;
begin
  Application := TDAIToolCLI.Create(nil);
  Application.Title:='DAI tools CLI';
  Application.Run;
  Application.Free;
end.
