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

  { TDAIToolCLI }

  TDAIToolCLI = class(TCustomApplication)

  protected
    procedure WriteError(const msg: string = '');
    procedure WriteHelp();
    procedure DoRun; override;
  private
    function getMetadataPath(const outPath: string): string;
    function getOutPath(const outPath, def: string; force: boolean): string;
    function convertTo(const paths: TStrings; inType: integer; outPath: string; outType: integer): boolean;
    function convertGraphic(const paths: TStrings; outPath: string; outType: integer; quantize: integer; dither: boolean; optimize: boolean): boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TDAIToolCLI }
const
  CONVERT_TO = 'convert-to';
  CONVERT_GRAPHIC = 'convert-graphic';

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

  procedure TDAIToolCLI.DoRun;
  var
    ErrorMsg: string;
    opts, argPaths, paths: TStringList;
    inType, outType: integer;
    t: string;
    path, inPath, inDir, outPath, outTyp: string;
    cmd, i: integer;
    SR: TSearchRec;
    dither: boolean;
    optimize: boolean;
    quantize: integer;
  begin
    paths := TStringList.Create;
    argPaths := TStringList.Create;
    opts := TStringList.Create;
    try
      // Pre processing of parameters
      ErrorMsg := CheckOptions('hc:o:t:g:q::dz', ['help', CONVERT_TO + ':', 'output:', 'input-type:', CONVERT_GRAPHIC + ':', 'quantize::', 'dither', 'optimize'], opts, argPaths);
      if ErrorMsg <> '' then begin
        WriteError(ErrorMsg);
        Exit;
      end;
      if HasOption('h', 'help') then begin
        WriteHelp();
        Exit;
      end;
      //
      cmd := 0;
      outTyp := GetOptionValue('c', CONVERT_TO);
      if (outTyp <> '') then begin
        cmd := 1;
      end
      else begin
        outTyp := GetOptionValue('g', CONVERT_GRAPHIC);
        if (outTyp <> '') then begin
          cmd := 2;
        end;
      end;
      if (cmd = 0) then begin
        WriteError();
        exit;
      end;
      outType := IndexOfSaveFilter(outTyp);
      if (outType < 0) then begin
        WriteError();
        exit;
      end;
      outPath := GetOptionValue('o', 'output');
      t := GetOptionValue('t', 'input-type');
      inType := IndexOfLoadFilter(t);
      optimize := HasOption('z', 'optimize');
      dither := HasOption('d', 'dither');
      if HasOption('q', 'quantize') then begin
        t := GetOptionValue('q', 'quantize');
        if (t = '') then begin
          t := '256';
        end;
      end
      else begin
        t := '0';
      end;
      quantize := StrToIntDef(t, 0);
      if (cmd = 2) then begin
        writeln('Dither: ', dither, ' quantization: ', quantize, ' optimize: ', optimize);
      end;
      //
      for i := 0 to argPaths.Count - 1 do begin
        inPath := argPaths[i];
        inDir := ExtractFileDir(inPath);
        if FindFirst(inPath, faAnyFile and not faDirectory, SR) = 0 then begin
          repeat
            path := inDir + '\' + SR.Name;
            paths.Add(path);
          until FindNext(SR) <> 0;
          findclose(SR);
        end;
      end;
      case cmd of
        1: begin
          convertTo(paths, inType, outPath, outType);
        end;
        2: begin
          convertGraphic(paths, outPath, outType, quantize, dither, optimize);
        end;
      end;
      //
      Terminate(0);
    finally
      FreeAndNil(argPaths);
      FreeAndNil(opts);
      FreeAndNil(paths);
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

  function TDAIToolCLI.getOutPath(const outPath, def: string; force: boolean): string;
  var
    base: string;
    i: integer;
  begin
    if (outPath = '') then begin
      Result := def;
    end
    else begin
      Result := outPath;
    end;
    base := ExtractFilePath(Result) + ChangeFileExt(ExtractFileName(Result), '') + '.%.3d' + ExtractFileExt(Result);
    if not force then begin
      Result := ExtractFilePath(Result) + ChangeFileExt(ExtractFileName(Result), '') + ExtractFileExt(Result);
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

  function TDAIToolCLI.convertTo(const paths: TStrings; inType: integer; outPath: string; outType: integer): boolean;
  var
    ext, outName: string;
    loadFilter, saveFilter: PFilter;
    s: RSegment;
    i: integer;
    inPath: string;
  begin
    Result := False;
    for i := 0 to paths.Count - 1 do begin
      inPath := paths[i];
      if (inType < 0) then begin
        ext := ExtractFileExt(inPath);
        if (length(ext) > 1) then begin
          ext := copy(ext, 2, length(ext) - 1);
        end;
        inType := IndexOfLoadFilter(ext);
        if (inType < 0) then begin
          Exit;
        end;
      end;
      loadFilter := FindLoadFilter(inType);
      saveFilter := FindSaveFilter(outType);
      outName := getOutPath(outPath, ChangeFileExt(inPath, '.' + saveFilter^.ext), paths.Count > 1);
      Write('Converting ' + inPath + ' [' + loadFilter^.displayName + '] to ' + outName + ' [' + saveFilter^.displayName + ']: ');
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
    Result := True;
    Writeln('done!');
  end;

  function TDAIToolCLI.convertGraphic(const paths: TStrings; outPath: string; outType: integer; quantize: integer; dither: boolean; optimize: boolean): boolean;
  var
    outName: string;
    saveFilter: PFilter;
    s: RSegment;
    image: TFPCustomImage;
    i: integer;
    inPath: string;
  begin
    Result := False;
    for i := 0 to paths.Count - 1 do begin
      inPath := paths[i];
      saveFilter := FindSaveFilter(outType);
      outName := getOutPath(outPath, ChangeFileExt(inPath, '.' + saveFilter^.ext), paths.Count > 1);
      Write('Converting ' + inPath + ' to ' + outName + ' [' + saveFilter^.displayName + ']: ');
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
      finally
        if (image <> nil) then begin
          FreeAndNil(image);
        end;
      end;
      Segment_writeMetadata(s, getMetadataPath(outPath));
    end;
    Result := True;
    Writeln('done!');
  end;

  constructor TDAIToolCLI.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TDAIToolCLI.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TDAIToolCLI.WriteHelp();
  var
    i: integer;
  begin
    { add your help code here }
    writeln('Usage:');
    writeln('  HELP -> ', ExtractFileName(ExeName), ' -h');
    writeln('  CONVERT -> ', ExtractFileName(ExeName), ' --', CONVERT_TO, ' save_type [--input-type load_type] [--output outputfile] inputfile1 inoutfile2 ...');
    writeln('  GRAPHIC -> ', ExtractFileName(ExeName), ' --', CONVERT_GRAPHIC, ' save_type [--output outputfile] [--dither] [--quantize[:col]]inputfile1 inoutfile2 ...');
    writeln();
    writeln('-o --output (if missing is input file with extension changed)');
    writeln('-t --input-file (if missing is input file extension)');
    writeln('-d --dither');
    writeln('-q[:col] --quantize[:col]');
    writeln('-z --optimize');
    for i := low(LOAD_FILTERS) to high(LOAD_FILTERS) do begin
      writeln('  ', LOAD_FILTERS[i].ext, ' -> ', StringReplace(LOAD_FILTERS[i].displayName, ' ', '_', [rfReplaceAll]));
    end;
    writeln();
    writeln('-c --', CONVERT_TO);
    writeln('-g --', CONVERT_GRAPHIC);
    for i := low(SAVE_FILTERS) to high(SAVE_FILTERS) do begin
      writeln('  ', StringReplace(SAVE_FILTERS[i].displayName, ' ', '_', [rfReplaceAll]), ' -> ', SAVE_FILTERS[i].ext);
    end;
    Terminate(0);
  end;

var
  Application: TDAIToolCLI;
begin
  Application := TDAIToolCLI.Create(nil);
  Application.Title := 'DAI tools CLI';
  Application.Run;
  Application.Free;
end.
