program DTcli;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  libDAI,
  libTools,
  uFilters;

type

  { TDAIToolCLI }

  TDAIToolCLI = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TDAIToolCLI }

  procedure TDAIToolCLI.DoRun;
  var
    ErrorMsg: string;
    opts, paths: TStringList;
    inType, outType: integer;
    inPath, inTyp: string;
    outPath, outTyp: string;
    loadFilter, saveFilter: PFilter;
    s: RSegment;
    Result: boolean;
  begin
    s.Name := '';
    paths := TStringList.Create;
    opts := TStringList.Create;
    inType := -1;
    outType := -1;
    // parse parameters
    ErrorMsg := CheckOptions('hc:o:t:', ['help', 'convert:', 'output:', 'input-type:'], opts, paths);
    if ErrorMsg <> '' then begin
      writeln(ErrorMsg);
      Terminate;
      Exit;
    end;
    if HasOption('h', 'help') then begin
      WriteHelp;
      Terminate;
      Exit;
    end;
    outTyp := GetOptionValue('c', 'convert');
    outPath := GetOptionValue('o', 'output');
    inTyp := GetOptionValue('t', 'input-type');
    if (paths.Count = 1) then begin
      inPath := paths[0];
    end
    else begin
      inPath := '';
    end;
    if (inTyp = '') then begin
      inTyp := ExtractFileExt(inPath);
      if (length(inTyp) > 1) then begin
        inTyp := copy(inTyp, 2, length(inTyp) - 1);
      end;
    end;
    outType := IndexOfSaveFilter(outTyp);
    inType := IndexOfLoadFilter(inTyp);
    if (inPath = '') or (outType < 0) or (inType < 0) then begin
      writeln('Invalid combination of parameters');
      Terminate;
      Exit;
    end;
    loadFilter := FindLoadFilter(inType);
    saveFilter := FindSaveFilter(outType);
    if (outPath = '') then begin
      outPath := ChangeFileExt(inPath, '.' + saveFilter^.ext);
    end;
    writeln('Converting ' + inPath + ' [' + loadFilter^.displayName + '] to ' + outPath + ' [' + saveFilter^.displayName + ']');
    Result := loadFilter^.proc(inPath, s);
    if not Result then begin
      Writeln(DAI_lastError());
      Terminate;
      Exit;
    end;
    Result := saveFilter^.proc(outPath, s);
    if not Result then begin
      Writeln(DAI_lastError());
      Terminate;
      exit;
    end;
    Writeln('Conversion done!');
    Terminate;
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

  procedure TDAIToolCLI.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExtractFileName(ExeName), ' -h');
  end;

var
  Application: TDAIToolCLI;
begin
  Application := TDAIToolCLI.Create(nil);
  Application.Title := 'DAI tools CLI';
  Application.Run;
  Application.Free;
end.

