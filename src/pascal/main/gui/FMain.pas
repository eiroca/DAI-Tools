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
unit FMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ActnList, ShellCtrls, ComCtrls, EditBtn, ExtCtrls, Buttons;

type

  { TfmMain }

  TfmMain = class(TForm)
    aConvert: TAction;
    aRefresh: TAction;
    aQuit: TAction;
    alMain: TActionList;
    bConvert: TBitBtn;
    cbInput: TComboBox;
    cbOutput: TComboBox;
    iInputFile: TFileNameEdit;
    iOutputFile: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    lvArchive: TShellListView;
    lvImport: TShellListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnConvert: TMenuItem;
    mnFile: TMenuItem;
    miQuit: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    sbStatus: TStatusBar;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    tsWorking: TTabSheet;
    tsArchive: TTabSheet;
    tvDAI: TShellTreeView;
    tvWorking: TShellTreeView;
    procedure aConvertExecute(Sender: TObject);
    procedure aQuitExecute(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShellListSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
  private
    function getInputPath(): string;
    function getOutputPath(const inPath: string; const outExt: string): string;
    procedure InitEnvironment();
    procedure InitCombo();
    procedure SetCurFile(const PathName: string);
    procedure SetStatus(const errMsg: string);
    procedure Refresh();
  public

  end;

var
  fmMain: TfmMain;

implementation

uses
  libTools, libDAI, libGraph;

{$R *.lfm}

const
  INPUT_TYPES: array of string = ('bin', 'sbin', 'dump', 'wav', 'png');
  INPUT_CALLS: array of DAI_func = (@DAI_loadBin, @DAI_loadSBin, @DAI_loadDump, @DAI_loadWAV, @DAI_loadPNG);
  OUTPUT_TYPE: array of string = ('bin', 'sbin', 'dump', 'png (full)', 'png (fast)', 'DAI (bin)', 'wav');
  OUTPUT_EXTS: array of string = ('bin', 'sbin', 'dump', 'png', 'png', 'DAI', 'wav');
  OUTPUT_CALL: array of DAI_func = (@DAI_saveBin, @DAI_saveSBin, @DAI_saveDump, @DAI_saveFullPNG, @DAI_savePNG, @DAI_saveDAIbin, @DAI_saveWAV);

  FONT_PATHNAME: array of string = ('DAI\ROMS\nch.bin', 'DAI\nch.bin', 'nch.bin');

{ TfmMain }

function TfmMain.getInputPath(): string;
begin
  Result := iInputFile.FileName;
end;

function TfmMain.getOutputPath(const inPath: string; const outExt: string): string;
begin
  Result := iOutputFile.FileName;
  if (Result = '') then begin
    Result := ChangeFileExt(inPath, '.' + outExt);
  end;
end;

procedure TfmMain.aQuitExecute(Sender: TObject);
begin
  Close();
end;

procedure TfmMain.aRefreshExecute(Sender: TObject);
begin
  Refresh();
end;

procedure TfmMain.aConvertExecute(Sender: TObject);
var
  fName: string;
  tIn, tOut: string;
  inPath, outPath: string;
  loadFunc, saveFunc: DAI_func;
  s: RSegment;
  Result: boolean;
begin
  inPath := iInputFile.FileName;
  if (inPath = '') then begin
    SetStatus('Missing input file');
    exit;
  end;
  fName := ExtractFileName(iInputFile.Text);
  tIn := cbInput.Caption;
  tOut := cbOutput.Caption;
  if (tIn = tOut) then begin
    SetStatus(Format('Invalid Conversion', [fName, tIn, tOut]));
    exit;
  end;
  loadFunc := INPUT_CALLS[cbInput.ItemIndex];
  saveFunc := OUTPUT_CALL[cbOutput.ItemIndex];
  tOut := OUTPUT_EXTS[cbOutput.ItemIndex];
  if (loadFunc = nil) or (saveFunc = nil) then begin
    SetStatus(Format('Conversion from %s to %s is not supported yet!', [tIn, tOut]));
    exit;
  end;
  outPath := getOutputPath(inPath, tOut);
  s.Name := ExtractFileName(outPath);
  s.size := 0;
  if (outPath = inPath) then begin
    outPath := outPath + '.' + tOut;
  end;
  Result := loadFunc(inPath, s);
  if not Result then begin
    SetStatus(DAI_lastError());
    exit;
  end;
  Result := saveFunc(outPath, s);
  if not Result then begin
    SetStatus(DAI_lastError());
    exit;
  end;
  Segment_writeMetadata(s, ExtractFileDir(outPath) + '\metadata.json');
  SetStatus(Format('Conversion of %s in %s done!', [fName, ExtractFilename(outPath)]));
  Refresh();
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  InitEnvironment();
  InitCombo();
  iInputFile.Caption := '';
  iOutputFile.Caption := '';
  PageControl1.ActivePageIndex := 0;
  tsArchive.Free;
end;

procedure TfmMain.FormActivate(Sender: TObject);
begin
  Refresh();
end;

procedure TfmMain.SetCurFile(const PathName: string);
var
  ext: string;
  ps: integer;
begin
  iInputFile.InitialDir := ExtractFileDir(PathName);
  iInputFile.FileName := PathName;
  ext := ExtractFileExt(PathName);
  if (length(ext) > 1) then begin
    ext := LowerCase(Copy(ext, 2, Length(ext) - 1));
    ps := cbInput.Items.IndexOf(ext);
    if (ps >= 0) then begin
      cbInput.ItemIndex := ps;
    end;
  end;
end;

procedure TfmMain.SetStatus(const errMsg: string);
begin
  sbStatus.SimpleText := errMsg;
end;

procedure TfmMain.Refresh();
var
  s: string;
begin
  s := lvImport.Root;
  lvImport.Root := '';
  lvImport.Root := s;
  s := tvDAI.Root;
  tvDAI.Root := '';
  tvDAI.Root := s;
end;

procedure TfmMain.ShellListSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
begin
  if Selected then begin
    SetCurFile((Sender as TShellListView).GetPathFromItem(Item));
  end;
end;

procedure TfmMain.InitEnvironment();
var
  basePath: string;
  p: string;
  fontOk: boolean;
begin
  basePath := ExtractFilePath(Application.ExeName);
  tvWorking.Root := '';
  tvWorking.Path := basePath + 'DAI\working';
  tvDAI.Root := basePath + 'DAI\archive';
  fontOk := False;
  for p in FONT_PATHNAME do begin
    if (FileExists(basePath + p) and DAI_initFont(basePath + p)) then begin
      SetStatus('Font loaded ' + p);
      fontOk := True;
      break;
    end;
  end;
  if (not fontOk) then begin
    SetStatus('Unable to load font');
  end;
end;

procedure TfmMain.InitCombo();
begin
  cbInput.Items.Clear;
  cbInput.Items.AddStrings(INPUT_TYPES);
  cbInput.ItemIndex := 0;
  cbOutput.Items.Clear;
  cbOutput.Items.AddStrings(OUTPUT_TYPE);
  cbOutput.ItemIndex := 0;
end;

end.
