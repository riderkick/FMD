unit frmWebsiteOptionAdvanced;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  LCLProc, Grids, VirtualTrees, FMDOptions;

type

  { TWebsiteOptionAdvancedForm }

  TWebsiteOptionAdvancedForm = class(TForm)
    pcAdvanced: TPageControl;
    sgUserAgent: TStringGrid;
    sgCookies: TStringGrid;
    tsCookies: TTabSheet;
    tsUserAgent: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgUserAgentDblClick(Sender: TObject);
    procedure sgUserAgentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgUserAgentSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
  private
    { private declarations }
  public
    { public declarations }
    procedure LoadFromFileToGrid(const AGrid: TStringGrid; const ASection: String);
  end;

var
  WebsiteOptionAdvancedForm: TWebsiteOptionAdvancedForm;

implementation

{$R *.lfm}

{ TWebsiteOptionAdvancedForm }

procedure TWebsiteOptionAdvancedForm.FormCreate(Sender: TObject);
begin
  LoadFromFileToGrid(sgUserAgent, 'UserAgent');
  LoadFromFileToGrid(sgCookies, 'Cookies');
end;

procedure TWebsiteOptionAdvancedForm.FormDestroy(Sender: TObject);
begin
  advancedfile.UpdateFile;
end;

procedure TWebsiteOptionAdvancedForm.sgUserAgentDblClick(Sender: TObject);
begin
  if not (Sender is TStringGrid) then Exit;
  if TStringGrid(Sender).SelectedColumn.Index > 0 then
    TStringGrid(Sender).EditorMode := True;
end;

procedure TWebsiteOptionAdvancedForm.sgUserAgentKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not (Sender is TStringGrid) then Exit;
  if (Key = VK_RETURN) and (TStringGrid(Sender).SelectedColumn.Index = 0) then
    Key := 0;
end;

procedure TWebsiteOptionAdvancedForm.sgUserAgentSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  s: String;
begin
  if ACol = 0 then Exit;
  if not (Sender is TStringGrid) then Exit;
  if TStringGrid(Sender).Cells[ACol, ARow] = Value then Exit;
  if Sender = sgUserAgent then
    s := 'UserAgent'
  else if Sender = sgCookies then
    s := 'Cookies';
  advancedfile.WriteString(s, sgUserAgent.Cells[0, ARow], Value);
end;

procedure TWebsiteOptionAdvancedForm.LoadFromFileToGrid(const AGrid: TStringGrid;
  const ASection: String);
var
  s: TStringList;
  i: Integer;
begin
  if AGrid = nil then Exit;
  if ASection = '' then Exit;
  AGrid.Clear;
  s := TStringList.Create;
  try
    advancedfile.ReadSectionRaw(ASection, s);
    if s.Count > 0 then
    begin
      AGrid.RowCount := s.Count + 1;
      for i := 0 to s.Count - 1 do
      begin
        AGrid.Cells[0, i + 1] := s.Names[i];
        AGrid.Cells[1, i + 1] := s.ValueFromIndex[i];
      end;
      AGrid.AutoSizeColumn(0);
      AGrid.SortColRow(True, 0);
    end;
  finally
    s.Free;
  end;
end;

end.

