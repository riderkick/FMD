{
        File: logform.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit logform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, lclintf;

type

  { TLog }

  TLog = class(TForm)
    btVisit: TButton;
    mmLog: TMemo;
    pnIE: TPanel;
    procedure btVisitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowLog;
  end;

var
  Log: TLog;

implementation

{$R *.lfm}

uses
  baseunit;

{ TLog }

procedure TLog.FormCreate(Sender: TObject);
begin
  Visible:= FALSE;
end;

procedure TLog.btVisitClick(Sender: TObject);
begin
  OpenURL('http://akarink.wordpress.com/');
end;

procedure TLog.ShowLog;
var
  l: TStringList;
begin
  if FileExists(WORK_FOLDER + 'note.txt') then
  begin
    l:= TStringList.Create;
    l.LoadFromFile(WORK_FOLDER + 'note.txt');
    if MessageDlg('', l.Text, mtInformation, [mbYes, mbNo], 0) = mrYes then
    begin
      OpenURL('https://akarink.wordpress.com/');
    end;
    DeleteFile(WORK_FOLDER + 'note.txt');
    l.Free;
  end;
  mmLog.Lines.Create;
  mmLog.Lines.LoadFromFile(WORK_FOLDER + LOG_FILE);
  Visible:= TRUE;
  DeleteFile(WORK_FOLDER + LOG_FILE);
end;

end.

