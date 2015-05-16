{
        File: frmLog.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit frmLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  ExtCtrls, lclintf;

type

  { TLog }

  TLog = class(TForm)
    mmLog: TMemo;
    pnIE: TPanel;
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
  uBaseUnit;

{ TLog }

procedure TLog.FormCreate(Sender: TObject);
begin
  ShowLog;
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
  //mmLog.Lines.Create;
  mmLog.Lines.LoadFromFile(WORK_FOLDER + LOG_FILE);
  DeleteFile(WORK_FOLDER + LOG_FILE);
end;

end.

