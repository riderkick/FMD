{
        File: logform.pas
        License: GPLv2
        This unit is part of Free Manga Downloader
}

unit logform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TLog }

  TLog = class(TForm)
    mmLog: TMemo;
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

procedure TLog.ShowLog;
var
  l: TStringList;
begin
  if FileExists(WORK_FOLDER + 'note.txt') then
  begin
    l:= TStringList.Create;
    l.LoadFromFile(WORK_FOLDER + 'note.txt');
    MessageDlg('', l.Text, mtInformation, [mbYes], 0);
    DeleteFile(WORK_FOLDER + 'note.txt');
    l.Free;
  end;
  mmLog.Lines.Create;
  mmLog.Lines.LoadFromFile(WORK_FOLDER + LOG_FILE);
  Visible:= TRUE;
  DeleteFile(WORK_FOLDER + LOG_FILE);
end;

end.

