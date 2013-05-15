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
begin
  mmLog.Lines.Create;
  mmLog.Lines.LoadFromFile(WORK_FOLDER + LOG_FILE);
  Visible:= TRUE;
  DeleteFile(WORK_FOLDER + LOG_FILE);
end;

end.

