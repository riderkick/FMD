unit frmLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, Buttons, LogTreeView, MultiLog;

type

  { TFormLogger }

  TFormLogger = class(TForm)
    btnClearLog: TBitBtn;
    ckStayOnTop: TCheckBox;
    lbLogLimit: TLabel;
    seLogLimit: TSpinEdit;
    tmClearLog: TTimer;
    tvLog: TLogTreeView;
    procedure btnClearLogClick(Sender: TObject);
    procedure ckStayOnTopChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmClearLogTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormLogger: TFormLogger;

implementation

{$R *.lfm}

{ TFormLogger }

procedure TFormLogger.tmClearLogTimer(Sender: TObject);
begin
  if tvLog.Items.TopLvlCount > seLogLimit.Value then
    try
      tvLog.BeginUpdate;
      while tvLog.Items.TopLvlCount > seLogLimit.Value do
        tvLog.Items.TopLvlItems[0].Delete;
    finally
      tvLog.EndUpdate;
    end;
end;

procedure TFormLogger.FormCreate(Sender: TObject);
begin
  Logger.Channels.Add(tvLog.Channel);
end;

procedure TFormLogger.ckStayOnTopChange(Sender: TObject);
begin
  if ckStayOnTop.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TFormLogger.btnClearLogClick(Sender: TObject);
begin
  tvLog.Clear;
end;

procedure TFormLogger.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TFormLogger.FormDestroy(Sender: TObject);
begin
  Logger.Channels.Remove(tvLog.Channel);
end;

end.

