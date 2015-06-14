unit frmShutdownCounter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, LCLType, DefaultTranslator, uBaseUnit;

type

  { TShutdownCounterForm }

  TShutdownCounterForm = class(TForm)
    Bevel1 : TBevel;
    btNow : TBitBtn;
    btAbort: TBitBtn;
    itCounter: TIdleTimer;
    lblMessage: TLabel;
    pnBottom : TPanel;
    procedure btAbortClick(Sender: TObject);
    procedure btNowClick(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState
      );
    procedure FormShow(Sender: TObject);
    procedure itCounterStartTimer(Sender: TObject);
    procedure itCounterTimer(Sender: TObject);
  private
    WaitCounter: Integer;
    WaitCounterOK: Boolean;
    { private declarations }
  public
    WaitTimeout: Integer;
    frmExitType: TFMDDo;
    { public declarations }
  end;

var
  ShutdownCounterForm: TShutdownCounterForm;

resourcestring
  RS_LblMessageShutdown = 'System will shutdown in %d second.';
  RS_LblMessageHibernate = 'System will hibernate in %d second.';
  RS_LblMessageExit = 'FMD will exit in %d second.';

implementation

{$R *.lfm}

{ TShutdownCounterForm }

procedure TShutdownCounterForm.btAbortClick(Sender: TObject);
begin
  WaitCounterOK := False;
  Close;
end;

procedure TShutdownCounterForm.btNowClick(Sender : TObject);
begin
  WaitCounterOK := True;
  Close;
end;

procedure TShutdownCounterForm.FormClose(Sender : TObject;
  var CloseAction : TCloseAction);
begin
  if WaitCounterOK then
    ModalResult := mrOK
  else
    ModalResult := mrAbort;
end;

procedure TShutdownCounterForm.FormCreate(Sender: TObject);
begin
  WaitTimeout := 60;
  WaitCounter := WaitTimeout;
  frmExitType := DO_NOTHING;
end;

procedure TShutdownCounterForm.FormKeyDown(Sender : TObject; var Key : Word;
  Shift : TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    WaitCounterOK := False;
    Close;
  end;
end;

procedure TShutdownCounterForm.FormShow(Sender: TObject);
var
  s: String = '';
begin
  case frmExitType of
    DO_POWEROFF  : s := RS_LblMessageShutdown;
    DO_HIBERNATE : s := RS_LblMessageHibernate;
    DO_EXIT      : s := RS_LblMessageExit;
  end;
  WaitCounter := WaitTimeout;
  lblMessage.Caption := Format(s, [WaitCounter]);
  WaitCounterOK := False;
  btAbort.SetFocus;
  itCounter.Enabled := True;
end;

procedure TShutdownCounterForm.itCounterStartTimer(Sender: TObject);
begin
  WaitCounter := WaitTimeout;
end;

procedure TShutdownCounterForm.itCounterTimer(Sender: TObject);
var
  s: String = '';
begin
  Dec(WaitCounter);
  case frmExitType of
    DO_POWEROFF  : s := RS_LblMessageShutdown;
    DO_HIBERNATE : s := RS_LblMessageHibernate;
    DO_EXIT      : s := RS_LblMessageExit;
  end;
  lblMessage.Caption := Format(s, [WaitCounter]);
  if WaitCounter = 0 then
  begin
    itCounter.Enabled := False;
    WaitCounterOK := True;
    Close;
  end;
end;

end.

