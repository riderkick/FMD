unit frmShutdownCounter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, LCLType;

type
  TExitType = (etExit, etShutdown, etHibernate);

  { TShutdownCounterForm }

  TShutdownCounterForm = class(TForm)
    Bevel1 : TBevel;
    btNow : TBitBtn;
    btAbort: TBitBtn;
    itCounter: TIdleTimer;
    lblMessage: TLabel;
    pnBottom : TPanel;
    pnTop: TPanel;
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
    ExitType: TExitType;
    { public declarations }
  end;

var
  ShutdownCounterForm: TShutdownCounterForm;
  { TODO 3 -oCholif -cT : need translation }
  SShutdown: String = 'System will shutdown in';
  SHibernate: String = 'System will hibernate in';
  SExit: String = 'FMD will exit in';
  SSecond: String = 'second.';

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
  ExitType := etExit;
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
begin
  case ExitType of
    etShutdown: lblMessage.Caption := SShutdown;
    etHibernate: lblMessage.Caption := SHibernate;
    etExit: lblMessage.Caption := SExit;
  end;
  WaitCounter := WaitTimeout;
  lblMessage.Caption := lblMessage.Caption + Format(' %d ', [WaitCounter]) + SSecond;
  WaitCounterOK := False;
  btAbort.SetFocus;
  itCounter.Enabled := True;
end;

procedure TShutdownCounterForm.itCounterStartTimer(Sender: TObject);
begin
  WaitCounter := WaitTimeout;
end;

procedure TShutdownCounterForm.itCounterTimer(Sender: TObject);
begin
  Dec(WaitCounter);
  case ExitType of
    etShutdown: lblMessage.Caption := SShutdown;
    etHibernate: lblMessage.Caption := SHibernate;
    etExit: lblMessage.Caption := SExit;
  end;
  lblMessage.Caption := lblMessage.Caption + Format(' %d ', [WaitCounter]) + SSecond;
  if WaitCounter = 0 then
  begin
    itCounter.Enabled := False;
    WaitCounterOK := True;
    Close;
  end;
end;

end.

