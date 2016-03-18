unit frmAccountSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TAccountSetForm }

  TAccountSetForm = class(TForm)
    btOk: TBitBtn;
    btCancel: TBitBtn;
    cbWebsiteName: TComboBox;
    ckShowPassword: TCheckBox;
    edUsername: TEdit;
    edPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure btOkClick(Sender: TObject);
    procedure ckShowPasswordChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AccountSetForm: TAccountSetForm;

resourcestring
  RS_CantBeEmpty = 'Username or password can''t be empty!';

implementation

{$R *.lfm}

{ TAccountSetForm }

procedure TAccountSetForm.btOkClick(Sender: TObject);
begin
  if (edUsername.Text = '') or (edPassword.Text = '') then begin
    MessageDlg(RS_CantBeEmpty, mtError, [mbOK], 0);
    if edUsername.Text = '' then edUsername.SetFocus
    else edPassword.SetFocus;
  end
  else
    ModalResult := mrOK;
end;

procedure TAccountSetForm.ckShowPasswordChange(Sender: TObject);
begin
  if ckShowPassword.Checked then
    edPassword.PasswordChar:=#0
  else
    edPassword.PasswordChar:='*';
end;

end.

