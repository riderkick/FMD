{ SimpleException Dialog

  Copyright (C) 2014 Nur Cholif

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit USimpleExceptionForm;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, StdCtrls, ExtCtrls, Buttons;

type

  { TSimpleExceptionForm }

  TSimpleExceptionForm = class(TForm)
    ButtonContinue : TBitBtn;
    ButtonDetails: TBitBtn;
    ButtonTerminate : TBitBtn;
    CheckBoxIgnoreException: TCheckBox;
    IconException: TImage;
    LabelExceptionMessage: TLabel;
    LabelExceptionCaption: TLabel;
    MemoExceptionLog: TMemo;
    PanelButton : TPanel;
    PanelCenter: TPanel;
    PanelMessage: TPanel;
    PanelExceptionIcon: TPanel;
    PanelBottom: TPanel;
    PanelTop: TPanel;
    BevelPannelBottom: TShape;
    procedure ButtonContinueClick(Sender: TObject);
    procedure ButtonDetailsClick(Sender: TObject);
    procedure ButtonTerminateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

uses USimpleException;

{$R *.lfm}

{ TSimpleExceptionForm }

procedure TSimpleExceptionForm.ButtonDetailsClick(Sender: TObject);
begin
  if ClientHeight <= PanelCenter.Top + PanelCenter.ChildSizing.TopBottomSpacing then
    ClientHeight := PanelTop.Height + PanelBottom.Height + 200
  else
    ClientHeight := PanelTop.Height + PanelBottom.Height;
end;

procedure TSimpleExceptionForm.ButtonTerminateClick(Sender: TObject);
begin
  //Application.Terminate;
  halt(1);
end;

procedure TSimpleExceptionForm.FormCreate(Sender: TObject);
begin
  Icon.Assign(Application.Icon);
end;

procedure TSimpleExceptionForm.FormShow(Sender: TObject);
begin
  Caption := SExceptionDialogTitle;
  if IconException.Picture.Graphic <> nil then
    PanelExceptionIcon.Show
  else
    PanelExceptionIcon.Hide;
  LabelExceptionCaption.Caption := SExceptionCaption;
  ButtonDetails.Caption := TCaption(SButtonDetails);
  ButtonTerminate.Caption := TCaption(SButtonTerminate);
  ButtonContinue.Caption := TCaption(SButtonContinue);
  CheckBoxIgnoreException.Caption := SCheckBoxIgnoreException;
  CheckBoxIgnoreException.Checked := False;
  ClientHeight := PanelTop.Height + PanelBottom.Height;
  ClientWidth := CheckBoxIgnoreException.Width + ButtonDetails.Width +
                      ButtonTerminate.Width + ButtonContinue.Width +
                      (PanelBottom.ChildSizing.LeftRightSpacing * 3);
  Position := poDesktopCenter;
  Position := poMainFormCenter;
end;

procedure TSimpleExceptionForm.ButtonContinueClick(Sender: TObject);
begin
  Close;
end;

procedure TSimpleExceptionForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if CheckBoxIgnoreException.Checked then
    ModalResult := mrIgnore
  else
    ModalResult := mrClose;
end;

end.

