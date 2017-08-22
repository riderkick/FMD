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

unit SimpleExceptionForm;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  windows,
  {$endif}
  Forms, Controls, StdCtrls, ExtCtrls, Buttons, Classes;

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
    normalheight,
    expandedheight: Integer;
  public
    { public declarations }
  end;

implementation

uses SimpleException;

{$R *.lfm}

{ TSimpleExceptionForm }

procedure TSimpleExceptionForm.ButtonDetailsClick(Sender: TObject);
begin
  if ClientHeight <= normalheight then
    ClientHeight := expandedheight
  else
    ClientHeight := normalheight;
end;

procedure TSimpleExceptionForm.ButtonTerminateClick(Sender: TObject);
begin
  {$ifdef windows}
  TerminateProcess(GetCurrentProcess, 0);
  {$else}
  halt;
  {$endif}
end;

procedure TSimpleExceptionForm.FormCreate(Sender: TObject);
begin
  Icon.Assign(Application.Icon);
  IconException.Visible := IconException.Picture.Graphic <> nil;
  Caption := SExceptionDialogTitle;
  LabelExceptionCaption.Caption := SExceptionCaption;
  ButtonDetails.Caption := TCaption(SButtonDetails);
  ButtonTerminate.Caption := TCaption(SButtonTerminate);
  ButtonContinue.Caption := TCaption(SButtonContinue);
  CheckBoxIgnoreException.Caption := SCheckBoxIgnoreException;
  CheckBoxIgnoreException.Checked := False;
end;

procedure TSimpleExceptionForm.FormShow(Sender: TObject);
begin
  normalheight := min(PanelTop.Height + ButtonDetails.Height + (6 * 2), Screen.Height);
  expandedheight := normalheight + 300;
  Constraints.MinWidth := CheckBoxIgnoreException.Width + ButtonDetails.Width + ButtonTerminate.Width + ButtonContinue.Width + (6 * 5);
  Constraints.MinHeight := normalheight;
  ClientWidth := min(max(PanelTop.Width, Constraints.MinWidth), Screen.Width);
  ClientHeight := normalheight;
  PanelTop.AutoSize := False;
  PanelTop.Width := ClientWidth;
  PanelTop.Anchors := [akLeft, akRight, akTop];
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

