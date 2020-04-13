unit frmDialogYesNo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, FMDForms;

type

  { TfrmDialogYN }

  TfrmDialogYN = class(TFMDForm)
    btYes: TButton;
    btNo: TButton;
    lbMessage: TLabel;
    mMessages: TMemo;
  private

  public

  end;

var
  frmDialogYN: TfrmDialogYN;

implementation

{$R *.lfm}

end.

