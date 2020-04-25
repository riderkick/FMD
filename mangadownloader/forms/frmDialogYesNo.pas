unit frmDialogYesNo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uBaseForms;

type

  { TfrmDialogYN }

  TfrmDialogYN = class(TBaseForm)
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

