unit frmDialogYesNo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmDialogYN }

  TfrmDialogYN = class(TForm)
    btYes: TButton;
    btNo: TButton;
    mMessages: TMemo;
  private

  public

  end;

var
  frmDialogYN: TfrmDialogYN;

implementation

{$R *.lfm}

end.

