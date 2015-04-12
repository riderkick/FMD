unit uMessage;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls;

type

  { TfrmMessage }

  TfrmMessage = class(TForm)
    mmMessage: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmMessage: TfrmMessage;

implementation

{$R *.lfm}

end.

