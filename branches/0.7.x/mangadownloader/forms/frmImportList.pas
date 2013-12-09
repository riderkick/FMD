unit frmImportList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmImportList }

  TfrmImportList = class(TForm)
    mmList: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ImportList: TfrmImportList;

implementation

{$R *.lfm}

{ TfrmImportList }

procedure TfrmImportList.FormCreate(Sender: TObject);
begin
  Hide;
end;

end.

