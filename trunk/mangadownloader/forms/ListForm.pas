unit ListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TImportList }

  TImportList = class(TForm)
    mmList: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ImportList: TImportList;

implementation

{$R *.lfm}

{ TImportList }

procedure TImportList.FormCreate(Sender: TObject);
begin
  Hide;
end;

end.

