{
        File: frmImportList.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit frmImportList;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls;

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

