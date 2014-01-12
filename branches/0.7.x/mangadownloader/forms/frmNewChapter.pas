{
        File: frmNewChapter.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit frmNewChapter;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  TNewChapterResult = (ncrDownload, ncrQueue, ncrCancel);

  { TNewChapter }

  TNewChapter = class(TForm)
    btDownload: TBitBtn;
    btCancel: TBitBtn;
    btQueue: TBitBtn;
    lbNotification: TLabel;
    mmMemo  : TMemo;
    procedure btCancelClick(Sender: TObject);
    procedure btDownloadClick(Sender: TObject);
    procedure btQueueClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    FFormResult: TNewChapterResult;
  private
    { private declarations }
  public
    { public declarations }
    property   FormResult: TNewChapterResult read FFormResult;
  end;

implementation

{$R *.lfm}

uses
  uBaseUnit;

{ TNewChapter }

procedure TNewChapter.FormCreate(Sender: TObject);
begin
  btDownload.Caption:= stDownload;
  btQueue   .Caption:= stAddToQueue;
  btCancel  .Caption:= stCancel;
  Caption           := stNewChapterNotification;
end;

procedure TNewChapter.btDownloadClick(Sender: TObject);
begin
  FFormResult:= ncrDownload;
end;

procedure TNewChapter.btQueueClick(Sender: TObject);
begin
  FFormResult:= ncrQueue;
end;

procedure TNewChapter.btCancelClick(Sender: TObject);
begin
  FFormResult:= ncrCancel;
end;

end.

