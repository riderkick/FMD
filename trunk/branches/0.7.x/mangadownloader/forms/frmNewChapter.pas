unit frmNewChapter;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  TNewChapterResult = (ncrDownload, ncrQueue, ncrCancel);

  { TfrmNewChapter }

  TfrmNewChapter = class(TForm)
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

{ TfrmNewChapter }

procedure TfrmNewChapter.FormCreate(Sender: TObject);
begin
  btDownload.Caption:= stDownload;
  btQueue   .Caption:= stAddToQueue;
  btCancel  .Caption:= stCancel;
  Caption           := stNewChapterNotification;
end;

procedure TfrmNewChapter.btDownloadClick(Sender: TObject);
begin
  FFormResult:= ncrDownload;
end;

procedure TfrmNewChapter.btQueueClick(Sender: TObject);
begin
  FFormResult:= ncrQueue;
end;

procedure TfrmNewChapter.btCancelClick(Sender: TObject);
begin
  FFormResult:= ncrCancel;
end;

end.

