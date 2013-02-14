unit subthreads;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, Controls, IniFiles, baseunit, data, fgl, downloads,
  Graphics;

type
  TSubThread = class(TThread)
  protected
    procedure   Execute; override;
    procedure   DoGetInfos;
    procedure   CallMainFormGetInfos;
  public
    cover            : TPicture;
    isHasCover,
    isCanStop,
    isTerminated,
    isSuspended : Boolean;
    OnShowInformation: procedure of object;

    website, link: String;
    isGetInfos   : Boolean;
    Info         : TMangaInformation;
    boolResult   : Boolean;

    constructor Create;
    destructor  Destroy; override;
  end;

implementation

uses
  mainunit;

constructor TSubThread.Create;
begin
  isCanStop      := FALSE;
  isSuspended    := TRUE;
  isTerminated   := FALSE;
  FreeOnTerminate:= TRUE;
  isGetInfos     := FALSE;
  Cover          := TPicture.Create;

 // ;
  inherited Create(FALSE);
end;

destructor  TSubThread.Destroy;
begin
  Cover.Free;
  isTerminated:= TRUE;
  inherited Destroy;
end;

procedure   TSubThread.DoGetInfos;
{var
  root: String;}

  function  GetMangaInfo(const website, URL: String): Boolean;
  begin
    Result:= FALSE;
    Info.isGenerateFolderChapterName:= MainForm.cbOptionGenerateChapterName.Checked;
    Info.isRemoveUnicode:= MainForm.cbOptionPathConvert.Checked;
    Info.mangaInfo.title:= MainForm.dataProcess.Title.Strings[MainForm.dataProcess.filterPos.Items[MainForm.vtMangaList.FocusedNode.Index]];
    if Info.GetInfoFromURL(URL, website, 0)<>NO_ERROR then
    begin
     // Info.Free;
      exit;
    end;
    // fixed
    Info.SyncInfoToData(MainForm.DataProcess, MainForm.DataProcess.filterPos.Items[MainForm.vtMangaList.FocusedNode.Index]);
    Result:= TRUE;
  end;

begin
  if MainForm.cbSelectManga.ItemIndex < 0 then exit;
 // if NOT MainForm.vtMangaList.Focused then exit;

  // ---------------------------------------------------

  if NOT GetMangaInfo(link, website) then
  begin
    MessageDlg('', stDlgCannotGetMangaInfo,
               mtInformation, [mbYes], 0);
    exit;
  end;

  cover.Clear;
  boolResult:= GetPage(TObject(cover), Info.mangaInfo.coverLink, 1);
end;

procedure   TSubThread.CallMainFormGetInfos;
begin
  TransferMangaInfo(MainForm.mangaInfo, Info.mangaInfo);
  MainForm.ShowInformation;
  if boolResult then
  begin
    try
      MainForm.imCover.Picture.Assign(cover);
    except
      cover.Clear;
    end;
  end;
  Info.Free;
end;

procedure   TSubThread.Execute;
begin
  while isSuspended do Sleep(32);
  while NOT Terminated do
  begin
    isCanStop:= FALSE;
    if isGetInfos then
    begin
      Info:= TMangaInformation.Create;
      DoGetInfos;
      Synchronize(CallMainFormGetInfos);
      isGetInfos:= FALSE;
    end;
    isCanStop:= TRUE;
    Sleep(64);
  end;
end;

end.

