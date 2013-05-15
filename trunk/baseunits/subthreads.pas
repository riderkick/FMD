{
        File: subthreads.pas
        License: GPLv2
        This unit is part of Free Manga Downloader
}

unit subthreads;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, Controls, IniFiles, baseunit, data, fgl, downloads,
  Graphics, Process;

type
  TSubThread = class(TThread)
  protected
    procedure   Execute; override;
    procedure   DoGetInfos;
    procedure   CallMainFormGetInfos;
    procedure   CallMainFormUpdate;
  public
    isFirst     : Boolean;
    mangaListPos: Integer;
    cover       : TPicture;
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

var
  LRevision: Cardinal;
  LVersion : String;

constructor TSubThread.Create;
begin
  isFirst        := TRUE;
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

  function  GetMangaInfo(const URL, website: String): Boolean;
  var
    selectedWebsite: String;
    filterPos      : Cardinal;
  begin
    Result:= FALSE;

    if mangaListPos <> -1 then
    begin
      filterPos:= MainForm.dataProcess.filterPos.Items[mangaListPos];
      Info.mangaInfo.title:= MainForm.dataProcess.Title.Strings[filterPos];
    end;

    Info.isGenerateFolderChapterName:= MainForm.cbOptionGenerateChapterName.Checked;
    Info.isRemoveUnicode:= MainForm.cbOptionPathConvert.Checked;

    if Info.GetInfoFromURL(website, URL, 0)<>NO_ERROR then
    begin
     // Info.Free;
      exit;
    end;
    // fixed
    if mangaListPos <> -1 then
    begin
      if website = MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex] then
        Info.SyncInfoToData(MainForm.DataProcess, filterPos);
    end;
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

procedure   TSubThread.CallMainFormUpdate;
var
  Process: TProcess;
begin
  if MessageDlg('', Format(stDlgNewVersion, [LVersion, LRevision]),
                        mtInformation, [mbYes, mbNo], 0) = mrYes then
  begin
    Process:= TProcess.Create(nil);
    Process.CommandLine:= oldDir + 'updater.exe 1';
    Process.Execute;
    MainForm.CloseNow;
    Halt;
  end;
end;

procedure   TSubThread.Execute;
var
  l: TStringList;
  i: Cardinal;

begin
  LRevision:= 0;
  while isSuspended do Sleep(32);
  while NOT Terminated do
  begin
    if isFirst then
    begin
      Sleep(2000);
      isFirst:= FALSE;
      l:= TStringList.Create;

      l.NameValueSeparator:= '=';
      if (GetPage(TObject(l), UPDATE_URL + 'version.txt', 0)) AND (l.Count > 0) then
      begin
        for i:= 0 to l.Count-1 do
        begin
          if l.Names[i] = 'Revision' then
            LRevision := StrToInt(l.ValueFromIndex[i]);
          if l.Names[i] = 'Version' then
            LVersion  := l.ValueFromIndex[i];
        end;


        if LRevision > Revision then
        begin
          Synchronize(CallMainFormUpdate);
        end;
      end;

      l.Free;
    end;

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

