{
        File: silentthreads.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit silentthreads;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, Controls, IniFiles, baseunit, data, fgl, downloads,
  Graphics, Process, lclintf;

type
  // for "Download all" feature
  TSilentThread = class(TThread)
  protected
    procedure   CallMainFormAfterChecking; virtual;
    procedure   CallMainFormIncreaseThreadCount; virtual;
    procedure   CallMainFormDecreaseThreadCount; virtual;
    procedure   Execute; override;
  public
    Info        : TMangaInformation;
    isTerminated,
    isSuspended : Boolean;
    // manga information from main thread
    title,
    website, URL: String;

    constructor Create;
    destructor  Destroy; override;
  end;

  // for "Add to Favorites" feature
  TAddToFavSilentThread = class(TSilentThread)
  protected
    procedure   CallMainFormAfterChecking; override;
    procedure   CallMainFormDecreaseThreadCount; override;
  public
  end;

implementation

uses
  mainunit;

// ----- TSilentThread -----

procedure   TSilentThread.CallMainFormAfterChecking;
var
  hh, mm, ss, ms,
  day, month, year: Word;
  s, s1 : String;
  i, pos: Cardinal;
begin
  if Info.mangaInfo.numChapter = 0 then exit;
  with MainForm do
  begin
    // add a new download task
    DLManager.AddTask;
    pos:= DLManager.containers.Count-1;
    DLManager.containers.Items[pos].mangaSiteID:= GetMangaSiteID(website);

    for i:= 0 to Info.mangaInfo.numChapter-1 do
    begin
      // generate folder name
       s:= CustomRename(OptionCustomRename,
                       Info.mangaInfo.website,
                       Info.mangaInfo.title,
                       Info.mangaInfo.chapterName.Strings[i],
                       Format('%.4d', [i+1]),
                       cbOptionPathConvert.Checked);
      DLManager.containers.Items[pos].chapterName .Add(s);
      DLManager.containers.Items[pos].chapterLinks.Add(Info.mangaInfo.chapterLinks.Strings[i]);
    end;

    DLManager.containers.Items[pos].downloadInfo.Status:= stWait;
    DLManager.containers.Items[pos].Status:= STATUS_WAIT;

    DLManager.containers.Items[pos].currentDownloadChapterPtr:= 0;
    DLManager.containers.Items[pos].downloadInfo.title  := Info.mangaInfo.title;
    DLManager.containers.Items[pos].downloadInfo.Website:= website;
    if edSaveTo.Text = '' then
      edSaveTo.Text:= options.ReadString('saveto', 'SaveTo', '');
    s:= CorrectFile(edSaveTo.Text);
    if s[Length(s)] = '/' then
      Delete(s, Length(s), 1);

    // save to
    if cbOptionGenerateMangaFolderName.Checked then
    begin
      if NOT cbOptionPathConvert.Checked then
        s:= s + '/' + RemoveSymbols(Info.mangaInfo.title)
      else
        s:= s + '/' + RemoveSymbols(UnicodeRemove(Info.mangaInfo.title));
    end;
    DLManager.containers.Items[pos].downloadInfo.SaveTo:= s;

    // time
    DecodeDate(Now, year, month, day);
    DecodeTime(Time, hh, mm, ss, ms);
    DLManager.containers.Items[pos].downloadInfo.dateTime:= IntToStr(Month)+'/'+IntToStr(Day)+'/'+IntToStr(Year)+' '+IntToStr(hh)+':'+IntToStr(mm)+':'+IntToStr(ss);

    UpdateVtDownload;

    DLManager.Backup;
    DLManager.CheckAndActiveTask;

    // generate downloaded chapters
    s:= '';
    if Info.mangaInfo.chapterLinks.Count = 0 then exit;
    for i:= 0 to Info.mangaInfo.chapterLinks.Count-1 do
    begin
      s:= s+IntToStr(i) + SEPERATOR;
    end;
    if s <> '' then
      DLManager.AddToDownloadedChaptersList(Info.mangaInfo.website + URL, s);
  end;
end;

procedure   TSilentThread.CallMainFormIncreaseThreadCount;
begin
  Inc(MainForm.currentActiveSilentThreadCount);
end;

procedure   TSilentThread.CallMainFormDecreaseThreadCount;
begin
  with MainForm do
  begin
    Dec(silentThreadCount);
    Dec(currentActiveSilentThreadCount);
    // change status
    if silentThreadCount > 0 then
      sbMain.Panels[1].Text:= 'Loading: '+IntToStr(silentThreadCount)
    else
      sbMain.Panels[1].Text:= '';
  end;
end;

procedure   TSilentThread.Execute;
var
  times: Cardinal;
begin
  while isSuspended do Sleep(32);

  while MainForm.currentActiveSilentThreadCount > 2 do
    Sleep(250);
  Synchronize(CallMainFormIncreaseThreadCount);

  // some of the code was taken from subthreads's GetMangaInfo
  // since it's multi-thread, we cannot call IE for fetching info from Batoto
  if website = GEHENTAI_NAME then
    times:= 4
  else
    times:= 3;

  Info.mangaInfo.title:= title;
  if Info.GetInfoFromURL(website, URL, times)<>NO_ERROR then
  begin
    Info.Free;
    exit;
  end;
  Synchronize(CallMainFormAfterChecking);
  Synchronize(CallMainFormDecreaseThreadCount);
end;

constructor TSilentThread.Create;
begin
  isSuspended    := TRUE;
  isTerminated   := FALSE;
  FreeOnTerminate:= TRUE;
  Info:= TMangaInformation.Create;

  inherited Create(FALSE);
end;

destructor  TSilentThread.Destroy;
begin
  Info.Free;
  isTerminated:= TRUE;
  inherited Destroy;
end;

// ----- TAddToFavSilentThread -----

procedure   TAddToFavSilentThread.CallMainFormAfterChecking;
var
  s, s2: String;
  i    : Cardinal;
begin
  with MainForm do
  begin
    if edSaveTo.Text = '' then
      edSaveTo.Text:= options.ReadString('saveto', 'SaveTo', '');
    s:= CorrectFile(edSaveTo.Text);
    if s[Length(s)] = '/' then
      Delete(s, Length(s), 1);

    if cbOptionGenerateMangaFolderName.Checked then
    begin
      if NOT cbOptionPathConvert.Checked then
        s:= s + '/' + RemoveSymbols(title)
      else
        s:= s + '/' + RemoveSymbols(UnicodeRemove(title));
    end;

    s2:= '';
    if (Info.mangaInfo.numChapter > 0) AND (website = MANGASTREAM_NAME) then
    begin
      for i:= 0 to Info.mangaInfo.numChapter-1 do
        s2:= s2 + Info.mangaInfo.chapterLinks.Strings[i] + SEPERATOR;
    end;

    favorites.Add(title,
                  IntToStr(Info.mangaInfo.numChapter),
                  s2,
                  website,
                  s,
                  URL);
    UpdateVtFavorites;
  end;
end;

procedure   TAddToFavSilentThread.CallMainFormDecreaseThreadCount;
begin
  inherited;
  Dec(MainForm.silentAddToFavThreadCount);
end;

end.

