{
        File: silentthreads.pas
        License: GPLv2
        This unit is part of Free Manga Downloader
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
    procedure   CallMainFormAddDownloadTask;
    procedure   CallMainFormDecreaseThreadCount;
    procedure   Execute; override;
  public
    Info        : TMangaInformation;
    isTerminated,
    isSuspended : Boolean;
    website, URL: String;

    constructor Create;
    destructor  Destroy; override;
  end;

implementation

uses
  mainunit;

// ----- TSilentThread -----

procedure   TSilentThread.CallMainFormAddDownloadTask;
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
      // generate folder name based on chapter name and numbering
      if (website <> GEHENTAI_NAME) AND
         (website <> MANGASTREAM_NAME) AND
         (website <> FAKKU_NAME) then
      begin
        s:= '';
        if cbOptionAutoNumberChapter.Checked then
          s:= Format('%.4d', [i+1]);

        if cbOptionGenerateChapterName.Checked then
        begin
          if cbOptionPathConvert.Checked then
            s1:= Format('%s', [UnicodeRemove(Info.mangaInfo.chapterName.Strings[i])])
          else
            s1:= Format('%s', [Info.mangaInfo.chapterName.Strings[i]]);

          if s = '' then
            s:= s1
          else
            s:= s + ' - ' + s1;
        end;
      end
      else
      if (website = MANGASTREAM_NAME) then
      begin
        if cbOptionPathConvert.Checked then
          s:= Format('%s', [UnicodeRemove(Info.mangaInfo.chapterName.Strings[i])])
        else
          s:= Format('%s', [Info.mangaInfo.chapterName.Strings[i]]);
      end
      else
      begin
        if NOT cbOptionPathConvert.Checked then
          s:= RemoveSymbols(TrimLeft(TrimRight(Info.mangaInfo.title)))
        else
          s:= UnicodeRemove(RemoveSymbols(TrimLeft(TrimRight(Info.mangaInfo.title))));
      end;

      if s='' then
        s:= Format('%.4d', [i+1]);
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
  end;
end;

procedure   TSilentThread.CallMainFormDecreaseThreadCount;
begin
  with MainForm do
  begin
    Dec(silentThreadCount);
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
  Info:= TMangaInformation.Create;

  // some of the code was taken from subthreads's GetMangaInfo
  // since it's multi-thread, we cannot call IE for fetching info from Batoto
  if website = GEHENTAI_NAME then
    times:= 4
  else
    times:= 3;

  if Info.GetInfoFromURL(website, URL, times)<>NO_ERROR then
  begin
    Info.Free;
    exit;
  end;
  Synchronize(CallMainFormAddDownloadTask);

  Info.Free;
  Synchronize(CallMainFormDecreaseThreadCount);
end;

constructor TSilentThread.Create;
begin
  isSuspended    := TRUE;
  isTerminated   := FALSE;
  FreeOnTerminate:= TRUE;

  inherited Create(FALSE);
end;

destructor  TSilentThread.Destroy;
begin
  isTerminated:= TRUE;
  inherited Destroy;
end;

end.

