{
        File: uGetMangaInfosThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader

        -----------------

        This class allows us to get infomation on certain site and shows it
        in FMD's Manga infos tab.
}

unit uGetMangaInfosThread;

{$mode delphi}

interface

uses
  SysUtils, Graphics, Dialogs, uBaseUnit, uData, FMDOptions,
  BaseThread;

type

  { TGetMangaInfosThread }

  TGetMangaInfosThread = class(TBaseThread)
  protected
    FMangaListPos: Integer;
    FCover: TPicture;
    FTitle, FWebsite, FLink: String;
    FInfo: TMangaInformation;
    FNumChapter: Cardinal;
    // Return TRUE if we can load manga cover.
    FIsHasMangaCover: Boolean;
    // Flush this thread, means that the result will not be shown.
    procedure Execute; override;
    procedure MainThreadSyncInfos;
    procedure MainThreadShowInfos;
    procedure MainThreadShowCover;
    procedure MainThreadShowCannotGetInfo;
  public
    constructor Create;
    destructor Destroy; override;
    property Title: String read FTitle write FTitle;
    property Website: String read FWebsite write FWebsite;
    property Link: String read FLink write FLink;
    property MangaListPos: Integer read FMangaListPos write FMangaListPos;
  end;

implementation

uses
  frmMain, WebsiteModules, FMDVars;

procedure TGetMangaInfosThread.MainThreadSyncInfos;
begin
  FInfo.SyncInfoToData(DataProcess);
  dataProcess.Commit;
end;

procedure TGetMangaInfosThread.Execute;

  function GetMangaInfo: Boolean;
  var
    filterPos: Cardinal;
    infob: byte;
  begin
    Result := False;
    try
      FInfo.mangaInfo.website := Website;
      FInfo.mangaInfo.link := Link;
      FInfo.mangaInfo.title := Title;
      FInfo.ModuleId := Modules.LocateModule(Website);
      if (FMangaListPos >= 0) and (MainForm.cbSelectManga.ItemIndex<>-1) and
        (website = MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex]) then
      begin
        filterPos := FMangaListPos;
        if FInfo.mangaInfo.title = '' then
          FInfo.mangaInfo.title := dataProcess.Value[filterPos, DATA_PARAM_TITLE];
        FInfo.mangaInfo.link := dataProcess.Value[filterPos, DATA_PARAM_LINK];
        FInfo.mangaInfo.authors := dataProcess.Value[filterPos, DATA_PARAM_AUTHORS];
        FInfo.mangaInfo.artists := dataProcess.Value[filterPos, DATA_PARAM_ARTISTS];
        FInfo.mangaInfo.status := dataProcess.Value[filterPos, DATA_PARAM_STATUS];
        FInfo.mangaInfo.summary := dataProcess.Value[filterPos, DATA_PARAM_SUMMARY];
        FInfo.mangaInfo.numChapter := StrToIntDef(dataProcess.Value[filterPos, DATA_PARAM_NUMCHAPTER], 0);
        FInfo.mangaInfo.genres := dataProcess.Value[filterPos, DATA_PARAM_GENRES];
        FNumChapter := StrToIntDef(dataProcess.Value[filterPos, DATA_PARAM_NUMCHAPTER], 0);
      end;
      FInfo.isGenerateFolderChapterName := OptionGenerateMangaFolder;
      FInfo.isRemoveUnicode := OptionChangeUnicodeCharacter;

      infob := INFORMATION_NOT_FOUND;

      //wait if there is concurrent connection limit
      if Modules.MaxConnectionLimit[FInfo.ModuleId] > 0 then
      begin
        while not Modules.CanCreateConnection(FInfo.ModuleId) do
          Sleep(SOCKHEARTBEATRATE);
        Modules.IncActiveConnectionCount(FInfo.ModuleId);
      end;

      infob := FInfo.GetInfoFromURL(Website, Link, 0);

      if Terminated or isExiting then Exit;
      if infob <> NO_ERROR then Exit;

      //set back if title changed
      if (FInfo.mangaInfo.title <> '') and (FInfo.mangaInfo.title <> FTitle) then
        FTitle := FInfo.mangaInfo.title;

      if FMangaListPos >= 0 then
      begin
        if dataProcess.WebsiteLoaded(Website) then
        begin
          if SitesWithoutInformation(website) then
          begin
            if FInfo.mangaInfo.authors = '' then
              FInfo.mangaInfo.authors := DataProcess.Value[filterPos, DATA_PARAM_AUTHORS];
            if FInfo.mangaInfo.artists = '' then
              FInfo.mangaInfo.artists := DataProcess.Value[filterPos, DATA_PARAM_ARTISTS];
            if FInfo.mangaInfo.genres = '' then
              FInfo.mangaInfo.genres := DataProcess.Value[filterPos, DATA_PARAM_GENRES];
            if FInfo.mangaInfo.summary = '' then
              FInfo.mangaInfo.summary := DataProcess.Value[filterPos, DATA_PARAM_SUMMARY];
          end;

          if not (Terminated or isExiting) then
            Synchronize(MainThreadSyncInfos);
        end;
      end;
      Result := True;
    except
      on E: Exception do
        MainForm.ExceptionHandler(Self, E);
    end;
  end;

begin
  try
    if not GetMangaInfo then
    begin
      if not (Terminated or isExiting) then
        Synchronize(MainThreadShowCannotGetInfo);
    end
    else
    begin
      if Terminated or isExiting then Exit;
      Synchronize(MainThreadShowInfos);
      FCover.Clear;
      // If there's cover then we will load it to the TPicture component.
      if OptionEnableLoadCover and (Trim(FInfo.mangaInfo.coverLink) <> '') then
        try
          FInfo.FHTTP.Document.Clear;
          if FInfo.FHTTP.GET(FInfo.mangaInfo.coverLink) then
          begin
            FCover.LoadFromStream(FInfo.FHTTP.Document);
            FIsHasMangaCover := True;
          end;
        except
        end;
      if not (Terminated or isExiting) then
        Synchronize(MainThreadShowCover);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TGetMangaInfosThread.MainThreadShowCannotGetInfo;
begin
  MessageDlg('', RS_DlgCannotGetMangaInfo,
    mtInformation, [mbYes], 0);
  MainForm.rmInformation.Clear;
  MainForm.tmAnimateMangaInfo.Enabled := False;
  MainForm.pbWait.Visible := False;
  MainForm.imCover.Picture.Assign(nil);
end;

procedure TGetMangaInfosThread.MainThreadShowInfos;
begin
  TransferMangaInfo(mangaInfo, FInfo.mangaInfo);
  with MainForm do begin
    if (FMangaListPos > -1) and dataProcess.WebsiteLoaded(Website) then
      begin
        vtMangaList.BeginUpdate;
        dataProcess.Refresh;
        vtMangaList.EndUpdate;
      end;
    ShowInformation(mangaInfo.title, mangaInfo.website, mangaInfo.link);
  end;
end;

procedure TGetMangaInfosThread.MainThreadShowCover;
begin
  MainForm.tmAnimateMangaInfo.Enabled := False;
  MainForm.pbWait.Visible := False;
  if FIsHasMangaCover then
  begin
    try
      MainForm.imCover.Picture.Assign(FCover);
    except
      on E: Exception do ;
    end;
    FCover.Clear;
  end;
end;

constructor TGetMangaInfosThread.Create;
begin
  inherited Create(True);
  FInfo := TMangaInformation.Create(Self);
  FCover := MainForm.mangaCover;
  FIsHasMangaCover := False;
  FMangaListPos := -1;
end;

destructor TGetMangaInfosThread.Destroy;
begin
  Modules.DecActiveConnectionCount(FInfo.ModuleId);
  GetInfosThread := nil;
  FCover := nil;
  FInfo.Free;
  inherited Destroy;
end;

end.
