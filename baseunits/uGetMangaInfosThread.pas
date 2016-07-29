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
  SysUtils, Graphics, Dialogs, uBaseUnit, uData, uFMDThread, FMDOptions;

type

  { TGetMangaInfosThread }

  TGetMangaInfosThread = class(TFMDThread)
  protected
    FMangaListPos: Integer;
    FCover: TPicture;
    FTitle, FWebsite, FLink: String;
    FInfo: TMangaInformation;
    FNumChapter: Cardinal;
    // Return TRUE if we can load manga cover.
    FIsHasMangaCover,
    // Flush this thread, means that the result will not be shown.
    FIsFlushed: Boolean;

    procedure DoTerminate; override;
    procedure Execute; override;
    procedure DoGetInfos;

    procedure MainThreadShowInfos;
    procedure MainThreadShowCover;
    procedure MainThreadShowCannotGetInfo;
  public
    constructor Create;
    destructor Destroy; override;

    property Title: String read FTitle write FTitle;
    property Website: String read FWebsite write FWebsite;
    property Link: String read FLink write FLink;
    property IsFlushed: Boolean read FIsFlushed write FIsFlushed;
    property MangaListPos: Integer read FMangaListPos write FMangaListPos;
  end;

implementation

uses
  frmMain, WebsiteModules;

procedure TGetMangaInfosThread.DoGetInfos;

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
          FInfo.mangaInfo.title := MainForm.dataProcess.Value[filterPos, DATA_PARAM_TITLE];
        if FInfo.mangaInfo.link = '' then
          FInfo.mangaInfo.link := MainForm.dataProcess.Value[filterPos, DATA_PARAM_LINK];
        FInfo.mangaInfo.authors := MainForm.dataProcess.Value[filterPos, DATA_PARAM_AUTHORS];
        FInfo.mangaInfo.artists := MainForm.dataProcess.Value[filterPos, DATA_PARAM_ARTISTS];
        FInfo.mangaInfo.status := MainForm.dataProcess.Value[filterPos, DATA_PARAM_STATUS];
        FInfo.mangaInfo.summary := MainForm.dataProcess.Value[filterPos, DATA_PARAM_SUMMARY];
        FInfo.mangaInfo.numChapter := StrToIntDef(MainForm.dataProcess.Value[filterPos, DATA_PARAM_NUMCHAPTER], 0);
        FInfo.mangaInfo.genres := MainForm.dataProcess.Value[filterPos, DATA_PARAM_GENRES];
        FNumChapter := StrToIntDef(MainForm.dataProcess.Value[filterPos, DATA_PARAM_NUMCHAPTER], 0);
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

      if Self.Terminated then Exit;
      if infob <> NO_ERROR then Exit;

      //set back if title changed
      if (FInfo.mangaInfo.title <> '') and (FInfo.mangaInfo.title <> FTitle) then
        FTitle := FInfo.mangaInfo.title;

      if FMangaListPos >= 0 then
      begin
        if MainForm.dataProcess.WebsiteLoaded(Website) then
        begin
          if SitesWithoutInformation(website) then
          begin
            if FInfo.mangaInfo.authors = '' then
              FInfo.mangaInfo.authors :=
                MainForm.DataProcess.Value[filterPos, DATA_PARAM_AUTHORS];
            if FInfo.mangaInfo.artists = '' then
              FInfo.mangaInfo.artists :=
                MainForm.DataProcess.Value[filterPos, DATA_PARAM_ARTISTS];
            if FInfo.mangaInfo.genres = '' then
              FInfo.mangaInfo.genres :=
                MainForm.DataProcess.Value[filterPos, DATA_PARAM_GENRES];
            if FInfo.mangaInfo.summary = '' then
              FInfo.mangaInfo.summary :=
                MainForm.DataProcess.Value[filterPos, DATA_PARAM_SUMMARY];
          end;

          FInfo.SyncInfoToData(MainForm.DataProcess);
          MainForm.dataProcess.Commit;
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
      if not Self.Terminated then
        Synchronize(MainThreadShowCannotGetInfo);
    end
    else
    begin
      Synchronize(MainThreadShowInfos);
      FCover.Clear;
      // If there's cover then we will load it to the TPicture component.
      if OptionEnableLoadCover and (Trim(FInfo.mangaInfo.coverLink) <> '') then
      begin
        FInfo.FHTTP.Document.Clear;
        FIsHasMangaCover := FInfo.FHTTP.GET(FInfo.mangaInfo.coverLink, FCover);
      end
      else
        FIsHasMangaCover := False;
      Synchronize(MainThreadShowCover);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TGetMangaInfosThread.DoTerminate;
begin
  Modules.DecActiveConnectionCount(FInfo.ModuleId);
  inherited DoTerminate;
end;

procedure TGetMangaInfosThread.Execute;
begin
  MainForm.isGetMangaInfos := True;
  try
    DoGetInfos;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TGetMangaInfosThread.MainThreadShowCannotGetInfo;
begin
  if IsFlushed then
    Exit;
  try
    MessageDlg('', RS_DlgCannotGetMangaInfo,
      mtInformation, [mbYes], 0);
    MainForm.rmInformation.Clear;
    MainForm.tmAnimateMangaInfo.Enabled := False;
    MainForm.pbWait.Visible := False;
    MainForm.imCover.Picture.Assign(nil);
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TGetMangaInfosThread.MainThreadShowInfos;
begin
  if IsFlushed then Exit;
  try
    TransferMangaInfo(MainForm.mangaInfo, FInfo.mangaInfo);
    with MainForm do begin
      if (FMangaListPos > -1) and dataProcess.WebsiteLoaded(Website) then
        begin
          vtMangaList.BeginUpdate;
          dataProcess.Refresh;
          vtMangaList.EndUpdate;
        end;
      ShowInformation(FTitle, FWebsite, FLink);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TGetMangaInfosThread.MainThreadShowCover;
begin
  if IsFlushed then
    Exit;
  try
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
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

constructor TGetMangaInfosThread.Create;
begin
  inherited Create(True);
  FIsFlushed := False;
  FInfo := TMangaInformation.Create(Self);
  FCover := MainForm.mangaCover;
  FMangaListPos := -1;
end;

destructor TGetMangaInfosThread.Destroy;
begin
  FInfo.Free;
  FCover := nil;
  if not IsFlushed then
    MainForm.isGetMangaInfos := False;
  inherited Destroy;
end;

end.
