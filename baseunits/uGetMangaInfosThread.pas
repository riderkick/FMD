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
  SysUtils, Graphics, Dialogs, blcksock,
  uBaseUnit, uData, uFMDThread;

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

    procedure SockOnHeartBeat(Sender: TObject);
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
  frmMain;

// ----- Protected methods -----

procedure TGetMangaInfosThread.DoGetInfos;

  function GetMangaInfo(const URL, website: String): Boolean;
  var
    filterPos: Cardinal;
    infob: byte;
  begin
    Result := False;
    try
      if (FMangaListPos >= 0) and
        (website = MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex]) then
      begin
        filterPos := MainForm.dataProcess.GetPos(FMangaListPos);
        FInfo.mangaInfo.title := MainForm.dataProcess.Param[filterPos, DATA_PARAM_NAME];
        FInfo.mangaInfo.link := MainForm.dataProcess.Param[filterPos, DATA_PARAM_LINK];
        FInfo.mangaInfo.authors := MainForm.dataProcess.Param[filterPos, DATA_PARAM_AUTHORS];
        FInfo.mangaInfo.artists := MainForm.dataProcess.Param[filterPos, DATA_PARAM_ARTISTS];
        FInfo.mangaInfo.status := MainForm.dataProcess.Param[filterPos, DATA_PARAM_STATUS];
        FInfo.mangaInfo.summary := MainForm.dataProcess.Param[filterPos, DATA_PARAM_SUMMARY];
        FInfo.mangaInfo.numChapter := StrToIntDef(MainForm.dataProcess.Param[filterPos, DATA_PARAM_NUMCHAPTER], 0);
        FNumChapter := StrToIntDef(MainForm.dataProcess.Param[filterPos, DATA_PARAM_NUMCHAPTER], 0);
        if SitesWithoutInformation(website) or
          (website = WebsiteRoots[EHENTAI_ID, 0]) then
          FInfo.mangaInfo.genres := MainForm.dataProcess.Param[filterPos, DATA_PARAM_GENRES];
      end;

      FInfo.isGenerateFolderChapterName := MainForm.cbOptionGenerateChapterName.Checked;
      FInfo.isRemoveUnicode := MainForm.cbOptionPathConvert.Checked;

      infob := INFORMATION_NOT_FOUND;
      try
        infob := FInfo.GetInfoFromURL(website, URL, 2);
      except
        on E: Exception do
          MainForm.ExceptionHandler(Self, E);
      end;

      if Self.IsTerminated then
        Exit;

      if infob <> NO_ERROR then
        Exit;

      // fixed
      if FMangaListPos >= 0 then
      begin
        if website = MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex] then
        begin
          if SitesWithoutInformation(website) then
          begin
            if FInfo.mangaInfo.authors = '' then
              FInfo.mangaInfo.authors :=
                MainForm.DataProcess.Param[filterPos, DATA_PARAM_AUTHORS];
            if FInfo.mangaInfo.artists = '' then
              FInfo.mangaInfo.artists :=
                MainForm.DataProcess.Param[filterPos, DATA_PARAM_ARTISTS];
            if FInfo.mangaInfo.genres = '' then
              FInfo.mangaInfo.genres :=
                MainForm.DataProcess.Param[filterPos, DATA_PARAM_GENRES];
            if FInfo.mangaInfo.summary = '' then
              FInfo.mangaInfo.summary :=
                MainForm.DataProcess.Param[filterPos, DATA_PARAM_SUMMARY];
          end;
          FInfo.SyncInfoToData(MainForm.DataProcess, filterPos);
        end;
      end;
      Result := True;
    except
      on E: Exception do
        MainForm.ExceptionHandler(Self, E);
    end;
  end;

begin
  if MainForm.cbSelectManga.ItemIndex < 0 then
    Exit;
  try
    if not GetMangaInfo(link, website) then
    begin
      if not Self.IsTerminated then
        Synchronize(MainThreadShowCannotGetInfo);
    end
    else
    begin
      Synchronize(MainThreadShowInfos);
      FCover.Clear;
      // If there's cover then we will load it to the TPicture component.
      if (OptionEnableLoadCover) and
        ((Pos('http://', FInfo.mangaInfo.coverLink) > 0) or
        (Pos('https://', FInfo.mangaInfo.coverLink) > 0)) then
        FIsHasMangaCover := GetPage(nil, FInfo.FHTTP, TObject(FCover), FInfo.mangaInfo.coverLink, 3, True)
      else
        FIsHasMangaCover := False;
      Synchronize(MainThreadShowCover);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TGetMangaInfosThread.SockOnHeartBeat(Sender: TObject);
begin
  if Terminated then
  begin
    TBlockSocket(Sender).Tag := 1;
    TBlockSocket(Sender).StopFlag := True;
    TBlockSocket(Sender).AbortSocket;
  end;
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
    MessageDlg('', stDlgCannotGetMangaInfo,
      mtInformation, [mbYes], 0);
    MainForm.rmInformation.Clear;
    MainForm.itAnimate.Enabled := False;
    MainForm.pbWait.Visible := False;
    MainForm.imCover.Picture.Assign(nil);
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TGetMangaInfosThread.MainThreadShowInfos;
begin
  if IsFlushed then
    Exit;
  try
    TransferMangaInfo(MainForm.mangaInfo, FInfo.mangaInfo);
    //if title or numChapter changed refresh the list
    //if (website = MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex]) and
    if (Website = MainForm.cbSelectManga.Text) and
      (FMangaListPos > -1) then
    begin
      if (FInfo.mangaInfo.title <> FTitle) or
        (FInfo.mangaInfo.numChapter <> FNumChapter) then
      begin
        FTitle := FInfo.mangaInfo.title;
        with MainForm.vtMangaList do
        begin
          ReinitNode(RootNode, True);
          Repaint;
        end;
      end;
    end;
    MainForm.ShowInformation(FTitle, FWebsite, FLink);
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
    MainForm.itAnimate.Enabled := False;
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

// ----- Public methods -----

constructor TGetMangaInfosThread.Create;
begin
  inherited Create(True);
  FIsFlushed := False;
  FInfo := TMangaInformation.Create;
  FInfo.FOwner := Self;
  FInfo.FHTTP.Sock.OnHeartbeat := SockOnHeartBeat;
  FInfo.FHTTP.Sock.HeartbeatRate := SOCKHEARTBEATRATE;
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
