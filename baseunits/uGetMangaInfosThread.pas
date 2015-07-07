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
  SysUtils, Graphics, Dialogs, uBaseUnit, uData, uFMDThread;

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
      if (FMangaListPos >= 0) and
        (website = MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex]) then
      begin
        filterPos := FMangaListPos;
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
      infob := FInfo.GetInfoFromURL(Website, Link, 2);

      if Self.Terminated then Exit;
      if infob <> NO_ERROR then Exit;

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

          FInfo.SyncInfoToData(MainForm.DataProcess);
          MainForm.dataProcess.Commit;
          //MainForm.dataProcess.Refresh;
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
    INIAdvanced.Reload;
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
        FIsHasMangaCover := GetPage(FInfo.FHTTP, TObject(FCover), FInfo.mangaInfo.coverLink, 3)
      else
        FIsHasMangaCover := False;
      Synchronize(MainThreadShowCover);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
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
    MessageDlg('', RS_DlgCannotGetMangaInfo,
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
  if IsFlushed then Exit;
  try
    TransferMangaInfo(MainForm.mangaInfo, FInfo.mangaInfo);
    with MainForm do begin
      if (FMangaListPos > -1) and
        (Website = MainForm.cbSelectManga.Text) then
        begin
          vtMangaList.BeginUpdate;
          dataProcess.Refresh;
          vtMangaList.EndUpdate;
          vtMangaList.Repaint;
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
