{
        File: uData.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uData;

{$mode objfpc}{$H+}

// This unit contains all necessary functions for data processing

interface

uses
  Classes, SysUtils, uBaseUnit, DBDataProcess, FMDOptions, httpsendthread,
  BaseThread, LazFileUtils, strutils, RegExpr, httpsend, MultiLog;

type

  { TMangaInformation }

  TMangaInformation = class(TObject)
  private
    FOwner: TBaseThread;
    FModuleId: Integer;
    procedure SetModuleId(AValue: Integer);
  public
    isGetByUpdater: Boolean;
    mangaInfo: TMangaInfo;
    parse: TStringList;
    isGenerateFolderChapterName: Boolean;
    isRemoveUnicode: Boolean;
    RemoveHostFromChapterLinks: Boolean;
    FHTTP: THTTPSendThread;

    constructor Create(AOwnerThread: TBaseThread = nil; ACreateInfo: Boolean = True);
    destructor Destroy; override;
    procedure ClearInfo;
    function GetDirectoryPage(var APage: Integer; const AWebsite: String): Byte;
    function GetNameAndLink(const ANames, ALinks: TStringList; const AWebsite, AURL: String): Byte;
    function GetInfoFromURL(const AWebsite, AURL: String): Byte;
    procedure SyncInfoToData(const ADataProcess: TDBDataProcess); overload;
    procedure AddInfoToData(const ATitle, ALink: String; const ADataProcess: TDBDataProcess); overload;
    //wrapper
    function GetPage(var AOutput: TObject; AURL: String; const AReconnect: Integer = 0): Boolean; inline;
    property Thread: TBaseThread read FOwner;
    property ModuleId: Integer read FModuleId write SetModuleId;
  end;

var
  options: TStringList;

implementation

uses
  Dialogs, frmMain, WebsiteModules, uUpdateThread;

{ TMangaInformation }

constructor TMangaInformation.Create(AOwnerThread: TBaseThread; ACreateInfo: Boolean);
begin
  inherited Create;
  FOwner := AOwnerThread;
  FHTTP := THTTPSendThread.Create(AOwnerThread);
  FHTTP.Headers.NameValueSeparator := ':';
  parse := TStringList.Create;
  if ACreateInfo then
    mangaInfo := TMangaInfo.Create;
  isGetByUpdater := False;
  ModuleId := -1;
  RemoveHostFromChapterLinks := True;
end;

destructor TMangaInformation.Destroy;
begin
  if Assigned(mangaInfo) then
    mangaInfo.Free;
  if Assigned(parse) then
    parse.Free;
  FHTTP.Free;
  inherited Destroy;
end;

procedure TMangaInformation.ClearInfo;
begin
  mangaInfo.artists := '';
  mangaInfo.authors := '';
  mangaInfo.genres := '';
  mangaInfo.summary := '';
  mangaInfo.coverLink := '';
  mangaInfo.numChapter := 0;
  mangaInfo.status := '';
  mangaInfo.title := '';
  mangaInfo.url := '';
  mangaInfo.website := '';
  mangaInfo.chapterName.Clear;
  mangaInfo.chapterLinks.Clear;
end;

procedure TMangaInformation.SetModuleId(AValue: Integer);
begin
  if FModuleId = AValue then Exit;
  FModuleId := AValue;
  if (FModuleId <> -1) and Assigned(FHTTP) then
    WebsiteModules.Modules[FModuleId].PrepareHTTP(FHTTP);
end;

function TMangaInformation.GetDirectoryPage(var APage: Integer; const AWebsite: String): Byte;
begin
  APage := 1;

  //load pagenumber_config if available
  if  Modules[ModuleId].Settings.Enabled and (Modules[ModuleId].Settings.UpdateListDirectoryPageNumber > 0) then
  begin
    APage := Modules[ModuleId].Settings.UpdateListDirectoryPageNumber;
    BROWSER_INVERT := True;
    Exit(NO_ERROR);
  end;

  BROWSER_INVERT := False;
  if ModuleId < 0 then
    ModuleId := Modules.LocateModule(AWebsite);
  if Modules.ModuleAvailable(ModuleId, MMGetDirectoryPageNumber) then
    Result := Modules.GetDirectoryPageNumber(Self, APage, TUpdateListThread(Thread).workPtr, ModuleId)
  else
    Exit(INFORMATION_NOT_FOUND);

  if APage < 1 then
    APage := 1;
end;

function TMangaInformation.GetNameAndLink(const ANames, ALinks: TStringList;
  const AWebsite, AURL: String): Byte;
begin
  if ModuleId < 0 then
    ModuleId := Modules.LocateModule(AWebsite);
  if Modules.ModuleAvailable(ModuleId, MMGetNameAndLink) then
  begin
    Result := Modules.GetNameAndLink(Self, ANames, ALinks, AURL, ModuleId)
  end
  else
    Exit(INFORMATION_NOT_FOUND);

  //remove host from AURL
  if ALinks.Count > 0 then
    RemoveHostFromURLsPair(ALinks, ANames);
end;

function TMangaInformation.GetInfoFromURL(const AWebsite, AURL: String): Byte;
var
  s, s2: String;
  j, k: Integer;
  del: Boolean;
  bmangaInfo: TBaseMangaInfo;
begin
  if Trim(AURL) = '' then
    Exit(INFORMATION_NOT_FOUND);

  GetBaseMangaInfo(mangaInfo, bmangaInfo);

  mangaInfo.website := AWebsite;
  mangaInfo.coverLink := '';
  mangaInfo.numChapter := 0;
  mangaInfo.chapterName.Clear;
  mangaInfo.chapterLinks.Clear;

  if ModuleId < 0 then
    ModuleId := Modules.LocateModule(AWebsite);
  if Modules.ModuleAvailable(ModuleId, MMGetInfo) then begin
    mangaInfo.url := FillHost(Modules.Module[ModuleId].RootURL, AURL);
    Result := Modules.GetInfo(Self, AURL, ModuleId);
  end
  else
    Exit(INFORMATION_NOT_FOUND);

  with mangaInfo do begin
    if link = '' then
      link := RemoveHostFromURL(mangaInfo.url);

    // cleanup info
    coverLink := CleanURL(coverLink);
    title := Trim(FixWhiteSpace(RemoveStringBreaks(CommonStringFilter(title))));
    authors := Trim(FixWhiteSpace(RemoveStringBreaks(Trim(authors))));
    artists := Trim(FixWhiteSpace(RemoveStringBreaks(Trim(artists))));
    genres := Trim(FixWhiteSpace(RemoveStringBreaks(Trim(genres))));

    authors := TrimRightChar(Trim(FixWhiteSpace(authors)), [',']);
    artists := TrimRightChar(Trim(FixWhiteSpace(artists)), [',']);
    genres := TrimRightChar(Trim(FixWhiteSpace(genres)), [',']);

    summary := CleanMultilinedString(FixWhiteSpace(summary));

    // fix info
    if (LeftStr(authors, 1) = '<') or (authors = '-') or (authors = ':') then
      authors := '';
    if (LeftStr(artists, 1) = '<') or (artists = '-') or (artists = ':') then
      artists := '';
    if (summary = '-') or (summary = ':') then
      summary := '';
    if title = '' then
      title := 'N/A';
    FillBaseMangaInfo(mangaInfo, bmangaInfo);

    // cleanup chapters
    if chapterLinks.Count > 0 then begin
      while chapterName.Count < chapterLinks.Count do
        chapterName.Add('');
      while chapterLinks.Count < chapterName.Count do
        chapterName.Delete(chapterName.Count - 1);
      for j := 0 to chapterLinks.Count - 1 do begin
        chapterLinks[j] := Trim(chapterLinks[j]);
        chapterName[j] := Trim(chapterName[j]);
      end;
    end;

    // remove duplicate chapter
    if chapterLinks.Count > 0 then
    begin
      j := 0;
      while j < (chapterLinks.Count - 1) do
      begin
        del := False;
        if (j + 1) < chapterLinks.Count then
          for k := j + 1 to chapterLinks.Count - 1 do
            if SameText(chapterLinks[j], chapterLinks[k]) then
            begin
              chapterLinks.Delete(j);
              chapterName.Delete(j);
              del := True;
              Break;
            end;
        if not del then
          Inc(j);
      end;
    end;

    if chapterLinks.Count > 0 then
    begin
      // remove host from chapter links
      if RemoveHostFromChapterLinks then
        RemoveHostFromURLsPair(chapterLinks, chapterName);
      // fixing chapter name
      for j := 0 to chapterName.Count - 1 do
        chapterName[j] := Trim(CleanString(RemoveStringBreaks(
          CommonStringFilter(chapterName[j]))));

      //remove manga name from chapter
      if OptionRemoveMangaNameFromChapter and (title <> '') then
      begin
        s := LowerCase(title);
        j := Length(s);
        for k := 0 to chapterName.Count - 1 do begin
          s2 := LowerCase(chapterName[k]);
          if Length(s2) > j then
            if Pos(s, s2) = 1 then begin
              s2 := chapterName[k];
              Delete(s2, 1, j);
              s2 := Trim(s2);
              if LeftStr(s2, 2) = '- ' then
                Delete(s2, 1, 2);
              chapterName[k] := s2;
            end;
        end;
      end;
    end;

    numChapter := chapterLinks.Count;
  end;
end;

procedure TMangaInformation.SyncInfoToData(const ADataProcess: TDBDataProcess);
begin
  if Assigned(ADataProcess) then
    with mangaInfo do
      ADataProcess.UpdateData(title, link, authors, artists, genres, status, summary,
        numChapter, website);
end;

procedure TMangaInformation.AddInfoToData(const ATitle, ALink: String; const ADataProcess: TDBDataProcess);
begin
  if Assigned(ADataProcess) then
  begin
    if (mangaInfo.title = '') and (ATitle <> '') then mangaInfo.title := ATitle;
    if (mangaInfo.link = '') and (ALink <> '') then mangaInfo.link := ALink;
    with mangaInfo do
      ADataProcess.AddData(title, link, authors, artists, genres, status,
        StringBreaks(summary), numChapter, Now);
  end;
end;

function TMangaInformation.GetPage(var AOutput: TObject; AURL: String; const AReconnect: Integer): Boolean;
begin
  Result := uBaseUnit.GetPage(FHTTP, AOutput, AURL, AReconnect);
end;

end.
