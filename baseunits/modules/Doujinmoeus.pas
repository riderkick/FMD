unit Doujinmoeus;

{$mode objfpc}{$H+}

interface

uses
  Classes, WebsiteModules, uData, uBaseUnit, uDownloadsManager;

implementation

uses
  simplehtmltreeparser, xquery;

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
begin
  Page := 100;
  Result := NO_ERROR;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String; Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Parser: TTreeParser;
  s: String;
  v:IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  Source := TStringList.Create;
  try
    s := 'get=' + IncStr(URL);
    with MangaInfo.FHTTP.Document do begin
      Clear;
      Write(PChar(s)^, Length(s));
    end;
    if GetPage(MangaInfo.FHTTP, TObject(Source), Module.RootURL + '/ajax/newest.php',
      3, 'POST') then
    begin
      Parser := TTreeParser.Create;
      try
        ParseHTMLTree(Parser, Source.Text);
        if SelectXPathString('json(*)("success")', Parser) = 'true' then
        begin
          for v in SelectXPathIX('json(*)("newest")()("token")', Parser) do
            Links.Add(v.toString);
          for v in SelectXPathIX('json(*)("newest")()("name")', Parser) do
            Names.Add(v.toString);
        end;
      finally
        Parser.Free;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
var
  info: TMangaInfo;
  Source: TStringList;
  Parser: TTreeParser;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  info := MangaInfo.mangaInfo;
  info.website := Module.Website;
  info.url := FillHost(Module.RootURL, URL);
  Source := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Source), info.url, Reconnect) then
    begin
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Parser := TTreeParser.Create;
        try
          ParseHTMLTree(Parser, Source.Text);
          with info do
          begin
            //title
            title := SelectXPathString('//div[@class="title"]/a[last()]', Parser);
            //cover
            coverLink := SelectXPathString('//div[@id="gallery"]/djm/@thumb', Parser);
            //artist
            artists := SelectXPathString(
              '//*[@id="page_info"]/div[@class="right"]/table/tbody/tr/td/a', Parser);
            //summary
            summary := SelectXPathString(
              '//*[@id="page_info"]/div/div[@class="message"]', Parser);
            //chapter
            if coverLink <> '' then
            begin
              chapterLinks.Add(info.url);
              chapterName.Add(title);
            end;
          end;
        finally
          Parser.Free;
        end;
      end
      else
        Result := INFORMATION_NOT_FOUND;
    end;
  finally
    Source.Free;
  end;
end;

function GetPageNumber(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Container: TTaskContainer;
  Source: TStringList;
  Parser: TTreeParser;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  Container.PageLinks.Clear;
  Container.PageContainerLinks.Clear;
  Container.PageNumber := 0;
  Source := TStringList.Create;
  try
    if DownloadThread.GetPage(TObject(Source), FillHost(Module.RootURL, URL),
      Container.Manager.retryConnect) then
    begin
      if Source.Count > 0 then
      begin
        Result := True;
        Parser := TTreeParser.Create;
        try
          ParseHTMLTree(Parser, Source.Text);
          for v in SelectXPathIX('//div[@id="gallery"]/djm/@file', Parser) do
            Container.PageLinks.Add(v.toString);
        finally
          Parser.Free;
        end;
      end;
    end;
  finally
    Source.Free;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Doujin-Moe';
    RootURL := 'http://www.doujin-moe.us';
    SortedList := True;
    FavoriteAvailable := False;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
