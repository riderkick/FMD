unit Doujinmoeus;

{$mode objfpc}{$H+}

interface

uses
  Classes, WebsiteModules, uData, uBaseUnit, uDownloadsManager;

implementation

uses
  simplehtmltreeparser, xquery;

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
            artists := SelectXPathString('//*[@id="page_info"]/div[@class="right"]/table/tbody/tr/td/a', Parser);
            //summary
            summary := SelectXPathString('//*[@id="page_info"]/div/div[@class="message"]', Parser);
            //chapter
            if title <> '' then
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
    InformationAvailable := True;
    FavoriteAvailable := False;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
