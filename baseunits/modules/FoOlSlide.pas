unit FoOlSlide;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager;

implementation

uses
  simplehtmltreeparser, xquery, RegExpr;

const
  dirurl = '/directory/';

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Parser: TTreeParser;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit;
  Source := TStringList.Create;
  try
    if GetPage(TObject(Source), Module.RootURL + dirurl, 3) then
    begin
      Result := INFORMATION_NOT_FOUND;
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Parser := TTreeParser.Create;
        try
          ParseHTMLTree(Parser, Source.Text);
          Page := StrToIntDef(ReplaceRegExpr('^.*/(\d+)/$',
            SelectXPathString('//a[contains(text(),"Last »»")]/@href', Parser),
            '$1', True), 1);
        finally
          Parser.Free;
        end;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String; Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Parser: TTreeParser;
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  Source := TStringList.Create;
  try
    if GetPage(TObject(Source), Module.RootURL + dirurl + IncStr(URL), 3) then
    begin
      Result := INFORMATION_NOT_FOUND;
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Parser := TTreeParser.Create;
        try
          ParseHTMLTree(Parser, Source.Text);
          for v in SelectXPathIX('//*[@class="list series"]/*/*[@class="title"]/a',
              Parser) do
          begin
            Links.Add(v.toNode.getAttribute('href'));
            Names.Add(v.toString);
          end;
        finally
          Parser.Free;
        end;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Parser: TTreeParser;

  procedure ScanInfo;
  var
    v: IXQValue;
  begin
    with MangaInfo.mangaInfo do begin
      //cover
      coverLink := SelectXPathString(
        '//*[@id="content"]//*[@class="thumbnail"]/img/@src', Parser);
      //title
      title := SelectXPathString('//*[@id="content"]//h1[@class="title"]', Parser);
      //author
      authors := TrimLeftChar(SelectXPathString(
        '//div[@class="info"]/*[contains(text(),"Author")]/following-sibling::text()[1]',
        Parser), [':', ' ']);
      //artist
      artists := TrimLeftChar(SelectXPathString(
        '//div[@class="info"]/*[contains(text(),"Artist")]/following-sibling::text()[1]',
        Parser), [':', ' ']);
      //summary
      summary := TrimLeftChar(SelectXPathString(
        '//div[@class="info"]/*[contains(text(),"Synopsis")]/following-sibling::text()[1]',
        Parser), [':', ' ']);
      //chapters
      for v in SelectXPathIX('//*[@class="list"]//*[@class="title"]/a', Parser) do
      begin
        chapterLinks.Add(v.toNode.getAttribute('href'));
        if v.toNode.getAttribute('title') <> '' then
          chapterName.Add(v.toNode.getAttribute('title'))
        else chapterName.Add(v.toString);
      end;
      if chapterLinks.Count > 0 then
        InvertStrings([chapterLinks, chapterName]);
    end;
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  MangaInfo.mangaInfo.website := Module.Website;
  MangaInfo.mangaInfo.url := FillHost(Module.RootURL, URL);
  Source := TStringList.Create;
  try
    if GetPage(TObject(Source), MangaInfo.mangaInfo.url, Reconnect) then
    begin
      Result := INFORMATION_NOT_FOUND;
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Parser := TTreeParser.Create;
        try
          ParseHTMLTree(Parser, Source.Text);
          ScanInfo;
        finally
          Parser.Free;
        end;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function GetPageNumber(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Parser: TTreeParser;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    Source := TStringList.Create;
    try
      if GetPage(TObject(Source), FillHost(Module.RootURL, URL),
        Manager.retryConnect) then
      begin
        if Source.Count > 0 then
        begin
          Result := True;
          Parser := TTreeParser.Create;
          try
            ParseHTMLTree(Parser, Source.Text);
            PageNumber := SelectXPathIX(
              '//*[@class="topbar_right"]//ul[@class="dropdown"]/li', Parser).Count;
          finally
            Parser.Free;
          end;
        end;
      end;
    finally
      Source.Free;
    end;
  end;
end;

function GetImageURL(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Parser: TTreeParser;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container do begin
    Source := TStringList.Create;
    try
      if GetPage(TObject(Source), FillHost(Module.RootURL, URL) +
        'page/' + IncStr(DownloadThread.workCounter),
        Manager.retryConnect) then
      begin
        if Source.Count > 0 then
        begin
          Result := True;
          Parser := TTreeParser.Create;
          try
            ParseHTMLTree(Parser, Source.Text);
            PageLinks[DownloadThread.workCounter] :=
              SelectXPathString('//*[@id="page"]//img/@src', Parser);
          finally
            Parser.Free;
          end;
        end;
      end;
    finally
      Source.Free;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Shoujosense';
    RootURL := 'http://reader.shoujosense.com';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
