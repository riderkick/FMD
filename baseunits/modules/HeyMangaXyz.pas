unit HeyMangaXyz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/manga-series/new/';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '1') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        page := XPath('//div/a[@class="btn btn-sm btn-icon"]').Count;
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + IncStr(AURL)) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//div[@class="row"]/div/div/a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(v.toString);
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do
  begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//div[@class="mangas"]/div[@class="manga"]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := SeparateRight(XPathString('//ul[@class="lead"]/li[starts-with(.,"Name: ")]'), ': ');
          s := XPathString('//ul[@class="lead"]/li[starts-with(.,"Status: ")]');
          if Pos('Ongoing', s) > 0 then
            status := '1'
          else if Pos('Completed', s) > 0 then
            status := '0';
          genres := XPathStringAll('//ul[@class="lead"]/li[starts-with(.,"Genre: ")]/a');
          summary := SeparateRight(XPathString('//ul[@class="lead"]/li[starts-with(.,"Plot: ")]'), ': ');
          for v in XPath('//div[@id="chapters"]/ul//li//a') do
          begin
            s := v.toNode.getAttribute('href');
            if RightStr(s, 2) = '/1' then
              SetLength(s, Length(s) - 1);
            chapterLinks.Add(s);
            chapterName.Add(v.toString);
          end;
          if chapterLinks.Count > 0 then
          begin
            s := XPathString('//div[@id="chapters"]/ul/p[starts-with(.,"Next Chapter:")]/following-sibling::li//a/@href');
            if s <> '' then
            begin
              if RightStr(s, 2) = '/1' then
                SetLength(s, Length(s) - 1);
              if SameText(s, chapterLinks[chapterLinks.Count-1]) then
              begin
                chapterLinks.Delete(chapterLinks.Count - 1);
                chapterName.Delete(chapterName.Count - 1);
              end;
            end;
          end;
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.manager.container do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL) + '1') then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageNumber := XPath('//select[@id="page_list"]/option').Count - 1;
          for v in XPath('//picture/img') do
            PageLinks.Add(v.toNode.getAttribute('src'));
        finally
          Free;
        end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container, DownloadThread.FHTTP do
  begin
    if GET(FillHost(Module.RootURL, AURL) + IncStr(DownloadThread.workCounter)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[DownloadThread.workCounter] := XPathString('//picture/img[@id="img-content"]/@src');
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'HeyManga';
    RootURL := 'http://www.heymanga.xyz';
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
