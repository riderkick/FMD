unit WPAdultSiteSkins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, ImageHoster;

implementation

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(XPathString('//div[@class="paginator"]/span[starts-with(.,"Page") and contains(.," of ")]/normalize-space(substring-after(.," of "))'),1);
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL;
  if AURL <> '0' then
    s += '/page/' + IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        XPathHREFtitleAll('//div[@class="posts"]/div[starts-with(@id,"post-")]/a', ALinks, ANames);
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//div[@class="single-post"]//img[starts-with(@class,"attachment-") and not(@data-lazy-src)]/@src');
          if coverLink = '' then coverLink := XPathString('//div[@class="single-post"]/p//img[not(@data-lazy-src)]/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//div[@class="posts"]/h2[@class="post-title"][1]');
          chapterLinks.Add(url);
          chapterName.Add(title);
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
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
      try
        XPathStringAll('//div[@class="single-post"]//dl[@class="gallery-item"]/dt/a/@href', PageContainerLinks);
        XPathStringAll('//div[@class="single-post"]/p//a[./img[not(data-lazy-src)]]/@href', PageContainerLinks);
        if PageContainerLinks.Count = 0 then
          for v in XPath('//div[@class="single-post"]/p//img[not(@data-lazy-src)]/@src') do
            PageLinks.Add(MaybeFillHost(Module.RootURL, v.toString))
        else
          PageNumber := PageContainerLinks.Count;
      finally
        Free;
      end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    if DownloadThread.WorkId >= PageContainerLinks.Count then Exit;
    s := ImageHoster.GetImageHoster(DownloadThread.FHTTP, MaybeFillHost(Module.RootURL, PageContainerLinks[DownloadThread.WorkId]));
    if s = '' then
    if GET(MaybeFillHost(Module.RootURL, PageContainerLinks[DownloadThread.WorkId])) then
    begin
      with TXQueryEngineHTML.Create(Document) do
        try
          s := MaybeFillHost(Module.RootURL, XPathString('//div[@class="attachment-image"]//img/@src'));
        finally
          Free;
        end;
    end;
    if s <> '' then
    begin
      Result := True;
      PageLinks[DownloadThread.WorkId] := s;
    end;
  end;

end;

procedure RegisterModule;
  function AddWebsiteModule(const AWebsite, ARootURL: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      Category := 'Adult';
      SortedList := True;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnGetImageURL := @GetImageURL;
    end;
  end;
begin
  AddWebsiteModule('PornComix', 'http://www.porncomix.info');
  AddWebsiteModule('Comic-XXX', 'http://comics-xxx.com');
  AddWebsiteModule('FreeAdultComix', 'http://freeadultcomix.com');
  AddWebsiteModule('AsianHotties', 'http://tits.asianhotties.me');
end;

initialization
  RegisterModule;

end.

