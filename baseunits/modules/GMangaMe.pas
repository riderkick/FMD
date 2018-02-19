unit GMangaMe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/mangas') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//div[@class="row"]//a[./div[@class="manga-cover-container"]]') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(XPathString('div/span[@class="info-item info-title"]', v));
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
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//div/img[starts-with(@class,"img-responsive")]/@src'));
          if title = '' then title := XPathString('//div[@class="content-div"]/h2');
          summary := XPathString('//div[@class="content-div"]/div[@class="summary"]/text()');
          authors := XPathString('//label[starts-with(.,"المؤلفون")]/following-sibling::*');
          artists := XPathString('//label[starts-with(.,"الرسامون")]/following-sibling::*');
          genres := XPathStringAll('//label[starts-with(.,"التصنيفات")]/following-sibling::*//a');
          status := MangaInfoStatusIfPos(XPathString('//label[starts-with(.,"حالة القصة")]/following-sibling::*'),
            'مستمرة',
            'منتهية');
          for v in XPath('//tr/td[./a[@class="chapter-link"]]') do
          begin
            chapterLinks.Add(XPathString('./a/@href', v));
            chapterName.Add(XPathString('../td[1]', v));
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
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      s := XPathString('//script[contains(.,"var release_pages")]/substring-before(substring-after(substring-after(.,"var release_pages"), "(["), "]")', Document);
      if s <> '' then
      begin
        s := StringReplace(s, '"', '', [rfReplaceAll]);
        PageLinks.CommaText := s;
      end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'GManga';
    RootURL := 'http://gmanga.me';
    Category := 'Arabic';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
