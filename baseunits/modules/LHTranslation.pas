unit LHTranslation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  readerUrl = 'http://read.lhtranslation.com';

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          if title = '' then title := XPathString('//h1[@class="postsby"]/substring-after(.,":")');
          title := Trim(title);
          coverLink := XPathString('(//div[@class="featured-thumbnail"])[1]/img/@src');
          // FIXME: no genres, summary, etc
          // some chapters are not available for online reading
          for v in XPath('//h2[@class="title"]/a') do begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(Trim(ReplaceString(v.toString, title, '')));
          end;
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  s := '/read-' + ReplaceString(AURL, '/', '') + '.html';
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(readerUrl, s)) then begin
      Result := True;
      XPathStringAll('//img[@class="chapter-img"]/@src', Document, PageLinks);
    end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//select[@id="cat"]/option[@value!="-1"]') do begin
          ALinks.Add(Module.RootURL + '/?cat=' + v.toNode.getAttribute('value'));
          ANames.Add(v.toString);
        end;
      finally
        Free;
      end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'LHTranslation';
    RootURL := 'http://lhtranslation.com';
    TotalDirectory := 1;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.

