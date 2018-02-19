unit MangaFox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, MangaFoxWatermark;

implementation

var
  removewatermark: Boolean = True;
  saveaspng: Boolean = False;

resourcestring
  RS_RemoveWatermark = 'Remove watermark';
  RS_SaveAsPNG = 'Save as PNG';

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/manga/') then
  begin
    Result := NO_ERROR;
    XPathHREFAll('//*[@class="manga_list"]/ul/li/a', MangaInfo.FHTTP.Document, ALinks, ANames);
  end;
end;

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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//*[@class="cover"]/img/@src'));
          if title = '' then title := XPathString('//title/substring-before(.," Manga - Read")');
          authors := XPathString('//div[@id="title"]/table/tbody/tr[2]/td[2]');
          artists := XPathString('//div[@id="title"]/table/tbody/tr[2]/td[3]');
          genres := XPathString('//div[@id="title"]/table/tbody/tr[2]/td[4]');
          summary := XPathString('//p[@class="summary"]');
          status := MangaInfoStatusIfPos(XPathString('//div[@id="series_info"]/div[5]/span'));
          for v in XPath('//ul[@class="chlist"]/li/div/*[self::h3 or self::h4]') do
          begin
            chapterLinks.Add(StringReplace(XPathString('a/@href', v), '1.html', '', [rfReplaceAll]));
            chapterName.Add(XPathString('string-join(.//*[not(contains(@class,"newch"))]/text()," ")', v));
          end;
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL) + '1.html') then begin
      Result := True;
      PageNumber := XPathCount('//*[@id="top_bar"]//select[@onchange="change_page(this)"]/option[@value!="0"]', Document);
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
    if GET(MaybeFillHost(Module.RootURL, AURL) + IncStr(DownloadThread.WorkId) + '.html') then begin
      Result := True;
      s := XPathString('//div[@class="read_img"]//img[@id="image"]/@src', Document);
      PageLinks[DownloadThread.WorkId] := s;
      // remove invalid last page
      if DownloadThread.WorkId = PageLinks.Count - 1 then
      begin
        s := LowerCase(RemovePathDelim(s));
        if (RightStr(s, 10) = 'compressed') or
          (Pos('/compressed?', s) <> 0) then
        begin
          PageLinks.Delete(DownloadThread.WorkId);
          PageNumber := PageLinks.Count;
        end;
      end;
    end;
  end;
end;

function AfterImageSaved(const AFilename: String; const Module: TModuleContainer): Boolean;
begin
  Result := True;
  if removewatermark then
    Result := MangaFoxWatermark.RemoveWatermark(AFilename, saveaspng);
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaFox';
    RootURL := 'http://fanfox.net';
    Category := 'English';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
    OnAfterImageSaved := @AfterImageSaved;
    AddOptionCheckBox(@removewatermark, 'RemoveWatermark', @RS_RemoveWatermark);
    AddOptionCheckBox(@saveaspng, 'SaveAsPNG', @RS_SaveAsPNG);
  end;
end;

initialization
  RegisterModule;

end.
