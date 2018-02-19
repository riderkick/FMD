unit Luscious;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, RegExpr;

implementation

const
  dirurl = '/c/-/albums/frontpage/0/t/manga/sorted/new/page/';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '1/') then
  begin
    Result := NO_ERROR;
    Page := StrToIntDef(TrimChar(XPathString('//*[@class="pagination"]/*[@class="last"]/a/@href/substring-after(.,"/new/page")', MangaInfo.FHTTP.Document), ['/']), 1);
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + IncStr(AURL) + '/') then
  begin
    Result := NO_ERROR;
    XPathHREFtitleAll('//*[@id="albums_wrapper"]//*[@class="item_cover"]/a', MangaInfo.FHTTP.Document, ALinks, ANames);
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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//*[@class="album_cover_item"]//img/@src'));
          if title = '' then title := XPathString('//*[@class="album_cover"]/h2');
          artists := XPathString('//*[@id="tag_section"]/ol/li/a[starts-with(.,"Artist")]/text()[last()]');
          genres := XPathString('//*[@id="tag_section"]/ol/string-join(li/a/text()[last()],", ")');
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
  n: String;
  i: Integer;
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
          while True do
          begin
            XPathStringAll('//*[@class="picture_page"]//img/@data-src', PageLinks);
            n := XPathString('//*[@class="pagination"]/a/@href');
            if n = '' then Break;
            if GET(MaybeFillHost(Module.RootURL, n)) then
              ParseHTML(Document)
            else
            begin
              PageLinks.Clear;
              Break;
            end;
          end;
          if PageLinks.Count <> 0 then
            with TRegExpr.Create('\.\d+x\d+(\.\w+)') do
              try
                for i := 0 to PageLinks.Count - 1 do
                  PageLinks[i] := Replace(PageLinks[i], '$1', True);
              finally
                Free;
              end;
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
    Website := 'Luscious';
    RootURL := 'https://luscious.net';
    Category := 'H-Sites';
    SortedList := True;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
