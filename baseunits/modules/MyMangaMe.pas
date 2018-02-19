unit MyMangaMe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread;

implementation

const
  dirurl = '/manga-directory/';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(XPathString('//script[contains(.,"totalPages")]/substring-before(substring-after(.,"totalPages: "),",")'), 1);
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
  s: String;
begin
  Result := NET_PROBLEM;
  s := Module.RootURL + dirurl;
  if AURL <> '0' then
    s := s + 'all/az/' + IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//div[@class="manga-hover-wrapper"]/a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(XPathString('div[@class="manga-details directory"]', v));
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
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//div[@class="manga-cover"]//img/@src'));
          if title = '' then title := XPathString('//h1');
          v := XPath('//div[@class="manga-data"]');
          authors := XPathString('b[starts-with(.,"Author:")]/following-sibling::text()[1]', v);
          artists := XPathString('b[starts-with(.,"Artist:")]/following-sibling::text()[1]', v);
          genres := XPathString('b[starts-with(.,"Genre:")]/string-join(following-sibling::a[preceding::b[1][starts-with(.,"Genre:")]],", ")', v);
          status := MangaInfoStatusIfPos(XPathString(
            'b[starts-with(.,"Status:")]/following-sibling::text()[1]', v),
            'Ongoing',
            'Completed');
          summary := XPathStringAll('b[starts-with(.,"Sypnosis:")]/following-sibling::text()[preceding::b[1][[starts-with(.,"Sypnosis:")]]]', LineEnding, v);
          for v in XPath('//section[@class="section-chapter"]/div/div[@class="row"]//a') do
          begin
            s := v.toNode.getAttribute('href');
            if RightStr(s, 2) = '/1' then
              SetLength(s, Length(s) - 2);
            chapterLinks.Add(s);
            chapterName.Add(v.toString);
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
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL) + '/1') then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageNumber := XPath('(//select[@id="page-dropdown"])[1]/option').Count;
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
  with DownloadThread.Task.Container, DownloadThread, FHTTP do
    if GET(MaybeFillHost(Module.RootURL, AURL) + '/' + IncStr(WorkId)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[WorkId] := XPathString('//script[contains(.,"my_image0.src")]/substring-before(substring-after(.,"my_image0.src = ''"),"'';")');
        finally
          Free;
        end;
    end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MyMangaMe';
    RootURL := 'http://mymanga.me';
    Category := 'English';
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
