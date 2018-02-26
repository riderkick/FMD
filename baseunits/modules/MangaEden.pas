unit MangaEden;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  diren = '/en/en-directory/';
  dirit = '/en/it-directory/';
var
  MMangaEden, MMangaEdenIT,
  MPervEden, MPervEdenIT: TModuleContainer;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer; const WorkPtr: Integer;
  const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL;
  if (Module = MMangaEden) or (Module = MPervEden) then
    s += diren
  else
    s += dirit;
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(XPathString('//*[@class="pagination pagination_bottom"]/a[last()-1]'), 1);
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL;
  if (Module = MMangaEden) or (Module = MPervEden) then
    s += diren
  else
    s += dirit;
  if AURL <> '0' then
    s := s + '?page=' + IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//table[@id="mangaList"]//tr/td[1]/a') do begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(v.toString);
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//*[starts-with(@class,"mangaImage")]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//*[@class="manga-title"]');
          authors := XPathStringAll('//*[@class="rightBox"]/a[contains(@href,"/?author=")]');
          artists := XPathStringAll('//*[@class="rightBox"]/a[contains(@href,"/?artist=")]');
          genres := XPathStringAll('//*[@class="rightBox"]/a[contains(@href,"/?categories")]');
          summary := XPathString('//*[@id="mangaDescription"]');
          s := CleanString(XPathString('//*[@class="rightBox"]'));
          AddCommaString(genres, Trim(GetBetween('Type ', ' Status', s)));
          s := AnsiLowerCase(s);
          if Pos('status ongoing', s) > 0 then
            status := '1'
          else if Pos('status completed', s) > 0 then
            status := '0';
          for v in XPath('//table//tr/td/a[@class="chapterLink"]') do
          begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(XPathStringAll('*', ' ', v));
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
var
  s: String;
  source: TStringList;
  i: Integer;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL)) then begin
      Result := True;

      s := '';
      source := TStringList.Create;
      try
        source.LoadFromStream(Document);
        if source.Count > 0 then
          for i := 0 to source.Count - 1 do
            if Pos('var pages = ', source[i]) > 0 then
            begin
              s := source[i];
              s := GetBetween('[', ']', s);
              if s <> '' then
                s := '[' + s + ']';
              Break;
            end;
      finally
        source.Free;
      end;

      with TXQueryEngineHTML.Create do
        try
          if s <> '' then
          begin
            ParseHTML(s);
            PageLinks.AddText(XPathStringAll('json(*)()("fs")', LineEnding));
          end;

          if PageLinks.Count = 0 then
          begin
            ParseHTML(Document);
            PageNumber := XPath('//select[@id="pageSelect"]/option').Count;
          end;
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
  s := RemoveURLDelim(AURL);
  if RightStr(s, 2) = '/1' then
    SetLength(s, Length(s) - 1);
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    s := AppendURLDelim(s) + IncStr(DownloadThread.WorkId) + '/';
    if GET(FillHost(Module.RootURL, s)) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[DownloadThread.WorkId] :=
            XPathString('//img[@id="mainImg"]/@src');
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;

  function AddWebsiteModule(const AWebsite, ARootURL, ACategory: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do begin
      Website := AWebsite;
      RootURL := ARootURL;
      Category := ACategory;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnGetImageURL := @GetImageURL;
    end;
  end;

begin
  MMangaEden := AddWebsiteModule('MangaEden', 'http://www.mangaeden.com', 'English');
  MMangaEdenIT := AddWebsiteModule('MangaEden_IT', 'http://www.mangaeden.com', 'Italian');
  MPervEden := AddWebsiteModule('PervEden', 'http://www.perveden.com', 'H-Sites');
  MPervEdenIT := AddWebsiteModule('PervEden_IT', 'http://www.perveden.com', 'H-Sites');
end;

initialization
  RegisterModule;

end.
