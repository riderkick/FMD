unit MangaBackup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, RegExpr;

implementation

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/genre/?add') then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := XPath('(//select)[1]/option').Count;
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
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL;
  if AURL = '0' then
    s := s + '/genre/?add'
  else
    s := s + '/genre/' + IncStr(AURL) + '?add';
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//div[@class="ls1"]/div//h3/a') do begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(XPathString('text()', v.toNode));
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
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//div[@class="cover"]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//h1/a');
          authors := XPathString('//table[@class="attr"]/tbody/tr[5]/td');
          artists := XPathString('//table[@class="attr"]/tbody/tr[6]/td');
          genres := '';
          for v in XPath('//table[@class="attr"]/tbody/tr[7]/td/a') do
            AddCommaString(genres, v.toString);
          summary := XPathString('//p[@class="summary"]');
          s := XPathString('//table[@class="attr"]/tbody/tr[9]/td');
          if s <> '' then begin
            s := LowerCase(s);
            if Pos('ongoing', s) > 0 then
              status := '1'
            else if Pos('completed', s) > 0 then
              status := '0';
          end;
          for v in XPath('//ul[@class="chapter"]/li/span/a') do begin
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
var
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//section[@id="viewer"]/div[@class="canvas"]/a[@class="img-link"]/img') do
            PageLinks.Add(v.toNode.getAttribute('src'));
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
    Website := 'MangaBackup';
    RootURL := 'http://mangabackup.com';
    Category := 'English';
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
