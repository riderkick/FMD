unit SubManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, RegExpr;

implementation

const
  dirurl = '/series';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '10000000') then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        page := StrToIntDef(XPathString('//ul[@class="pagination"]/li[last()-1]'), 1);
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
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//table[@class="caps"]/tbody/tr/td[1]/a') do begin
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
          coverLink := XPathString('//p[@class="cb"]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//div[@id="b"]//h1/a');
          summary := XPathString('//div[@id="b"]/div[2]/p[3]');
          genres := XPathString('//div[@id="b"]/div[2]/p[4]');
          authors := XPathString('//div[@id="b"]/div[2]/p[5]/a');
          if XPath('//table[@class="caps"]/tbody/tr').Count > 0 then
            if GET(AppendURLDelim(url) + 'completa') then begin
              ParseHTML(Document);
              with TRegExpr.Create do
                try
                  Expression := '^.*/(\d+)/?$';
                  for v in XPath('//table[@class="caps"]/tbody/tr/td[@class="s"]/a') do begin
                    s := v.toNode.getAttribute('href');
                    if s <> '' then begin
                      s := Replace(s, '/c/$1', True);
                      chapterLinks.Add(s);
                      chapterName.Add(v.toString);
                    end;
                  end;
                finally
                  Free;
                end;
              InvertStrings([chapterLinks, chapterName]);
            end;
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
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageNumber := XPath('//table[1]/tbody//select/option').Count;
        finally
          Free;
        end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    s := FillHost(Module.RootURL, AURL);
    if DownloadThread.WorkId > 0 then
      s := s + '/' + IncStr(DownloadThread.WorkId);
    if GET(s) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[DownloadThread.WorkId] := XPathString('//div[@id="ab"]/a/img/@src');
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
    Website := 'SubManga';
    RootURL := 'http://submanga.com';
    Category := 'Spanish';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
