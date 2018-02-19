unit Hakihome;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/listmangahentai.html';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      s := query.XPathString('//*[@class="nav"]/li[last()-1]/a/@href');
      if s <> '' then Page := StrToIntDef(GetBetween('/pagel/', '/', s), 1);
    finally
      query.Free;
    end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL + dirurl;
  if AURL <> '0' then s += '/pagel/' + IncStr(AURL) + '/';
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//table[@class="listing"]/tbody/tr/td[2]/a') do begin
        ALinks.Add(v.toNode.getAttribute('href'));
        ANames.Add(v.toString);
      end;
    finally
      query.Free;
    end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        coverLink := query.XPathString('//*[@class="noidung"]//img/@src');
        if title = '' then title := query.XPathString('//*[@class="tuade"]');
        artists := SeparateRight(query.XPathString('//*[@class="art lefts"]'), ':');
        genres := SeparateRight(query.XPathString('//*[@class="category lefts"]'), ':');
        AddCommaString(genres, SeparateRight(query.XPathString('//*[@class="tag"]'), ':'));
        AddCommaString(genres, SeparateRight(query.XPathString('//*[@class="lan rights"]'), ':'));
        for v in query.XPath('//table[@class="listing"]//a[@class="readchap"]') do begin
          chapterLinks.Add(v.toNode.getAttribute('href'));
          chapterName.Add(Trim(title + ' ' + v.toString));
        end;
        if (chapterName.Count = 1) and (title <> '') then
          chapterName[0] := title;
        InvertStrings([chapterLinks, chapterName]);
      finally
        query.Free;
      end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
  s: String;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    Cookies.Values['ReadType'] := '2';
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        PageNumber := query.XPath('(//*[@id="topn"]/span/select)[last()]/option').Count;
        s := query.XPathString('//*[@id="contentchap"]//script[contains(.,"var jsondata")]');
        if s <> '' then begin
          s := Trim(GetBetween('=', ';', s));
          query.ParseHTML(s);
          for v in query.XPath('json(*)()') do
            PageLinks.Add(v.toString);
        end;
      finally
        query.Free;
      end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    s := AppendURLDelim(AURL) + IncStr(DownloadThread.WorkId) + '/';
    if GET(FillHost(Module.RootURL, s)) then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        PageLinks[DownloadThread.WorkId] := query.XPathString('//*[@id="con"]//img/@src');
      finally
        query.Free;
      end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Hakihome';
    RootURL := 'http://hakihome.com';
    Category := 'H-Sites';
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
