unit MangaHome;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/directory';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      Page := query.XPath('//select/option').Count;
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
  if AURL <> '0' then s += '/' + IncStr(AURL) + '.html';
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//div[@class="cover-info"]/p[@class="title"]/a') do begin
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
  a, v, t: IXQValue;
  s: String;
  i: Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        coverLink := query.XPathString('//img[@class="detail-cover"]/@src');
        if title = '' then title := query.XPathString('//div[@class="manga-detail"]/h1');
        for v in query.XPath('//div[@class="manga-detail"]//p') do begin
          s := v.toString;
          if Pos('Author(s):', s) > 0 then authors := SeparateRight(s, ':') else
          if Pos('Artist(s):', s) > 0 then artists := SeparateRight(s, ':') else
          if Pos('Genre(s):', s) > 0 then genres := SeparateRight(s, ':') else
          if Pos('Status:', s) > 0 then begin
            s := LowerCase(s);
            if Pos('ongoing', s) > 0 then status := '1' else status := '0';
          end;
        end;
        summary := query.XPathString('//p[@id="show"]/text()');

        a := query.XPath('//ul[@class="detail-chlist"]/li/a/@href');
        v := query.XPath('//ul[@class="detail-chlist"]/li/a/span[1]');
        t := query.XPath('//ul[@class="detail-chlist"]/li/span[@class="vol"]');
        if (a.Count > 0) and (a.Count = v.Count) and (a.Count = t.Count) then
          for i := 1 to a.Count do begin
            chapterLinks.Add(a.get(i).toString);
            chapterName.Add(v.get(i).toString + ' ' + t.get(i).toString);
          end;
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
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        PageNumber := query.XPath('//div[@class="mangaread-pagenav"]/select/option').Count;
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
    s := RemoveURLDelim(AURL);
    if DownloadThread.WorkId > 0 then s += '/' + IncStr(DownloadThread.WorkId) + '.html';
    if GET(FillHost(Module.RootURL, s)) then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        PageLinks[DownloadThread.WorkId] := query.XPathString('//section[@id="viewer"]//img/@src');
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
    Website := 'MangaHome';
    RootURL := 'http://www.mangahome.com';
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
