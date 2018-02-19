unit PecintaKomik;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/directory/';

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//section[@class="cols"]//div[@class="col-cnt"]/ul/li/a') do begin
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
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        coverLink := query.XPathString('//section[@class="post"]/img/@src');
        if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
        if title = '' then title := query.XPathString('//div[@class="post-cnt"]/h2');
        for v in query.XPath('(//div[@class="post-cnt"])[1]/ul/li') do begin
          s := v.toString;
          if Pos('Author(s):', s) = 1 then authors := SeparateRight(s, ':') else
          if Pos('Artist(s):', s) = 1 then artists := SeparateRight(s, ':') else
          if Pos('Genre:', s) = 1 then genres := SeparateRight(s, ':') else
          if Pos('Sinopsis:', s) = 1 then summary := SeparateRight(s, ':');
        end;
        for v in query.XPath('(//div[@class="post-cnt"])[2]/ul/li/a') do begin
          chapterLinks.Add(v.toNode.getAttribute('href'));
          chapterName.Add(query.XPathString('text()', v.toNode));
        end;
        InvertStrings([chapterLinks, chapterName])
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
        PageNumber := query.XPath('//select[@name="page"]/option').Count;
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
  s, b: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    s := RemoveURLDelim(AURL);
    if DownloadThread.WorkId > 0 then s += '/' + IncStr(DownloadThread.WorkId);
    if GET(FillHost(Module.RootURL, s)) then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        b := query.XPathString('//base/@href');
        if b = '' then b := Module.RootURL;
        s := query.XPathString('//img[@class="picture"]/@src');
        if s <> '' then PageLinks[DownloadThread.WorkId] := MaybeFillHost(b, s);
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
    Website := 'PecintaKomik';
    RootURL := 'http://www.pecintakomik.com';
    Category := 'Indonesian';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
