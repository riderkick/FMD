unit MangaHere;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirURL = '/mangalist/';

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  v: IXQValue;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  Source := TStringList.Create;
  try
    if GetPage(MangaInfo.FHTTP, TObject(Source), Module.RootURL + dirURL) then
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          for v in Query.XPath('//a[@class="manga_info"]') do begin
            ALinks.Add(v.toNode.getAttribute('href'));
            ANames.Add(v.toString);
          end;
        finally
          Query.Free;
        end;
      end
      else
        Result := INFORMATION_NOT_FOUND;
  finally
    Source.Free;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  info: TMangaInfo;
  v: IXQValue;
  s: String;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  info := MangaInfo.mangaInfo;
  info.website := Module.Website;
  info.url := AppendURLDelim(FillHost(Module.RootURL, AURL));
  Source := TStringList.Create;
  try
    if MangaInfo.FHTTP.GET(info.url, TObject(Source)) then
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          with info do begin
            coverLink := Query.XPathString('//*[@class="manga_detail"]//img[@class="img"]/@src');
            if title = '' then
              title := Query.XPathString('//h1[@class="title"]');
            for v in Query.XPath('//*[@class="detail_topText"]/li') do begin
              s := v.toString;
              if Pos('Author(s):', s) = 1 then authors := SeparateRight(s, ':')
              else if Pos('Artist(s):', s) = 1 then artists := SeparateRight(s, ':')
              else if Pos('Genre(s):', s) = 1 then genres := SeparateRight(s, ':')
              else if Pos('Status:', s) = 1 then begin
                if Pos('Ongoing', s) > 0 then status := '1'
                else status := '0';
              end;
            end;
            summary := Query.XPathString('//*[@class="detail_topText"]/li/p[@id="show"]/text()');
            for v in Query.XPath('//*[@class="detail_list"]/ul/li/span[@class="left"]/a/@href') do
              chapterLinks.Add(v.toString);
            for v in Query.XPath('//*[@class="detail_list"]/ul/li/span[@class="left"]') do
              chapterName.Add(CleanString(v.toString));
            InvertStrings([chapterLinks, chapterName]);
          end;
        finally
          Query.Free;
        end;
      end
      else
        Result := INFORMATION_NOT_FOUND;
  finally
    Source.Free;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  Container: TTaskContainer;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.Task.Container;
  with Container do begin
    Source := TStringList.Create;
    try
      if GetPage(DownloadThread.FHTTP, TObject(Source),
        AppendURLDelim(FillHost(Module.RootURL, AURL)), Manager.retryConnect) then
        if Source.Count > 0 then
        begin
          Result := True;
          Query := TXQueryEngineHTML.Create(Source.Text);
          try
            PageNumber :=
              Query.XPath('//section[@class="readpage_top"]//span[@class="right"]/select/option').Count;
          finally
            Query.Free;
          end;
        end;
    finally
      Source.Free;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  rurl: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container do begin
    Source := TStringList.Create;
    try
      rurl := AppendURLDelim(FillHost(Module.RootURL, AURL));
      if DownloadThread.WorkId > 0 then
        rurl += IncStr(DownloadThread.WorkId) + '.html';
      if GetPage(DownloadThread.FHTTP, TObject(Source), rurl, Manager.retryConnect) then
        if Source.Count > 0 then
        begin
          Result := True;
          Query := TXQueryEngineHTML.Create(Source.Text);
          try
            PageLinks[DownloadThread.WorkId] :=
              Query.XPathString('//*[@id="viewer"]//img[@id="image"]/@src');
          finally
            Query.Free;
          end;
        end;
    finally
      Source.Free;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaHere';
    RootURL := 'http://www.mangahere.co';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
