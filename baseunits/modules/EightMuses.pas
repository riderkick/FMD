unit EightMuses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager;

implementation

function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
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
  info.url := RemoveURLDelim(FillHost(Module.RootURL, URL));
  Source := TStringList.Create;
  try
    if GetPage(MangaInfo.FHTTP, TObject(Source), info.url, Reconnect) then
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          with info do begin
            coverLink := Query.XPathString('//div[@class="holder"]/a/img/@src');
            if Pos('//', coverLink) = 1 then coverLink := 'https:' + coverLink;
            if title = '' then
              title := Query.XPathString('//ul[@class="breadcrumbs"]/li[last()]');
            //multi
            if Query.XPathString('//div[@class="holder"]/a') <> '' then begin
              for v in Query.XPath('//div[@class="holder"]/a') do begin
                chapterLinks.Add(v.toNode.getAttribute('href'));
                s := v.toString;
                if (title <> '') and (Pos('Issue', s) = 1) then s := title + ' ' + s;
                chapterName.Add(s);
              end;
            end
            else begin
              chapterLinks.Add(info.url);
              chapterName.Add(title);
            end;
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

function GetPageNumber(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  Container: TTaskContainer;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  with Container do begin
    Source := TStringList.Create;
    try
      if GetPage(DownloadThread.FHTTP, TObject(Source),
        RemoveURLDelim(FillHost(Module.RootURL, URL)), Manager.retryConnect) then
        if Source.Count > 0 then
        begin
          Result := True;
          Query := TXQueryEngineHTML.Create(Source.Text);
          try
            for v in Query.XPath('//div[@class="holder"]/a/@href') do begin
              PageContainerLinks.Add(v.toString);
              Inc(PageNumber);
            end;
          finally
            Query.Free;
          end;
        end;
    finally
      Source.Free;
    end;
  end;
end;

function GetImageURL(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  rurl: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container do begin
    Source := TStringList.Create;
    try
      rurl := RemoveURLDelim(FillHost(Module.RootURL, PageContainerLinks[DownloadThread.WorkCounter]));
      if GetPage(DownloadThread.FHTTP, TObject(Source), rurl, Manager.retryConnect) then
        if Source.Count > 0 then
        begin
          Result := True;
          Query := TXQueryEngineHTML.Create(Source.Text);
          try
            rurl := Query.XPathString('//img[@id="image"]/@src');
            if Pos('//', rurl) = 1 then rurl := 'https:' + rurl;
            PageLinks[DownloadThread.workCounter] := rurl;
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
    Website := '8Muses';
    RootURL := 'https://www.8muses.com';
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
