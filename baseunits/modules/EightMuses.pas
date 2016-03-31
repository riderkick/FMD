unit EightMuses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread;

implementation

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    url := RemoveURLDelim(FillHost(Module.RootURL, AURL));
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//div[@class="holder"]/a/img/@src');
          if Pos('//', coverLink) = 1 then coverLink := 'https:' + coverLink;
          if title = '' then
            title := XPathString('//ul[@class="breadcrumbs"]/li[last()]');
          //multi
          if XPathString('//div[@class="holder"]/a') <> '' then begin
            for v in XPath('//div[@class="holder"]/a') do begin
              chapterLinks.Add(v.toNode.getAttribute('href'));
              s := v.toString;
              if (title <> '') and (Pos('Issue', s) = 1) then s := title + ' ' + s;
              chapterName.Add(s);
            end;
          end
          else begin
            chapterLinks.Add(url);
            chapterName.Add(title);
          end;
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  v: IXQValue;
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.manager.container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    if GET(RemoveURLDelim(FillHost(Module.RootURL, AURL))) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//div[@class="holder"]/a/img/@src') do
          begin
            s := v.toString;
            if LeftStr(s, 2) = '//' then
              s := 'https:' + s;
            s := StringReplace(s, '/th/', '/fu/', [rfIgnoreCase]);
            PageLinks.Add(s);
          end;
          if PageLinks.Count = 0 then
          begin
            for v in XPath('//div[@class="holder"]/a/@href') do
              PageContainerLinks.Add(v.toString);
            PageNumber := PageContainerLinks.Count;
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
  rurl: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.manager.container do begin
    rurl := RemoveURLDelim(FillHost(Module.RootURL, PageContainerLinks[DownloadThread.WorkCounter]));
    if GET(rurl) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          rurl := XPathString('//img[@id="image"]/@src');
          if Pos('//', rurl) = 1 then
            rurl := 'https:' + rurl;
          PageLinks[DownloadThread.workCounter] := rurl;
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
