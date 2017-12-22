unit EightMuses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, RegExpr;

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
          coverLink := XPathString('//*[@class="gallery"]/a/div/img/@data-src');
          if coverLink <> '' then
            coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if Pos('//', coverLink) = 1 then coverLink := 'https:' + coverLink;
          if title = '' then
            title := XPathString('//*[@class="top-menu-breadcrumb"]//li[last()]');
          if ExecRegExpr('^(?i)issue\s\d+', title) then
          begin
            s := title;
            title := XPathString('//*[@class="top-menu-breadcrumb"]//li[last()-1]');
          end;
          //multi
          if XPathString('//*[@class="gallery"]/a') <> '' then begin
            for v in XPath('//*[@class="gallery"]/a') do begin
              s := v.toNode.getAttribute('href');
              if s <> '' then
              begin
                chapterLinks.Add(s);
                s := v.toString;
                if (title <> '') and (Pos('Issue', s) = 1) then
                  s := title + ' - ' + s;
                chapterName.Add(s);
              end;
            end;
          end
          else begin
            chapterLinks.Add(url);
            if s <> '' then
              chapterName.Add(title + ' - ' + s)
            else
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
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    if GET(RemoveURLDelim(FillHost(Module.RootURL, AURL))) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//*[@class="gallery"]/a/div/img/@data-src') do
          begin
            s := v.toString;
            if s <> '' then
            begin
              s := StringReplace(s, '/th/', '/fm/', [rfIgnoreCase]);
              s := MaybeFillHost(Module.RootURL, s);
              if Pos('//', s) = 1 then
                s := 'https:' + s;
              PageLinks.Add(s);
            end;
          end;
          if PageLinks.Count = 0 then
          begin
            for v in XPath('//*[@class="gallery"]/a/@href') do
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
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    if DownloadThread.WorkId >= PageContainerLinks.Count then Exit;
    rurl := RemoveURLDelim(FillHost(Module.RootURL, PageContainerLinks[DownloadThread.WorkId]));
    if GET(rurl) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          rurl := XPathString('string-join((//*[@id="imageDir"]/@value,//*[@id="imageName"]/@value),"")');
          if rurl <> '' then
          begin
            rurl := MaybeFillHost(Module.RootURL, rurl);
            PageLinks[DownloadThread.WorkId] := rurl;
          end;
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
