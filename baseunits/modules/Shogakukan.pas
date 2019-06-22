unit Shogakukan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML;

implementation

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do try
        coverLink := XPathString('//meta[@property="og:image"]/@content');
        if title = '' then title := XPathString('//title/substring-before(.," | ")');
        chapterLinks.Add(url);
        chapterName.Add(title);
      finally
        Free;
      end;
    end;
  end;
end;


function TaskStart(const Task: TTaskContainer; const Module: TModuleContainer): Boolean;
begin
  Result := True;
  if Task = nil then Exit;
  Task.PageLinks.Clear;
  Task.PageLinks.Clear;
  Task.PageNumber := 0;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    if GET(MaybeFillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do try
        XPathStringAll('//*[@id="book_data_area"]/input[@data-key="imageCodes"]/@value', PageLinks);
        PageContainerLinks.Add(XPathString('//*[@id="book_data_area"]/input[@data-key="isbn"]/@value'));
        PageContainerLinks.Add(XPathString('//*[@id="book_data_area"]/input[@data-key="vsid"]/@value'));
      finally
        Free;
      end;
    end;
  end;
end;

function DownloadImage(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread, DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    if WorkId = 0 then
      Headers.Add('Referer: ' + Module.RootURL + '/' + PageContainerLinks[1])
    else
      Headers.Add('Referer: ' + Module.RootURL + '/' + PageContainerLinks[1] + '?page=' + IntToStr(WorkId));
    if POST(Module.RootURL + '/imgDeliver?gcode=' + PageContainerLinks[0],
      'base64=1&vsid=' + PageContainerLinks[1] + '&trgCode=' + PageLinks[WorkId]) then
      Result := Base64Decode(Document);
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Shogakukan';
    RootURL := 'https://shogakukan.tameshiyo.me';
    OnGetInfo := @GetInfo;
    OnTaskStart := @TaskStart;
    OnGetPageNumber := @GetPageNumber;
    OnDownloadImage := @DownloadImage;
  end;
end;

initialization
  RegisterModule;

end.
