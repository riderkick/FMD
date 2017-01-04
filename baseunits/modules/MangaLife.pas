unit MangaLife;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML;

implementation

uses
  synautil;

const
  dirURL = '/directory/';
  diralpha = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  dirURLmangasee = '/directory.php';
var
  MMangaSee,
  MMangaTraders: TModuleContainer;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  if (Module = MMangaSee) or
     (Module = MMangaTraders) then
    Page := Length(diralpha)
  else
    Page := 1;
end;

function fixcleanurl(const u: string): string;
begin
  Result := u;
  if Result = '' then Exit;
  while Result[1] = '.' do
    Delete(Result, 1, 1);
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  s := Module.RootURL;
  if Module = MMangaSee then
    s += dirURLmangasee
  else
    s+=dirURL;
  if AURL <> '0' then
  begin
    if Module = MMangaSee then
      s += '?c='
    else
    if Module = MMangaTraders then
      s += '?q=';
    s += diralpha[StrToIntDef(AURL, 0) + 1]
  end;
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//*[@id="content"]//a') do
        begin
          ALinks.Add(fixcleanurl(v.toNode.getAttribute('href')));
          ANames.Add(v.toString);
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
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := RemoveURLDelim(FillHost(Module.RootURL, AURL));
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
         coverLink := MaybeFillHost(Module.RootURL, XPathString('//meta[@property="og:image"]/@content'));
          if title = '' then title := XPathString('//*[@class="row"]//h1');
          authors := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Author")]'), ':');
          artists := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Artist")]'), ':');
          status := XPathString('//*[@class="row"][starts-with(.,"Scanlation Status")]');
          if status = '' then
            status := XPathString('//*[@class="row"][starts-with(.,"Status")]');
          if status <> '' then
            status := MangaInfoStatusIfPos(status);
          genres := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Genre")]'), ':');
          summary := Trim(SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Description")]'), ':'));
          for v in XPath('//div[@class="list chapter-list"]/a') do
          begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(v.toString);
          end;
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
  with DownloadThread.FHTTP, DownloadThread.Task.Container do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//img[@class="CurImage"]') do
            PageLinks.Add(v.toNode.getAttribute('src'));
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;

  function AddWebsiteModule(AWebsite, ARootURL: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
    end;
  end;

begin
  AddWebsiteModule('MangaLife', 'http://mangalife.org');
  MMangaSee := AddWebsiteModule('MangaSee', 'http://mangaseeonline.net');
  MMangaTraders := AddWebsiteModule('MangaTraders', 'http://mangatraders.biz');
end;

initialization
  RegisterModule;

end.
