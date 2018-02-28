unit MangaLife;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synacode;

implementation

uses
  synautil;

const
  dirURL = '/directory/';
  diralpha = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ';

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
  s := Module.RootURL + dirURL;
  if AURL <> '0' then
    s += diralpha[StrToIntDef(AURL, 0) + 1];
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
  s: String;
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
          title := XPathString('//*[@class="row"]//h1');
          if ResultCode = 404 then
          begin
            status := '-1';
            Exit;
          end;
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//meta[@property="og:image"]/@content'));
          authors := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Author")]'), ':');
          artists := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Artist")]'), ':');
          status := XPathString('//*[@class="row"][starts-with(.,"Scanlation Status")]');
          if status = '' then
            status := XPathString('//*[@class="row"][starts-with(.,"Status")]');
          if status <> '' then
            status := MangaInfoStatusIfPos(status);
          genres := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Genre")]'), ':');
          summary := Trim(SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Description")]'), ':'));
          for v in XPath('//div[@class="list chapter-list"]//a') do
          begin
            s := v.toNode.getAttribute('href');
            if Pos('-page-1', s) <> 0 then
              s := StringReplace(s, '-page-1', '', []);
            chapterLinks.Add(s);
            chapterName.Add(XPathString('span[@class="chapterLabel"]', v));
          end;
          InvertStrings([chapterLinks, chapterName]);
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
  with DownloadThread.FHTTP, DownloadThread.Task.Container do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          XPathStringAll('//*[contains(@class,"image-container")]//img/@src', PageLinks);
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
      Category := 'English';
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
    end;
  end;

begin
  AddWebsiteModule('MangaLife', 'http://mangalife.us');
  AddWebsiteModule('MangaSee', 'http://mangaseeonline.us');
end;

initialization
  RegisterModule;

end.
