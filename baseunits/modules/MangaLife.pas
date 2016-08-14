unit MangaLife;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread;

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
      s += '?start=';
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

function fixchapterurl(const u: string): string;
begin
 result:=fixcleanurl(u);
 if (pos('&page=1',result)<>0) or
    (pos('/page-1',result)<>0) then
   setlength(result,length(result)-7);
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
  i: Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if (Module = MMangaTraders) and (Pos('?series=', url) <> 0) then
      url := Module.RootURL + '/read-online/' + SeparateRight(url, '?series=');
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//meta[@property="og:image"]/@content'));
          if title = '' then title := XPathString('//*[@class="row"]//h1');
          authors := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Author:")]'), ':');
          artists := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Artist:")]'), ':');
          status := MangaInfoStatusIfPos(XPathString(
            '//*[@class="row"][starts-with(.,"Scanlation Status:")]'),
            'ongoing',
            'completed');
          genres := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Genre:")]'), ':');
          summary := Trim(SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Description:")]'), ':'));
          if Module.Website = 'MangaTraders' then
            v := XPath('//div[@class="well"]/div[@class="row"]/div/a[contains(@href,"/read-online/") and not(@class)]')
          else
          begin
            if Module.Website = 'MangaSee' then
              s := 'div.col-lg-12:nth-child(8)>div>div:nth-child(1)>a'
            else
              s := 'div.list>div>div>a';
            v := CSS(s)
          end;
          if v.Count > 0 then
          begin
            for i := 1 to v.Count do
            begin
              chapterLinks.Add(fixchapterurl(v.get(i).toNode.getAttribute('href')));
              chapterName.Add(v.get(i).toString);
            end;
            InvertStrings([chapterLinks, chapterName]);
          end;
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
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(fixchapterurl(MaybeFillHost(Module.RootURL, AURL))) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          XPathStringAll('//p/img/@src', PageLinks);
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
  AddWebsiteModule('MangaLife', 'http://manga.life');
  MMangaSee := AddWebsiteModule('MangaSee', 'http://mangasee.co');
  MMangaTraders := AddWebsiteModule('MangaTraders', 'http://mangatraders.org');
end;

initialization
  RegisterModule;

end.
