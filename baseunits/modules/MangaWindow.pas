unit MangaWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, RegExpr;

implementation

const
  dirurl = '/browse?sort=create&page=';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '1') then begin
    Result := NO_ERROR;
    s := XPathString('(//ul[contains(@class, "pagination")])[1]/li[last()-1]', MangaInfo.FHTTP.Document);
    Page := StrToIntDef(s, 1);
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + IncStr(AURL)) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
    try
      for v in XPath('//div[@id="series-list"]/div/div') do begin
        ALinks.Add(XPathString('a/@href', v));
        s := XPathString('span[contains(@class, "flag")]/@class', v);
        if s <> '' then begin
          s := RegExprGetMatch('flag_(\w+)\b', s, 1);
          if s <> 'united_kingdom' then s := ' [' + UpperCase(s) + ']'
          else s := ' [EN]';
        end;
        s := XPathString('a', v) + s;
        ANames.Add(s);
      end;
    finally
      Free;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  s: String;
  v, obj: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    s := MaybeFillHost(Module.RootURL, AURL);
    if GET(s) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          s := XPathString('//script[matches(., "var\s+images")]');
          s := RegExprGetMatch('var\s+images\s+\=\s+\{(.+?)\}', s, 1);
          s := '{' + s + '}';
          ParseHTML(s);
          obj := XPath('json(*)');
          for v in XPath('json(*)()') do
            PageLinks.Add(obj.getProperty(v.toString).toString);
        finally
          Free;
        end;
    end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  a: TStringArray;
  i: Integer;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//div[contains(@class, "attr-cover")]/img/@src'));
          if title = '' then begin
            title := XPathString('//h3[@class="item-title"]');
            s := XPathString('//h3[@class="item-title"]/parent::*/span[contains(@class, "flag")]/@class');
            if s <> '' then begin
              s := RegExprGetMatch('flag_(\w+)\b', s, 1);
              if s <> 'united_kingdom' then s := ' [' + UpperCase(s) + ']'
              else s := ' [EN]';
              title += s;
            end;
          end;
          authors := XPathStringAll('//div[@class="attr-item" and contains(b, "Authors")]/span');
          genres := XPathStringAll('//div[@class="attr-item" and contains(b, "Genres")]/span');
          a := genres.Split('/');
          for i := Low(a) to High(a) do a[i] := Trim(a[i]);
          genres := ''.Join(', ', a);
          status := MangaInfoStatusIfPos(XPathString('//div[@class="attr-item" and contains(b, "Status")]/span'));
          summary := XPathStringAll('//p[@class="summary-set"]/text()', '');
          XPathHREFAll('//div[contains(@class, "chapter-list")]/div[@class="main"]/div/a', chapterLinks, chapterName);
          InvertStrings([chapterLinks, chapterName]);
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
      SortedList := True;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    end;
  end;

begin
  AddWebsiteModule('MangaWindow', 'https://mangawindow.net');
  AddWebsiteModule('Batoto', 'https://bato.to');
end;

initialization
  RegisterModule;

end.

