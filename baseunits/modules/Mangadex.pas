unit Mangadex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, RegExpr, FMDVars;

implementation

const
  PerPage = 100;

var
  showalllang: Boolean = False;
  showscangroup: Boolean = False;

resourcestring
  RS_ShowAllLang = 'Show all language';
  RS_ShowScanGroup = 'Show scanlation group';

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s, lang, group: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    Cookies.Values['mangadex_h_toggle'] := '1';
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          if title = '' then title := XPathString('//h3[@class="panel-title"]/text()');
          if Pos('emailprotected', title) > 0 then
            title := Trim(ReplaceString(XPathString('//title'), '(Manga) - MangaDex', ''));
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//img[@alt="Manga image"]/@src'));
          authors := XPathString('//th[contains(., "Author")]/following-sibling::td/a');
          artists := XPathString('//th[contains(., "Artist")]/following-sibling::td/a');
          genres := XPathStringAll('//th[contains(., "Genres")]/following-sibling::td/span');
          summary := XPathString('//th[contains(., "Description")]/following-sibling::td');
          status := MangaInfoStatusIfPos(XPathString('//th[contains(., "Status")]/following-sibling::td'));
          if showalllang then
            s := '//div[contains(@class, "tab-content")]//table/tbody/tr/td[1]/a'
          else
            s := '//div[contains(@class, "tab-content")]//table/tbody/tr[td[2]/img/@title="English"]/td[1]/a';
          for v in XPath(s) do begin
            chapterLinks.Add(v.toNode().getAttribute('href'));
            s := v.toString();
            if showalllang then begin
              lang := XPathString('parent::td/parent::tr/td[2]/img/@title', v);
              if lang <> '' then s := s + ' [' + lang + ']';
            end;
            if showscangroup then begin
              group := XPathString('parent::td/parent::tr/td[3]/a', v);
              if group <> '' then s := s + ' [' + group + ']';
            end;
            chapterName.Add(s);
          end;
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  s, dataurl, server, pages: String;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    Cookies.Values['mangadex_h_toggle'] := '1';
    if GET(MaybeFillHost(Module.RootURL, AURL)) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
      try
        s := XPathString('//script[contains(., "var page_array")]');
        dataurl := GetBetween('var dataurl = ''', ''';', s);
        server := GetBetween('var server = ''', ''';', s);
        pages := '[' + GetBetween('var page_array = [', '];', s) + ']';
        ParseHTML(pages);
        for v in XPath('json(*)()') do
          PageLinks.Add(MaybeFillHost(Module.RootURL, server + dataurl + '/' + v.toString));
      finally
        Free;
      end;
    end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  s, p: String;
begin
  Result := NET_PROBLEM;
  if Module.CurrentDirectoryIndex = 0 then
    s := '~'
  else
    s := ALPHA_LIST_UP[Module.CurrentDirectoryIndex + 1];
  p := IntToStr(StrToInt(AURL) * PerPage);
  MangaInfo.FHTTP.Cookies.Values['mangadex_h_toggle'] := '1';
  if MangaInfo.FHTTP.GET(Module.RootURL + '/titles/' + s + '/' + p) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        if AURL = '0' then begin
          updateList.CurrentDirectoryPageNumber := 1;
          s := XPathString('//li[@class="paging"]/a[contains(span/@title, "last")]/@href');
          if s <> '' then begin
            s := ReplaceRegExpr('^.+\/(\d+)$', s, '$1', True);
            updateList.CurrentDirectoryPageNumber := (StrToIntDef(s, 0) div PerPage) + 1;
          end;
        end;
        XPathHREFAll('//table/tbody/tr/td[2]/a', ALinks, ANames);
      finally
        Free;
      end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do begin
    Website := 'Mangadex';
    RootURL := 'https://mangadex.com';
    Category := 'English';
    Settings.MaxTaskLimit := 4;
    Settings.MaxConnectionLimit := 4;
    TotalDirectory := Length(ALPHA_LIST_UP);
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    AddOptionCheckBox(@showalllang,'ShowAllLang', @RS_ShowAllLang);
    AddOptionCheckBox(@showscangroup,'ShowScanGroup', @RS_ShowScanGroup);
  end;
end;

initialization
  RegisterModule;

end.

