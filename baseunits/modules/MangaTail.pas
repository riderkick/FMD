unit MangaTail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML;

implementation

uses FMDVars;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  s: String;
  v: IXQValue;
  i, x: Integer;
begin
  Result := NET_PROBLEM;
  s := Module.RootURL + '/directory';
  if AURL <> '0' then
    s += '?page=' + AURL;
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do try
      i := 1;
      for v in XPath('//ul[@class="pagination"]/li') do begin
        x := StrToIntDef(v.toString, 1);
        if x > i then i := x;
      end;
      updateList.CurrentDirectoryPageNumber := i - 1;
      XPathHREFAll('//table[contains(@class,"directory_list")]//tr/td[1]/a', ALinks, ANames);
      for i := 0 to ANames.Count - 1 do
      begin
        s := ANames[i];
        if RightStr(s, 6) = ' Manga' then
        begin
          SetLength(s, Length(s) - 6);
          ANames[i] := s;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do try
        coverLink := XPathString('//*[@class="bookface"]/img/@src');
        if title = '' then begin
          title := XPathString('//h1');
          if RightStr(title, 6) = ' Manga' then
            SetLength(title, Length(title) - 6);
        end;
        status := MangaInfoStatusIfPos(XPathString('//*[contains(@class,"field-status")]'));
        XPathHREFAll('//table[contains(@class,"chlist")]//tr/td[1]/a', chapterLinks, chapterName);
        InvertStrings([chapterLinks, chapterName]);
        s := XPathStringAll('//iframe[contains(.,"field-name")]/substring-after(substring-before(.,"""}}"),":en"":""")', LineEnding);
        if s <> '' then begin
          s := StringReplace(s, '\n', LineEnding, [rfReplaceAll]);
          s := StringReplace(s, '\', '', [rfReplaceAll]);
          ParseHTML(s);
          coverLink := XPathString('//img/@src');
          authors := XPathString('//*[contains(@class,"author")]/*[@class="field-items"]/string-join(.//text(),", ")');
          artists := XPathString('//*[contains(@class,"artist")]/*[@class="field-items"]/string-join(.//text(),", ")');
          genres := XPathString('//*[contains(@class,"genres")]/*[@class="field-items"]/string-join(.//text(),", ")');
          summary := XPathString('//*[contains(@class,"summary")]/*[@class="field-items"]//text()');
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
    if GET(MaybeFillHost(Module.RootURL, AURL + '?page=all')) then
    begin
      Result := True;
      XPathStringAll('//*[@id="images"]//img[not(contains(@src,"adsense"))]/@src', Document, PageLinks);
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
  AddWebsiteModule('MangaTail', 'http://www.mangatail.com');
  AddWebsiteModule('MangaSail', 'http://www.mangasail.com');
end;

initialization
  RegisterModule;

end.
