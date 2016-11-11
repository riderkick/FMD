unit MangaFox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, MangaFoxWatermark;

implementation

var
  removewatermark: Boolean = True;
  saveaspng: Boolean = False;

resourcestring
  RS_RemoveWatermark = 'Remove watermark';
  RS_SaveAsPNG = 'Save as PNG';

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/manga/') then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//div[@class="manga_list"]/ul/li/a[starts-with(@class,"series_preview manga")]') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(v.toString);
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//div[@class="cover"]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//div[@class="cover"]/img/@alt');
          if title = '' then begin
            title := XPathString('//meta[@property="og:title"]/@content');
            if RightStr(title, 6) = ' Manga' then
              SetLength(title, Length(title) - 6);
          end;
          authors := XPathString('//div[@id="title"]/table/tbody/tr[2]/td[2]');
          artists := XPathString('//div[@id="title"]/table/tbody/tr[2]/td[3]');
          genres := XPathString('//div[@id="title"]/table/tbody/tr[2]/td[4]');
          s := XPathString('//div[@id="series_info"]/div[5]/span');
          if s <> '' then
          begin
            s := LowerCase(s);
            if Pos('ongoing', s) > 0 then
              status := '1'
            else if Pos('completed', s) > 0 then
              status := '0';
          end;
          summary := XPathString('//p[@class="summary"]');
          for v in XPath('//ul[@class="chlist"]/li') do
          begin
            s := XPathString('div/*/a[@class="tips"]/@href', v.toNode);
            if RightStr(s, 6) = '1.html' then
              SetLength(s, Length(s) - 6);
            chapterLinks.Add(s);
            chapterName.Add(Trim(XPathString('div/*/a[@class="tips"]', v.toNode) + ' ' +
              XPathString('div/*/span[@class="title nowrap"]', v.toNode)));
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
  v: IXQValue;
  i: Integer;
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    s := ChapterLinks[CurrentDownloadChapterPtr];
    if RightStr(s, 6) = '1.html' then begin
      SetLength(s, Length(s) - 6);
      ChapterLinks[CurrentDownloadChapterPtr] := s;
    end;
    if GET(AppendURLDelim(FillHost(Module.RootURL, s)) + '1.html') then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          v := XPath('//select[@onchange="change_page(this)"]/option');
          for i := v.Count downto 1 do begin
            s := v.get(i).toNode.getAttribute('value');
            if s <> '0' then begin
              PageNumber := StrToIntDef(s, 0);
              Break;
            end;
          end;
        finally
          Free;
        end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    if GET(AppendURLDelim(FillHost(Module.RootURL, AURL)) +
      IncStr(DownloadThread.WorkId) + '.html') then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[DownloadThread.WorkId] :=
            XPathString('//div[@class="read_img"]//img[@id="image"]/@src');
        finally
          Free;
        end;
    end;
  end;
end;

function AfterImageSaved(const AFilename: String; const Module: TModuleContainer): Boolean;
begin
  Result := True;
  if removewatermark then
    Result := MangaFoxWatermark.RemoveWatermark(AFilename, saveaspng);
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaFox';
    RootURL := 'http://mangafox.me';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
    OnAfterImageSaved := @AfterImageSaved;
    AddOptionCheckBox(@removewatermark, 'RemoveWatermark', @RS_RemoveWatermark);
    AddOptionCheckBox(@saveaspng, 'SaveAsPNG', @RS_SaveAsPNG);
  end;
end;

initialization
  RegisterModule;

end.
