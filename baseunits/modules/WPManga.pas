unit WPManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, synacode, RegExpr;

implementation

function GetDirURL(const AModule: TModuleContainer): String;
begin
  with AModule do
    if StringIn(Website, ['MangaSpy', 'MangaIce']) then
      Result := 'manga_list'
    else
    if Website = 'ReadHentaiManga' then
      Result := 'hentai-manga-list'
    else if Website = 'HentaiRead' then
      Result := 'hentai-list'
    else
      Result := 'manga-list';
  Result := '/' + Result + '/all/any/last-added/';
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer; const WorkPtr: Integer;
  const Module: TModuleContainer): Integer;
begin
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + GetDirURL(Module)) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(ReplaceRegExpr('^.*\/(\d+)/.*$',
          XPathString('//ul[@class="pgg"]/li[last()]/a/@href'), '$1', True), 1);
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + GetDirURL(Module) + IncStr(AURL) + '/') then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        if StringIn(Module.Website, ['MangaSpy', 'MangaIce']) then
          XPathHREFAll('//*[contains(@id,"content")]//*[@class="det"]/a', ALinks, ANames)
        else
          XPathHREFtitleAll('//*[contains(@id,"content")]//a[./img]', ALinks, ANames);
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
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//img[starts-with(@class,"cvr")]/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          title := XPathString('//*[@itemprop="itemreviewed"]');
          authors := XPathString('//*[contains(@class,"mng_det")]//*[self::p or self::li][starts-with(.,"Author")]/substring-after(.," ")');
          artists := XPathString('//*[contains(@class,"mng_det")]//*[self::p or self::li][starts-with(.,"Artist")]/substring-after(.," ")');
          status := MangaInfoStatusIfPos(XPathString('//*[contains(@class,"mng_det")]//*[self::p or self::li][starts-with(.,"Status")]/substring-after(.," ")'));
          if StringIn(Module.Website, ['ReadHentaiManga', 'HentaiRead']) then
            genres := XPathString('string-join(//*[contains(@class,"mng_det")]//*[self::p or self::li]//a,", ")')
          else
            genres := XPathString('//*[contains(@class,"mng_det")]//*[self::p or self::li][starts-with(.,"Category")]/string-join((./*[position()>1]),", ")');
          if Module.Website = 'HentaiRead' then
          begin
            chapterLinks.Add(XPathString('//a[@class="lst"]/@href'));
            chapterName.Add(title);
          end
          else
          begin
            while True do
            begin
              for v in XPath('//a[@class="lst"]') do
              begin
                chapterLinks.Add(v.toNode.getAttribute('href'));
                s := v.toNode.getAttribute('title');
                if s = '' then
                  s := XPathString('*[@class="val"]', v.toNode);
                if s = '' then
                  s := XPathString('text()[1]', v.toNode);
                chapterName.Add(s);
              end;
              if Thread.IsTerminated then Break;
              s := Trim(XPathString('//*[@class="pgg"]//*[./a[@class="sel"]]/following-sibling::*[./a]/a/@href'));
              if s = '' then Break;
              if GET(MaybeFillHost(Module.RootURL, s)) then
                ParseHTML(Document)
              else
                Break;
            end;
            InvertStrings([chapterLinks, chapterName]);
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
  s: String;
  v, x: IXQValue;
  allnum: Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    Cookies.Values['viewer'] := '1';
    if GET(AppendURLDelim(FillHost(Module.RootURL, AURL)) + '1') then begin
      Result := True;
      //multi page
      with TXQueryEngineHTML.Create(Document) do
        try
          s := XPathString('//script[contains(.,"imglist")]/substring-after(substring-before(.,"]"),"[")');
          if s <> '' then
            s := '[' + s + ']'
          else
          begin
            s := XPathString('//script[contains(.,"img_lst")]/substring-after(substring-before(.,"'')"),"(''")');
            if s <> '' then
              s := DecodeURL(s);
          end;
          ParseHTML(s);
          XPathStringAll('json(*)()("url")', PageLinks);
          if PageLinks.Count = 0 then
            XPathStringAll('json(*)()', PageLinks);
        finally
          Free;
        end;
      //single page
      if PageLinks.Count = 0 then
        with TXQueryEngineHTML.Create(Document) do
          try
            PageNumber := XPath('(//select[@class="cbo_wpm_pag"])[1]/option').Count;
            if PageNumber = 0 then
              PageNumber := XPath('(//select[@name="page"])[1]/option').Count;
            if PageNumber = 0 then
              for v in XPath('//select') do
              begin
                allnum := True;
                for x in XPath('option', v.toNode) do
                begin
                  if StrToIntDef(x.toString, -1) = -1 then
                  begin
                    allnum := False;
                    Break;
                  end;
                end;
                if allnum then
                begin
                  PageNumber := XPath('option', v.toNode).Count;
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
var
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do
    if GET(AppendURLDelim(FillHost(Module.RootURL, AURL)) + IncStr(DownloadThread.WorkId) + '/') then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          if Module.Website = 'ReadHentaiManga' then
            s := HTMLDecode(XPathString('//img[@id="main_img"]/@src'))
          else
            s := XPathString('//*[contains(@class,"mng_rdr")]//img/@src');
          if s = '' then
            s := XPathString('//*[@id="reader"]//img[@id="picture"]/@src');
          PageLinks[DownloadThread.WorkId] := s;
        finally
          Free;
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
      SortedList := True;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnGetImageURL := @GetImageURL;
    end;
  end;

begin
  AddWebsiteModule('MangaBoom', 'http://www.mangaboom.com');
  AddWebsiteModule('Authrone', 'http://www.authrone.com');
  AddWebsiteModule('EyeOnManga', 'http://www.eyeonmanga.com');
  AddWebsiteModule('ReadHentaiManga', 'http://readhentaimanga.com');
  AddWebsiteModule('MangaSpy', 'http://www.mangaspy.com');
  AddWebsiteModule('MangaIce', 'http://www.mangaice.com');
  AddWebsiteModule('MangaCow', 'http://mngcow.co');
  AddWebsiteModule('HentaiRead', 'http://hentairead.com');
end;

initialization
  RegisterModule;

end.
