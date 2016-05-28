unit WPManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, RegExpr;

implementation

const
  dirURL = '/manga-list/all/any/last-added/';
  dirURLmangaindo = '/daftar-manga/all/any/last-added/';
  dirURLreadhentaimanga = '/hentai-manga-list/all/any/last-added/';
  dirURLmangahen = '/manga_list/all/any/last-added/';
  dirURLsenmanga = '/directory/all/any/last-added/';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer;
  const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  with MangaInfo.FHTTP do begin
    if Module.Website = 'MangaIndo' then s := dirURLmangaindo
    else if Module.Website = 'ReadHentaiManga' then s := dirURLreadhentaimanga
    else if Module.Website = 'MangaHen' then s := dirURLmangahen
    else if Module.Website = 'SenManga' then s := dirURLsenmanga
    else s := dirURL;
    if GET(Module.RootURL + s) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          s := XPathString('//*[@class="pgg"]/li[last()]/a/@href');
          s := ReplaceRegExpr('^.*\/(\d+)/.*$', s, '$1', True);
          Page := StrToIntDef(s, 1);
        finally
          Free;
        end;
    end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  with MangaInfo.FHTTP do begin
    if Module.Website = 'MangaIndo' then s := dirURLmangaindo
    else if Module.Website = 'ReadHentaiManga' then s := dirURLreadhentaimanga
    else if Module.Website = 'MangaHen' then s := dirURLmangahen
    else if Module.Website = 'SenManga' then s := dirURLsenmanga
    else s := dirURL;
    if GET(Module.RootURL + s + IncStr(AURL) + '/') then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          if Module.Website = 'MangaIndo' then begin
            for v in XPath('//*[@id="sct_content"]//div[@class="node"]/a[1]/@href') do
              ALinks.Add(v.toString);
            for v in XPath('//*[@id="sct_content"]//div[@class="node"]/div[1]') do
              ANames.Add(v.toString);
          end
          else if Module.Website = 'ReadHentaiManga' then
            for v in XPath('//*[@id="content"]//*[@id="center"]/a') do begin
              ALinks.Add(v.toNode.getAttribute('href'));
              ANames.Add(v.toNode.getAttribute('title'));
            end
          else if Module.Website = 'SenManga' then
            for v in XPath('//table[@id="wpm_mng_lst"]/tbody/tr/td/a[1]') do begin
              ALinks.Add(v.toNode.getAttribute('href'));
              ANames.Add(v.toNode.getAttribute('title'));
            end
          else if (Module.Website = 'EyeOnManga') or
            (Module.Website = 'MangaBoom') then
            for v in XPath('//*[@id="sct_content"]//h2/a[1]') do begin
              ALinks.Add(v.toNode.getAttribute('href'));
              ANames.Add(v.toString);
            end
          else
            for v in XPath('//*[@id="sct_content"]//div[@class="det"]/a[1]') do begin
              ALinks.Add(v.toNode.getAttribute('href'));
              ANames.Add(v.toString);
            end;
        finally
          Free;
        end;
    end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  s: String;
  i, pagecount: Integer;

  procedure scanchapters;
  begin
    with MangaInfo.mangaInfo, query do if Module.Website = 'MangaBoom' then
        for v in XPath('//ul[@class="lst"]//a[1]') do begin
          chapterLinks.Add(v.toNode.getAttribute('href'));
          chapterName.Add(v.toNode.Next.toString());
        end
      else if (Module.Website = 'EyeOnManga') or
        (Module.Website = 'MangaIndo') then
        for v in XPath('//ul[@class="chp_lst"]//a[1]') do begin
          chapterLinks.Add(v.toNode.getAttribute('href'));
          chapterName.Add(v.toString);
        end
      else begin
        for v in XPath('//ul[@class="lst"]//a[1]/@href') do
          chapterLinks.Add(v.toString);
        for v in XPath('//ul[@class="lst"]//a[1]/b[1]') do
          chapterName.Add(v.toString);
      end;
  end;

  function getwpmangavalue(aname: String): String;
  begin
    Result := query.XPathString('(//*[@class="mng_ifo"]//p)[starts-with(.,"' +
      aname + '")]');
    if Result <> '' then Result := Trim(TrimChar(SeparateRight(Result, aname), [':', ' ']));
    if Result = '-' then
      Result := '';
  end;

  procedure scaninfo;
  begin
    with MangaInfo.mangaInfo, query do begin
      if Module.Website = 'EyeOnManga' then
        summary := XPathString('//*[@class="wpm_pag mng_det"]/p[1]')
      else
        summary := XPathString('//*[@class="det"]/p[1]');
      if summary <> '' then
        summary := TrimChar(SeparateRight(summary, 'Summary'), [':', ' ']);
      authors := getwpmangavalue('Author');
      artists := getwpmangavalue('Artist');
      genres := getwpmangavalue('Category');
      status := getwpmangavalue('Status');
      if status <> '' then begin
        status := LowerCase(status);
        if Pos('ongoing', status) > 0 then status := '1'
        else if Pos('completed', status) > 0 then status := '0'
        else status := '';
      end;
      scanchapters;
    end;
  end;

  procedure scaninfomangaindo;
  begin
    with MangaInfo.mangaInfo, query do begin
      summary := XPathString('//*[@class="wpm_pag mng_det"]/p[1]');
      authors := getwpmangavalue('Penulis');
      artists := getwpmangavalue('Seniman');
      genres := getwpmangavalue('Kategori');
      status := getwpmangavalue('Status');
      if status <> '' then begin
        status := LowerCase(status);
        if Pos('berjalan', status) > 0 then status := '1'
        else if Pos('tamat', status) > 0 then status := '0'
        else status := '';
      end;
    end;
    scanchapters;
  end;

begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    website := Module.Website;
    url := AppendURLDelim(FillHost(Module.RootURL, AURL));
    if GET(MangaInfo.mangaInfo.url) then begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
        coverLink := query.XPathString('//img[starts-with(@class,"cvr")]/@src');
        if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
        title := query.XPathString('//*[@itemprop="itemreviewed"]');
        if Module.Website = 'MangaIndo' then scaninfomangaindo
        else scaninfo;
        s := query.XPathString('//*[@class="pgg"]/li[last()]/a/@href');
        if s <> '' then begin
          s := ReplaceRegExpr('^.*\/(\d+)/.*$', s, '$1', True);
          pagecount := StrToIntDef(s, 1);
          if pagecount > 1 then
            for i := 2 to pagecount do
              if GET(url + 'chapter-list/' + IntToStr(i) + '/') then begin
                query.ParseHTML(StreamToString(Document));
                scanchapters;
              end;
        end;
        if Module.Website <> 'EyeOnManga' then
          InvertStrings([chapterLinks, chapterName]);
        { add missing chapter number }
        //if (Module.Website = 'EyeOnManga') or
        //  (Module.Website = 'MangaIndo') and
        //  (chapterName.Count > 0) then
        //  for i := 0 to chapterName.Count - 1 do
        //    chapterName[i] := 'Ch.' + IncStr(i) + ' ' + chapterName[i];
      finally
        query.Free;
      end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  s: String;
  v: IXQValue;
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
      s := StreamToString(Document);
      if Pos('var imglist = ', s) > 0 then
      begin
        s := GetBetween('var imglist = ', '];', s);
        if s <> '' then
        begin
          s := s + ']';
          s := RemoveBreaks(s);
          with TXQueryEngineHTML.Create(s) do
            try
              for v in XPath('json(*)()("url")') do
                PageLinks.Add(v.toString);
            finally
              Free;
            end;
        end;
      end;
      //single page
      if PageLinks.Count = 0 then
        with TXQueryEngineHTML.Create(Document) do
          try
            PageNumber := XPath('(//select[@class="cbo_wpm_pag"])[1]/option').Count;
            if PageNumber = 0 then
              PageNumber := XPath('(//select[@name="page"])[1]/option').Count;
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
    if GET(AppendURLDelim(FillHost(Module.RootURL, AURL)) + IncStr(WorkCounter) + '/') then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          if Module.Website = 'ReadHentaiManga' then
            s := HTMLDecode(XPathString('//img[@id="main_img"]/@src'))
          else
            s := XPathString('//*[@class="wpm_pag mng_rdr"]//img/@src');
          if s = '' then
            s := XPathString('//*[@id="reader"]//img[@id="picture"]/@src');
          PageLinks[WorkCounter] := s;
        finally
          Free;
        end;
    end;
end;

procedure RegisterModule;

  procedure AddWebsiteModule(AWebsite, ARootURL: String);
  begin
    with AddModule do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      SortedList := True;
      InformationAvailable := True;
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
  AddWebsiteModule('MangaIndo', 'http://mangaindo.id');
  AddWebsiteModule('ReadHentaiManga', 'http://readhentaimanga.com');
  AddWebsiteModule('MangaHen', 'http://www.mangahen.com');
  AddWebsiteModule('MangaBug', 'http://www.mangabug.com');
  AddWebsiteModule('SenManga', 'http://www.senmanga.com');
end;

initialization
  RegisterModule;

end.
