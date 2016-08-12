unit WPManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, RegExpr;

implementation

const
  dirURL = '/manga-list/all/any/last-added/';
  dirURLreadhentaimanga = '/hentai-manga-list/all/any/last-added/';
  dirURLmangahen = '/manga_list/all/any/last-added/';

function GetDirURL(const AWebsite: String): String;
begin
  if AWebsite = 'ReadHentaiManga' then
    Result := dirURLreadhentaimanga
  else
  if AWebsite = 'MangaHen' then
    Result := dirURLmangahen
  else
    Result := dirURL;
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer;
  const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  with MangaInfo.FHTTP do begin
    if Module.Website = 'Manga-Joy' then
      AllowServerErrorResponse := True;
    if GET(Module.RootURL + GetDirURL(Module.Website)) then begin
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
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  with MangaInfo.FHTTP do begin
    if Module.Website = 'Manga-Joy' then
      AllowServerErrorResponse := True;
    if GET(Module.RootURL + GetDirURL(Module.Website) + IncStr(AURL) + '/') then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          if Module.Website = 'ReadHentaiManga' then
            for v in XPath('//*[@id="content"]//*[@id="center"]/a') do begin
              ALinks.Add(v.toNode.getAttribute('href'));
              ANames.Add(v.toNode.getAttribute('title'));
            end
          else if (Module.Website = 'EyeOnManga') or
            (Module.Website = 'MangaBoom') then
            for v in XPath('//*[@id="sct_content"]//h2/a[1]') do begin
              ALinks.Add(v.toNode.getAttribute('href'));
              ANames.Add(v.toString);
            end
          else if (Module.Website = 'MangaJoy') then
            for v in XPath('//*[@id="sct_manga_list"]//*[@class="det"]/h2/a[1]') do begin
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
  var
    t: String;
  begin
    with MangaInfo.mangaInfo, query, TRegExpr.Create do
      for v in XPath('//ul[contains(@class,"lst")]/li/a') do
      begin
        chapterLinks.Add(v.toNode.getAttribute('href'));
        t := v.toNode.getAttribute('title');
        if t = '' then
          t := XPathString('*[@class="val"]', v.toNode);
        if t = '' then
          t := XPathString('text()[1]', v.toNode);
        chapterName.Add(t);
      end;
  end;

  function getwpmangavalue(aname: String): String;
  begin
    Result := query.XPathString('//*[contains(@class,"mng_det")]//p[starts-with(.,"' +
      aname + '")]');
    if Result <> '' then Result := Trim(TrimChar(SeparateRight(Result, aname), [':', ' ']));
    if Result = '-' then
      Result := '';
  end;

  procedure scaninfo;
  begin
    with MangaInfo.mangaInfo, query do begin
      authors := getwpmangavalue('Author');
      artists := getwpmangavalue('Artist');
      genres := '';
      AddCommaString(genres, getwpmangavalue('Category'));
      AddCommaString(genres, getwpmangavalue('Genres'));
      status := MangaInfoStatusIfPos(getwpmangavalue('Status'), 'Ongoing', 'Completed');
      summary := getwpmangavalue('Summary');
      if summary = '' then
      begin
        summary := XPathString('//*[contains(@class,"mng_det")]//p');
        if (summary <> '') and
          ((Pos('name:', LowerCase(summary)) <> 0) or
          (Pos('alternative name', LowerCase(summary)) = 1)) then
          summary := '';
      end;
      scanchapters;
    end;
  end;

begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    website := Module.Website;
    url := AppendURLDelim(FillHost(Module.RootURL, AURL));
    if Module.Website = 'Manga-Joy' then
      AllowServerErrorResponse := True;
    if GET(MangaInfo.mangaInfo.url) then begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
        coverLink := query.XPathString('//img[starts-with(@class,"cvr")]/@src');
        if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
        title := query.XPathString('//*[@itemprop="itemreviewed"]');
        scaninfo;
        s := query.XPathString('//*[@class="pgg"]/li[last()]/a/@href');
        if s <> '' then begin
          s := ReplaceRegExpr('^.*\/(\d+)/.*$', s, '$1', True);
          pagecount := StrToIntDef(s, 1);
          if pagecount > 1 then
            for i := 2 to pagecount do
            begin
              if MangaInfo.FHTTP.Thread.IsTerminated then Break;
              if GET(url + 'chapter-list/' + IntToStr(i) + '/') then begin
                query.ParseHTML(StreamToString(Document));
                scanchapters;
              end;
            end;
        end;
        if Module.Website <> 'EyeOnManga' then
          InvertStrings([chapterLinks, chapterName]);
        { add missing chapter number }
        //if (Module.Website = 'EyeOnManga') and
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
  AddWebsiteModule('ReadHentaiManga', 'http://readhentaimanga.com');
  AddWebsiteModule('MangaHen', 'http://www.mangahen.com');
  AddWebsiteModule('MangaIce', 'http://www.mangaice.com');
  AddWebsiteModule('MangaJoy', 'http://manga-joy.com');
end;

initialization
  RegisterModule;

end.
