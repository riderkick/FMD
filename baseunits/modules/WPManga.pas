unit WPManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager;

implementation

uses
  RegExpr, synautil;

const
  dirURL = '/manga-list/all/any/last-added/';
  dirURLmangaindo = '/daftar-manga/all/any/last-added/';
  dirURLreadhentaimanga = '/hentai-manga-list/all/any/last-added/';

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  s: String;
begin
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  with MangaInfo.FHTTP do begin
    if Module.Website = 'MangaIndo' then s := dirURLmangaindo
    else if Module.Website = 'ReadHentaiManga' then s := dirURLreadhentaimanga
    else s := dirURL;
    if GET(Module.RootURL + s) then begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        s := Query.XPathString('//*[@class="pgg"]/li[last()]/a/@href');
        s := ReplaceRegExpr('^.*\/(\d+)/.*$', s, '$1', True);
        Page := StrToIntDef(s, 1);
      finally
        query.Free;
      end;
    end;
  end;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const AURL: String; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  s: String;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  with MangaInfo.FHTTP do begin
    if Module.Website = 'MangaIndo' then s := dirURLmangaindo
    else if Module.Website = 'ReadHentaiManga' then s := dirURLreadhentaimanga
    else s := dirURL;
    if GET(Module.RootURL + s + IncStr(AURL) + '/') then begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        if Module.Website = 'MangaIndo' then begin
          for v in Query.XPath('//*[@id="sct_content"]//div[@class="node"]/a[1]/@href') do
            Links.Add(v.toString);
          for v in Query.XPath('//*[@id="sct_content"]//div[@class="node"]/div[1]') do
            Names.Add(v.toString);
        end
        else if Module.Website = 'ReadHentaiManga' then begin
          for v in Query.XPath('//*[@id="content"]//*[@id="center"]/a') do begin
            Links.Add(v.toNode.getAttribute('href'));
            Names.Add(v.toNode.getAttribute('title'));
          end;
        end
        else begin
          if (Module.Website = 'EyeOnManga') or
            (Module.Website = 'MangaBoom') then
            s := '//*[@id="sct_content"]//h2/a[1]'
          else
            s := '//*[@id="sct_content"]//div[@class="det"]/a[1]';
          for v in Query.XPath(s) do begin
            Links.Add(v.toNode.getAttribute('href'));
            Names.Add(v.toString);
          end;
        end;
      finally
        query.Free;
      end;
    end;
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const AURL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  s: String;
  i, pagecount: Integer;

  procedure scanchapters;
  begin
    with MangaInfo.mangaInfo, query do begin
      if Module.Website = 'MangaBoom' then begin
        for v in XPath('//ul[@class="lst"]//a[1]') do begin
          chapterLinks.Add(v.toNode.getAttribute('href'));
          chapterName.Add(v.toNode.Next.toString());
        end;
      end
      else if (Module.Website = 'EyeOnManga') or
        (Module.Website = 'MangaIndo') then
      begin
        for v in XPath('//ul[@class="chp_lst"]//a[1]') do begin
          chapterLinks.Add(v.toNode.getAttribute('href'));
          chapterName.Add(v.toString);
        end;
      end
      else begin
        for v in XPath('//ul[@class="lst"]//a[1]/@href') do
          chapterLinks.Add(v.toString);
        for v in XPath('//ul[@class="lst"]//a[1]/b[1]') do
          chapterName.Add(v.toString);
      end;
    end;
  end;

  function getwpmangavalue(aname: String): String;
  begin
    Result := query.XPathString('(//*[@class="mng_ifo"]//p)[contains(.,"' +
      aname + '")]');
    if Result <> '' then Result := TrimChar(SeparateRight(Result, aname), [':', ' ']);
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

function GetPageNumber(var DownloadThread: TDownloadThread; const AURL: String;
  Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.manager.container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    if GET(AppendURLDelim(FillHost(Module.RootURL, AURL)) + '1') then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        PageNumber := query.XPath('(//select[@class="cbo_wpm_pag"])[1]/option').Count;
      finally
        query.Free;
      end;
    end;
  end;
end;

function GetImageURL(var DownloadThread: TDownloadThread; const AURL: String;
  Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.manager.container do begin
    if GET(AppendURLDelim(FillHost(Module.RootURL, AURL)) + IncStr(DownloadThread.workCounter) + '/') then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        if Module.Website = 'ReadHentaiManga' then
          s := HTMLDecode(query.XPathString('//img[@id="main_img"]/@src'))
        else
          s := query.XPathString('//*[@class="wpm_pag mng_rdr"]//img/@src');
        PageLinks[DownloadThread.WorkCounter] := s;
      finally
        query.Free;
      end;
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
  AddWebsiteModule('MangaCap', 'http://www.mangacap.com');
  AddWebsiteModule('MangaBoom', 'http://www.mangaboom.com');
  AddWebsiteModule('Authrone', 'http://www.authrone.com');
  AddWebsiteModule('EyeOnManga', 'http://www.eyeonmanga.com');
  AddWebsiteModule('MangaIndo', 'http://mangaindo.id');
  AddWebsiteModule('ReadHentaiManga', 'http://readhentaimanga.com');
end;

initialization
  RegisterModule;

end.
