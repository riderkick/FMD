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

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  s: String;
begin
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  Source := TStringList.Create;
  try
    if GetPage(MangaInfo.FHTTP, TObject(Source), Module.RootURL + dirURL, 3) then
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          s := Query.XPathString('//*[@class="pgg"]/li[last()]/a/@href');
          s := ReplaceRegExpr('^.*\/(\d+)/.*$', s, '$1', True);
          Page := StrToIntDef(s, 1);
        finally
          Query.Free;
        end;
      end
      else
        Result := INFORMATION_NOT_FOUND;
  finally
    Source.Free;
  end;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String; Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  v: IXQValue;
  i: Integer;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  Source := TStringList.Create;
  try
    if GetPage(MangaInfo.FHTTP, TObject(Source), Module.RootURL + dirURL +
      IncStr(URL) + '/', 3) then
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          if (Module.Website = 'EyeOnManga') or
            (Module.Website = 'MangaBoom') then
            v := Query.XPath('//*[@id="sct_content"]//h2/a[1]')
          else
            v := Query.XPath('//*[@id="sct_content"]//div[@class="det"]/a[1]');
          if v.Count > 0 then
            for i := 0 to v.Count - 1 do begin
              Links.Add(v.get(i).toNode.getAttribute('href'));
              Names.Add(v.get(i).toString);
            end;
        finally
          Query.Free;
        end;
      end
      else
        Result := INFORMATION_NOT_FOUND;
  finally
    Source.Free;
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  info: TMangaInfo;
  v: IXQValue;
  s: String;
  i, pagecount: Integer;

  procedure scanchapters;
  begin
    if Module.Website = 'MangaBoom' then begin
      for v in Query.XPath('//ul[@class="lst"]//a[1]') do begin
        info.chapterLinks.Add(v.toNode.getAttribute('href'));
        info.chapterName.Add(v.toNode.Next.toString());
      end;
    end
    else if module.Website = 'EyeOnManga' then begin
      for v in Query.XPath('//ul[@class="chp_lst"]//a[1]') do begin
        info.chapterLinks.Add(v.toNode.getAttribute('href'));
        info.chapterName.Add(v.toString);
      end;
    end
    else begin
      for v in Query.XPath('//ul[@class="lst"]//a[1]/@href') do
        info.chapterLinks.Add(v.toString);
      for v in Query.XPath('//ul[@class="lst"]//a[1]/b[1]') do
        info.chapterName.Add(v.toString);
    end;
  end;

  procedure scaninfo;
  begin
    with info, Query do begin
      coverLink := XPathString('//img[starts-with(@class,"cvr")]/@src');
      title := Query.XPathString('//*[@itemprop="itemreviewed"]');
      if Module.Website = 'EyeOnManga' then
        summary := XPathString('//*[@class="wpm_pag mng_det"]/p[1]')
      else
        summary := XPathString('//*[@class="det"]/p[1]');
      authors := XPathString('(//*[@class="mng_ifo"]//p)[contains(.,"Author")]');
      artists := XPathString('(//*[@class="mng_ifo"]//p)[contains(.,"Artist")]');
      genres := XPathString('(//*[@class="mng_ifo"]//p)[contains(.,"Category")]');
      status := XPathString('(//*[@class="mng_ifo"]//p)[contains(.,"Status")]');
      if summary <> '' then summary :=
          TrimChar(SeparateRight(summary, 'Summary'), [':', ' ']);
      if authors <> '' then authors :=
          TrimChar(SeparateRight(authors, 'Author'), [':', ' ']);
      if artists <> '' then artists :=
          TrimChar(SeparateRight(artists, 'Artist'), [':', ' ']);
      if genres <> '' then genres :=
          TrimChar(SeparateRight(genres, 'Category'), [':', ' ']);
      if status <> '' then begin
        status := LowerCase(status);
        if Pos('ongoing', status) > 0 then status := '1'
        else if Pos('completed', status) > 0 then status := '0'
        else status := '';
      end;
      scanchapters;
    end;
  end;

begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  info := MangaInfo.mangaInfo;
  info.website := Module.Website;
  info.url := AppendURLDelim(FillHost(Module.RootURL, URL));
  Source := TStringList.Create;
  try
    if GetPage(MangaInfo.FHTTP, TObject(Source), info.url, Reconnect) then
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          scaninfo;
          s := Query.XPathString('//*[@class="pgg"]/li[last()]/a/@href');
          if s <> '' then begin
            s := ReplaceRegExpr('^.*\/(\d+)/.*$', s, '$1', True);
            pagecount := StrToIntDef(s, 1);
            if pagecount > 1 then
              for i := 2 to pagecount do
                if GetPage(MangaInfo.FHTTP, TObject(Source),
                  info.url + 'chapter-list/' + IntToStr(i) + '/',
                  Reconnect) then
                begin
                  Query.ParseTree(Source.Text);
                  scanchapters;
                end;
          end;
          if Module.Website <> 'EyeOnManga' then
            InvertStrings([info.chapterLinks, info.chapterName]);
        finally
          Query.Free;
        end;
      end
      else
        Result := INFORMATION_NOT_FOUND;
  finally
    Source.Free;
  end;
end;

function GetPageNumber(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  Container: TTaskContainer;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  with Container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    Source := TStringList.Create;
    try
      if GetPage(DownloadThread.FHTTP, TObject(Source),
        AppendURLDelim(FillHost(Module.RootURL, URL)) + '1',
        Manager.retryConnect) then
        if Source.Count > 0 then
        begin
          Result := True;
          Query := TXQueryEngineHTML.Create(Source.Text);
          try
            PageNumber :=
              Query.XPath('(//select[@class="cbo_wpm_pag"])[1]/option').Count;
          finally
            Query.Free;
          end;
        end;
    finally
      Source.Free;
    end;
  end;
end;

function GetImageURL(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  Container: TTaskContainer;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  with Container do begin
    Source := TStringList.Create;
    try
      if DownloadThread.GetPage(TObject(Source),
        AppendURLDelim(FillHost(Module.RootURL, URL)) +
        IncStr(DownloadThread.workCounter) + '/',
        Manager.retryConnect) then
      begin
        ParseHTML(Source.Text, Source);
        if Source.Count > 0 then
        begin
          Result := True;
          Query := TXQueryEngineHTML.Create(Source.Text);
          try
            Container.PageLinks[DownloadThread.WorkCounter] :=
              Query.XPathString('//*[@class="wpm_pag mng_rdr"]//img/@src');
          finally
            Query.Free;
          end;
        end;
      end;
    finally
      Source.Free;
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
end;

initialization
  RegisterModule;

end.
