unit EHentai;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager;

implementation

uses RegExpr;

const
  dirURL = 'f_doujinshi=on&f_manga=on&f_western=on&f_apply=Apply+Filter';

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/?' + dirURL) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      Page := StrToIntDef(query.CSSString('table.ptt>tbody>tr>td:nth-last-child(2)>a'), 1) + 1;
    finally
      query.Free;
    end;
  end;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  rurl: String;
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if AURL = '0' then rurl := Module.RootURL + '/?' + dirURL
  else rurl := Module.RootURL + '/?page=' + IncStr(AURL) + '&' + dirURL;
  if MangaInfo.FHTTP.GET(rurl) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//table[@class="itg"]/tbody/tr/td/div/div/a') do
      begin
        ANames.Add(v.toString);
        ALinks.Add(v.toNode.getAttribute('href'));
      end;
    finally
      query.Free;
    end;
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const AURL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;

  procedure ScanParse;
  var
    getOK: Boolean;
  begin
    getOK := True;
    // check content warning
    if Pos('Content Warning', query.XPathString('//div/h1')) > 0 then
    begin
      getOK := MangaInfo.FHTTP.GET(MangaInfo.mangaInfo.url + '?nw=session');
      if getOK then
        query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
    end;
    if getOK then
    begin
      with MangaInfo.mangaInfo do begin
        //title
        title := Query.XPathString('//*[@id="gn"]');
        //cover
        coverLink := Query.XPathString('//*[@id="gd1"]/img/@src');
        //artists
        artists := '';
        for v in Query.XPath('//a[starts-with(@id,"ta_artist")]') do
          AddCommaString(artists, v.toString);
        //genres
        genres := '';
        for v in Query.XPath(
            '//a[starts-with(@id,"ta_")and(not(starts-with(@id,"ta_artist")))]') do
          AddCommaString(genres, v.toString);
        //chapter
        if title <> '' then begin
          chapterLinks.Add(url);
          chapterName.Add(title);
        end;
        //status
        with TRegExpr.Create do
          try
            Expression := '(?i)[\[\(\{](wip|ongoing)[\]\)\}]';
            if Exec(title) then
              status := '1'
            else
            begin
              Expression := '(?i)[\[\(\{]completed[\]\)\}]';
              if Exec(title) then
                status := '0';
            end;
          finally
            Free;
          end;
      end;
    end;
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    website := Module.Website;
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then
    begin
      Result := NO_ERROR;
      // if there is only 1 line, it's banned message!
      //if Source.Count = 1 then
      //  info.summary := Source.Text
      //else
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        ScanParse;
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
  v: IXQValue;
  rurl: String;

  procedure GetImageLink;
  begin
    for v in query.XPath('//div[@id="gdt"]//a/@href') do
      with DownloadThread.manager.container do begin
        PageLinks.Add('G');
        PageContainerLinks.Add(v.toString);
      end;
  end;

  procedure ScanParse;
  var
    getOK: Boolean;
    i, p: Integer;
    //s: String;
  begin
    getOK := True;
    //check content warning
    if Pos('Content Warning', query.XPathString('//div/h1')) > 0 then
    begin
      getOK := DownloadThread.FHTTP.GET(rurl + '?nw=session');
      if getOK then
        query.ParseHTML(StreamToString(DownloadThread.FHTTP.Document));
    end;
    if getOK then
    begin
      GetImageLink;
      //get page count
      p := StrToIntDef(query.CSSString(
        'table.ptt>tbody>tr>td:nth-last-child(2)>a'), 1) - 1;
      if p > 0 then
        for i := 1 to p do
          if DownloadThread.FHTTP.GET(rurl + '?p=' + IntToStr(i)) then
          begin
            query.ParseHTML(StreamToString(DownloadThread.FHTTP.Document));
            GetImageLink;
          end;
    end;
  end;

begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.manager.container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    rurl := AppendURLDelim(FillHost(Module.RootURL, AURL));
    if GET(rurl) then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        ScanParse;
      finally
        query.Free;
      end;
    end;
  end;
end;

function DownloadImage(var DownloadThread: TDownloadThread;
  const AURL, APath, AName, APrefix: String; Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
  iurl: String;
  reconnect: Integer;

  function DoDownloadImage: Boolean;
  var
    i, rcount: Integer;
    base_url, startkey, gid, startpage, nl: String;
    source: TStringList;
  begin
    rcount := 0;
    Result := False;
    source := TStringList.Create;
    try
      while (not Result) and (not DownloadThread.IsTerminated) do begin
        source.LoadFromStream(DownloadThread.FHTTP.Document);
        query.ParseHTML(source.Text);
        iurl := query.XPathString('//*[@id="img"]/@src');
        if iurl = '' then
          iurl := query.XPathString('//a/img/@src[not(contains(.,"ehgt.org/"))]');
        if iurl <> '' then
          Result := SaveImage(DownloadThread.FHTTP, -1, iurl, APath, AName, APrefix);
        if DownloadThread.IsTerminated then Break;
        if rcount >= reconnect then Break;
        if not Result then begin
          base_url := '';
          startkey := '';
          gid := '';
          startpage := '';
          nl := '';
          //get AURL param
          if source.Count > 0 then
            for i := 0 to source.Count - 1 do begin
              if Pos('var base_url=', source[i]) > 0 then
                base_url := RemoveURLDelim(GetValuesFromString(source[i], '='))
              else if Pos('var startkey=', source[i]) > 0 then
                startkey := GetValuesFromString(source[i], '=')
              else if Pos('var gid=', source[i]) > 0 then
                gid := GetValuesFromString(source[i], '=')
              else if Pos('var startpage=', source[i]) > 0 then
                startpage := GetValuesFromString(source[i], '=')
              else if Pos('return nl(', source[i]) > 0 then
                nl := ReplaceRegExpr('(?i)^.*nl\([''"](.*)[''"]\).*$',
                  source[i], '$1', True);
            end;
          if (base_url <> '') and (startkey <> '') and (gid <> '') and
            (startpage <> '') then
            iurl := base_url + '/s/' + startkey + '/' + gid + '-' + startpage
          else iurl := FillHost(Module.RootURL, AURL);
          if nl <> '' then begin
            iurl := iurl + '?nl=' + nl;
            if not DownloadThread.FHTTP.GET(iurl) then Break;
          end else Break;
          if rcount >= reconnect then Break
          else Inc(rcount);
        end;
      end;
    finally
      source.Free;
    end;
  end;

begin
  Result := False;
  if DownloadThread = nil then Exit;
  reconnect := DownloadThread.FHTTP.RetryCount;
  iurl := FillHost(Module.RootURL, AURL);
  if DownloadThread.FHTTP.GET(iurl) then begin
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(DownloadThread.FHTTP.Document));
      Result := DoDownloadImage;
    finally
      query.Free;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'E-Hentai';
    RootURL := 'http://g.e-hentai.org';
    MaxTaskLimit := 1;
    MaxConnectionLimit := 4;
    SortedList := True;
    InformationAvailable := True;
    DynamicPageLink := True;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnDownloadImage := @DownloadImage;
  end;
end;

initialization
  RegisterModule;

end.
