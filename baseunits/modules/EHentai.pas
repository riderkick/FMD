unit EHentai;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  accountmanagerdb, synacode;

implementation

uses RegExpr;

const
  dirURL = 'f_doujinshi=on&f_manga=on&f_western=on&f_apply=Apply+Filter';
  exhentaiurllogin = 'https://forums.e-hentai.org/index.php?act=Login&CODE=01';

var
  onlogin: Boolean = False;
  locklogin: TRTLCriticalSection;

function ExHentaiLogin(var AHTTP: THTTPSendThread): Boolean;
var
  s: String;
const
  acc = 'ExHentai';
begin
  Result := False;
  if AHTTP = nil then Exit;
  if Account.Enabled[acc] = False then Exit;
  if Account.Username[acc] = '' then Exit;

  if TryEnterCriticalsection(locklogin) > 0 then
    with AHTTP do begin
      onlogin := True;
      Account.Status[acc] := asChecking;
      Reset;
      Cookies.Clear;
      s := 'returntype=8&CookieDate=1&b=d&bt=pone'+
           '&UserName=' + EncodeURLElement(Account.Username[acc]) +
           '&PassWord=' + EncodeURLElement(Account.Password[acc]) +
           '&ipb_login_submit=Login%21';
      if POST(exhentaiurllogin, s) then begin
        if ResultCode = 200 then begin
          Result := Cookies.Values['ipb_pass_hash'] <> '';
          if Result then begin
            Account.Cookies[acc] := GetCookies;
            Account.Status[acc] := asValid;
          end
          else Account.Status[acc] := asInvalid;
          Account.Save;
        end;
      end;
      onlogin := False;
      if Account.Status[acc] = asChecking then
        Account.Status[acc] := asUnknown;
      LeaveCriticalsection(locklogin);
    end
  else
  begin
    while onlogin do Sleep(1000);
    if Result then AHTTP.Cookies.Text := Account.Cookies[acc];
  end;
end;

function GETWithLogin(var AHTTP: THTTPSendThread; const AURL, AWebsite: String): Boolean;
begin
  Result := False;
  if AWebsite = 'ExHentai' then begin
    AHTTP.FollowRedirection := False;
    AHTTP.Cookies.Text := Account.Cookies[AWebsite];
    Result := AHTTP.GET(AURL);
    if Result and (AHTTP.ResultCode > 300) then begin
      Result := ExHentaiLogin(AHTTP);
      if Result then
        Result := AHTTP.GET(AURL);
    end;
  end
  else
    Result := AHTTP.GET(AURL);
end;

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if GETWithLogin(MangaInfo.FHTTP, Module.RootURL + '/?' + dirURL, Module.Website) then begin
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
  if GETWithLogin(MangaInfo.FHTTP, rurl, Module.Website) then begin
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
      getOK := GETWithLogin(MangaInfo.FHTTP, MangaInfo.mangaInfo.url + '?nw=session', Module.Website);
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
  with MangaInfo.mangaInfo do begin
    website := Module.Website;
    url := FillHost(Module.RootURL, AURL);
    if GETWithLogin(MangaInfo.FHTTP, url, Module.Website) then begin
      Result := NO_ERROR;
      // if there is only 1 line, it's banned message!
      //if Source.Count = 1 then
      //  info.summary := Source.Text
      //else
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
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
      getOK := GETWithLogin(DownloadThread.FHTTP, rurl + '?nw=session', Module.Website);
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
          if GETWithLogin(DownloadThread.FHTTP, rurl + '?p' + IntToStr(i), Module.Website) then
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
    if GETWithLogin(DownloadThread.FHTTP, rurl, Module.Website) then begin
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
            if not GETWithLogin(DownloadThread.FHTTP, iurl, Module.Website) then Break;
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
  if GETWithLogin(DownloadThread.FHTTP, iurl, Module.Website) then begin
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
  with AddModule do
  begin
    Website := 'ExHentai';
    RootURL := 'http://exhentai.org';
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
    AccountSupport := True;
    OnLogin := @ExHentaiLogin;
  end;
end;

initialization
  InitCriticalSection(locklogin);
  RegisterModule;

finalization
  DoneCriticalsection(locklogin);

end.
