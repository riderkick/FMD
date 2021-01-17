unit EHentai;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, synacode,
  RegExpr, LazFileUtils;

implementation

const
  dirURL = 'f_doujinshi=on&f_manga=on&f_western=on&f_apply=Apply+Filter';
  exhentaiurllogin = 'https://forums.e-hentai.org/index.php?act=Login&CODE=01';
  accname = 'ExHentai';

var
  onlogin: Boolean = False;
  locklogin: TRTLCriticalSection;
  settingsimagesize: Integer = 0;
  settingsimagesizestr: array [0..5] of string = (
    'xr_a',
    'xr_780',
    'xr_980',
    'xr_1280',
    'xr_1600',
    'xr_2400');

resourcestring
  RS_DownloadOriginalImage = 'Download original image(require ExHentai account)';
  RS_SettingsImageSize = 'Image size:';
  RS_SettingsImageSizeItems =
    'Auto' + LineEnding +
    '780x' + LineEnding +
    '980x' + LineEnding +
    '1280x' + LineEnding +
    '1600x' + LineEnding +
    '2400x' + LineEnding +
    'Original';

function ExHentaiLogin(const AHTTP: THTTPSendThread; const Module: TModuleContainer): Boolean;
var
  s: String;
begin
  Result := False;
  if AHTTP = nil then Exit;
  if Module.Account.Enabled = False then Exit;

  if TryEnterCriticalsection(locklogin) > 0 then
    with AHTTP do begin
      onlogin := True;
      Module.Account.Status := asChecking;
      Reset;
      Cookies.Clear;
      s := 'returntype=8&CookieDate=1&b=d&bt=pone' +
        '&UserName=' + EncodeURLElement(Module.Account.Username) +
        '&PassWord=' + EncodeURLElement(Module.Account.Password) +
        '&ipb_login_submit=Login%21';
      if POST(exhentaiurllogin, s) then begin
        if ResultCode = 200 then begin
          Result := Cookies.Values['ipb_pass_hash'] <> '';
          if Result then
            Module.Account.Status := asValid
          else Module.Account.Status := asInvalid;
        end;
      end;
      onlogin := False;
      if Module.Account.Status = asChecking then
        Module.Account.Status := asUnknown;
      LeaveCriticalsection(locklogin);
    end
  else
  begin
    while onlogin do Sleep(1000);
  end;
end;

function GETWithLogin(const AHTTP: THTTPSendThread; const AURL: String; const Module: TModuleContainer): Boolean;
var
  ACookies: String;
begin
  Result := False;
  if Assigned(Module.Account) and Module.Account.Enabled then begin
    AHTTP.FollowRedirection := False;
    // force no warning
    AHTTP.Cookies.Values['nw'] := '1';
    if AHTTP.Cookies.Count > 0 then
      ACookies := AHTTP.Cookies.Text
    else
      ACookies := '';
    Result := AHTTP.GET(AURL);
    if Result and (AHTTP.ResultCode > 300) then begin
      Result := ExHentaiLogin(AHTTP, Module);
      if Result then
      begin
        if ACookies <> '' then AHTTP.Cookies.AddText(ACookies);
        Result := AHTTP.GET(AURL);
      end;
    end;
  end
  else
    Result := AHTTP.GET(AURL);
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if GETWithLogin(MangaInfo.FHTTP, Module.RootURL + '/?' + dirURL, Module) then begin
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

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  rurl: String;
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if AURL = '0' then rurl := Module.RootURL + '/?' + dirURL
  else rurl := Module.RootURL + '/?page=' + IncStr(AURL) + '&' + dirURL;
  if GETWithLogin(MangaInfo.FHTTP, rurl, Module) then begin
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

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;

  procedure ScanParse;
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

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  with MangaInfo.mangaInfo do begin
    website := Module.Website;
    url := ReplaceRegExpr('/\?\w+.*$', AURL, '/', False);
    url := AppendURLDelim(FillHost(Module.RootURL, url));
    if GETWithLogin(MangaInfo.FHTTP, url, Module) then begin
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

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
  rurl: String;
  getOK: Boolean;
  i, p: Integer;

  procedure GetImageLink;
  var
    x: IXQValue;
  begin
    with DownloadThread.Task.Container, query do begin
      ParseHTML(DownloadThread.FHTTP.Document);
      for x in XPath('//div[@id="gdt"]//a') do
      begin
        PageContainerLinks.Add(x.toNode.getAttribute('href'));
        FileNames.Add(ExtractFileNameOnly(Trim(SeparateRight(
          XPathString('img/@title', x.toNode), ':'))));
      end;
      while PageLinks.Count < PageContainerLinks.Count do
        PageLinks.Add('G');
    end;
  end;

begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    Filenames.Clear;
    PageNumber := 0;
    rurl := ReplaceRegExpr('/\?\w+.*$', AURL, '/', False);
    rurl := AppendURLDelim(FillHost(Module.RootURL, rurl));
    if GETWithLogin(DownloadThread.FHTTP, rurl, Module) then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        getOK := True;
        //check content warning
        if Pos('Content Warning', query.XPathString('//div/h1')) > 0 then
          getOK := GETWithLogin(DownloadThread.FHTTP, rurl + '?nw=session', Module);
        if getOK then begin
          GetImageLink;
          //get page count
          p := StrToIntDef(query.XPathString('//table[@class="ptt"]//td[last()-1]'), 0);
          if p > 1 then begin
            Dec(p);
            for i := 1 to p do
            begin
              if DownloadThread.IsTerminated then Break;
              if GETWithLogin(DownloadThread.FHTTP, rurl + '?p=' + IntToStr(i), Module) then
                GetImageLink;
            end;
          end;
          SerializeAndMaintainNames(FileNames);
          // check the max length of filenames, serialize the filenames if it's exceds available space
          p := 0;
          for i := 0 to FileNames.Count - 1 do
          begin
            if Length(FileNames[i]) > p then
              p := Length(FileNames[i]);
          end;
          if p > Task.CurrentMaxFileNameLength then
            FileNames.Clear;
        end;
      finally
        query.Free;
      end;
    end;
  end;
end;

function DownloadImage(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
  iurl: String;
  reconnect: Integer;

  function DoDownloadImage: Boolean;
  var
    rcount: Integer;
    base_url, startkey, gid, startpage, nl, s, nls: String;
  begin
    rcount := 0;
    Result := False;
    nls := '';
    while (not Result) and (not DownloadThread.IsTerminated) do begin
      s := StreamToString(DownloadThread.FHTTP.Document);
      query.ParseHTML(DownloadThread.FHTTP.Document);
      iurl := '';
      if (settingsimagesize > High(settingsimagesizestr)) and (Module.Account.Status = asValid) then
        iurl := query.XPathString('//a/@href[contains(.,"/fullimg.php")]');
      if iurl = '' then
        iurl := query.XPathString('//*[@id="img"]/@src');
      if iurl = '' then
        iurl := query.XPathString('//a/img/@src[not(contains(.,"ehgt.org/"))]');
      if iurl <> '' then
        Result := GETWithLogin(DownloadThread.FHTTP, iurl, Module);
      if DownloadThread.IsTerminated then Break;
      if rcount >= reconnect then Break;
      if not Result then begin
        base_url := '';
        startkey := '';
        gid := '';
        startpage := '';
        nl := '';
        //get AURL param
        if Pos('var base_url', s) > 0 then
        begin
          base_url := GetBetween('var base_url=', ';', s);
          base_url := TrimChar(base_url, ['''', '"']);
        end;
        if Pos('var startkey', s) > 0 then
        begin
          startkey := GetBetween('var startkey=', ';', s);
          startkey := TrimChar(startkey, ['''', '"']);
        end;
        if Pos('var gid', s) > 0 then
        begin
          gid := GetBetween('var gid=', ';', s);
          gid := TrimChar(gid, ['''', '"']);
        end;
        if Pos('var startpage', s) > 0 then
        begin
          startpage := GetBetween('var startpage=', ';', s);
          startpage := TrimChar(startpage, ['''', '"']);
        end;
        if Pos('return nl(', s) > 0 then
        begin
          nl := GetBetween('return nl(', ')', s);
          nl := TrimChar(nl, ['''', '"']);
        end;
        s := '';

        if (base_url <> '') and (startkey <> '') and (gid <> '') and
          (startpage <> '') then
          iurl := RemoveURLDelim(base_url) + '/s/' + startkey + '/' + gid + '-' + startpage
        else
          iurl := FillHost(Module.RootURL, AURL);
        if nl <> '' then
        begin
          if nls = '' then
            nls := '?nl=' + nl
          else
            nls := nls + '&nl=' + nl;
          iurl := iurl + nls;
        end;
        if not GETWithLogin(DownloadThread.FHTTP, iurl, Module) then Break;
        Inc(rcount);
      end;
    end;
  end;

begin
  Result := False;
  if DownloadThread = nil then Exit;
  reconnect := DownloadThread.FHTTP.RetryCount;
  iurl := FillHost(Module.RootURL, AURL);
  if settingsimagesize <= High(settingsimagesizestr) then
    DownloadThread.FHTTP.Cookies.Values['uconfig'] := settingsimagesizestr[settingsimagesize];
  if GETWithLogin(DownloadThread.FHTTP, iurl, Module) then begin
    query := TXQueryEngineHTML.Create(DownloadThread.FHTTP.Document);
    try
      Result := DoDownloadImage;
    finally
      query.Free;
    end;
  end;
end;

procedure RegisterModule;

  function AddWebsiteModule(AWebsite, ARootURL: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do begin
      Website := AWebsite;
      RootURL := ARootURL;
      Category := 'H-Sites';
      MaxTaskLimit := 1;
      MaxConnectionLimit := 2;
      SortedList := True;
      DynamicPageLink := True;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnDownloadImage := @DownloadImage;
    end;
  end;

begin
  with AddWebsiteModule('E-Hentai', 'https://e-hentai.org') do
    AddOptionComboBox(@settingsimagesize, 'SettingsImageSize', @RS_SettingsImageSize, @RS_SettingsImageSizeItems);
  with AddWebsiteModule('ExHentai', 'https://exhentai.org') do begin
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
