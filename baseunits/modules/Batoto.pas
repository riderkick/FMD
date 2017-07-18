unit Batoto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  accountmanagerdb, XQueryEngineHTML, httpsendthread, dateutils,
  synautil, MultiLog;

implementation

const
  modulename = 'Batoto';
  urlroot = 'http://bato.to';
  urllogin = 'https://bato.to/forums/index.php?app=core&module=global&section=login&do=process';
  dirurls: array [0..1] of String = (
    '/comic/_/sp/',
    '/comic/_/comics/');
  perpage = 50;
  dirparam = '?sort_col=record_saved&sort_order=desc&per_page=';

var
  locklogin: TRTLCriticalSection;
  showalllang: Boolean = False;
  showscangroup: Boolean = False;
  serverselection: Integer = 0;

const
  serverselectionvalue : array [0..4] of String = (
    'img',
    'img3',
    'img4',
    'cdn',
    'cdn2'
    );

resourcestring
  RS_ShowAllLang = 'Show all language';
  RS_ShowScanGroup = 'Show scanlation group';
  RS_ServerSelection = 'Image server:';
  RS_ServerSelectionItems =
    'Auto' + LineEnding +
    'Image Server EU' + LineEnding +
    'Image Server NA' + LineEnding +
    'CDN (default)' + LineEnding +
    'CDN2 (testing)';

function Login(const AHTTP: THTTPSendThread): Boolean;
var
  query: TXQueryEngineHTML;
  loginform: THTMLForm;
  key: String;
begin
  Result := False;
  if AHTTP = nil then Exit;
  if Account.Enabled[modulename] = False then Exit;
  if Account.Username[modulename] = '' then Exit;

  if TryEnterCriticalsection(locklogin) > 0 then
    with AHTTP do begin
      Account.Status[modulename] := asChecking;
      Reset;
      Cookies.Clear;
      Logger.Send('Batoto, login: get login form');
      if GET(urlroot) then begin
        loginform := THTMLForm.Create;
        query := TXQueryEngineHTML.Create(Document);
        try
          key := query.XPathString('//input[@name="auth_key"]/@value');
          if key <> '' then begin
            with loginform do begin
              Put('auth_key', key);
              Put('referer', 'https://bato.to/');
              Put('ips_username', Account.Username[modulename]);
              Put('ips_password', Account.Password[modulename]);
              Put('rememberMe', '1');
            end;
            Clear;
            Headers.Values['Referer'] := ' https://bato.to/';
            Logger.Send('Batoto, login: send authentification');
            if POST(urllogin, loginform.GetData) then begin
              if ResultCode = 200 then begin
                Result := Cookies.Values['pass_hash'] <> '';
                if Result then begin
                  Logger.Send('Batoto, login: success');
                  Account.Cookies[modulename] := GetCookies;
                  Account.Status[modulename] := asValid;
                end else begin
                  Logger.SendError('Batoto, login: failed, wrong user/password?');
                  Account.Status[modulename] := asInvalid;
                end;
                Account.Save;
              end
              else
                Logger.SendError('Batoto, login: failed, unexpected server reply: ' +
                  IntToStr(ResultCode) + ' ' + ResultString);
            end
            else
              Logger.SendError('Batoto, login: connection failed');
          end;
        finally
          query.Free;
          loginform.Free
        end;
      end;
      if Account.Status[modulename] = asChecking then
        Account.Status[modulename] := asUnknown;
      LeaveCriticalsection(locklogin);
    end
  else
  begin
    EnterCriticalsection(locklogin);
    try
      if Result then
        AHTTP.Cookies.Text := Account.Cookies[modulename];
    finally
      LeaveCriticalsection(locklogin);
    end;
  end;
end;

function GETWithLogin(const AHTTP: THTTPSendThread; const AURL: String): Boolean;
var
  s: String;
begin
  Result := False;
  AHTTP.Cookies.Text := Account.Cookies['Batoto'];
  AHTTP.KeepAlive := False;
  Result := AHTTP.GET(AURL);
  if (AHTTP.ResultCode > 400) and (AHTTP.ResultCode < 500) then Exit;
  if Result then begin
    Result := True;
    if Account.Enabled[modulename] = False then Exit;
    if Account.Username[modulename] = '' then Exit;
    if Account.Status[modulename] = asInvalid then Exit;
    s := StreamToString(AHTTP.Document);
    Result := (Pos('class=''logged_in''', s) > 0) or (Pos('class="logged_in"', s) > 0);
    if not Result then begin
      Result := Login(AHTTP);
      if Result then
        Result := AHTTP.GET(AURL)
      else
      begin
        Result := True;
        AHTTP.Document.Clear;
        WriteStrToStream(AHTTP.Document, s);
      end;
    end;
  end;
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurls[WorkPtr] +
    dirparam + IntToStr(perpage))
  then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
        s := Trim(XPathString('//ul[@class="ipsList_inline left pages"]/li/a'));
        if s <> '' then
          Page := StrToIntDef(Trim(SeparateRight(LowerCase(s), 'page 1 of ')), 1);
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL + dirurls[Module.CurrentDirectoryIndex] + dirparam + IntToStr(perpage);
  if AURL <> '0' then s += '&st=' + IntToStr(StrToInt(AURL) * perpage);
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//table[@class="ipb_table topic_list hover_rows"]/tbody/tr/td/h4/a') do begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(v.toString);
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v, w: IXQValue;
  s, t, l: String;
  i: Integer;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  with MangaInfo.mangaInfo do begin
    website := modulename;
    url := FillHost(urlroot, AURL);
    if GETWithLogin(MangaInfo.FHTTP, url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
        try
          coverLink := XPathString('//div[@class="ipsBox"]//img/@src');
          if title = '' then
            title := XPathString('//h1[@class="ipsType_pagetitle"]');
          for v in XPath('//table[@class="ipb_table"]//tr') do begin
            s := v.toString;
            if Pos('Author:', s) > 0 then authors := GetRightValue('Author:', s)
            else if Pos('Artist:', s) > 0 then artists := GetRightValue('Artist:', s)
            else if Pos('Description:', s) > 0 then summary := GetRightValue('Description:', s)
            else if Pos('Status:', s) > 0 then begin
              if Pos('Ongoing', s) > 0 then status := '1'
              else status := '0';
            end;
          end;
          v := XPath('//table[@class="ipb_table"]//tr[starts-with(*, "Genres:")]/td/a');
          if v.Count > 0 then begin
            genres := '';
            for i := 1 to v.Count do AddCommaString(genres, v.get(i).toString);
          end;

          if showalllang then
            s := '//table[@class="ipb_table chapters_list"]//tr[starts-with(@class, "row lang")]'
          else s := '//table[@class="ipb_table chapters_list"]//tr[starts-with(@class, "row lang_English")]';
          for v in XPath(s) do begin
            w := XPath('td[1]/a', v.toNode);
            chapterLinks.Add(w.toNode.getAttribute('href'));
            t := w.toString;
            if showalllang then begin
              l := XPath('td[2]/div', v.toNode).toNode.getAttribute('title');
              if l <> '' then t += ' [' + l + ']';
            end;
            if showscangroup then begin
              l := XPath('td[3]', v.toNode).toString;
              if l <> '' then t += ' [' + l + ']';
            end;
            chapterName.Add(t);
          end;
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end else Result := INFORMATION_NOT_FOUND;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  v: IXQValue;
  cid, s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    Headers.Values['Referer'] := ' ' + urlroot + '/reader';
    cid := SeparateRight(AURL, '/reader#');
    Cookies.Text := Account.Cookies['Batoto'];
    if GET(urlroot + '/areader?id=' + cid + '&p=1') then begin
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageContainerLinks.Text := cid;
          if XPathString('//select[@id="page_select"]') <> '' then begin
            PageNumber := XPath('(//select[@id="page_select"])[1]/option/@value').Count;
            if PageNumber > 0 then begin
              s := XPathString('//div[@id="full_image"]//img/@src');
              if s <> '' then PageLinks.Add(s);
            end;
          end
          else begin
            // long-strip view
            PageLinks.Clear;
            for v in XPath('//div/img/@src') do
              PageLinks.Add(v.toString);
          end;
        finally
          Free;
        end;
    end;
  end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  rurl: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    if PageContainerLinks.Text = '' then Exit;
    rurl := urlroot + '/areader?id=' + PageContainerLinks[0] + '&p=' +
      IntToStr(DownloadThread.WorkId + 1);
    Headers.Values['Referer'] := ' ' + Module.RootURL + '/reader';
    Cookies.Text := Account.Cookies['Batoto'];
    if serverselection <> 0 then
      Cookies.Values['server_selection'] := serverselectionvalue[serverselection];
    if GET(rurl) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[DownloadThread.WorkId] := XPathString('//div[@id="full_image"]//img/@src');
        finally
          Free;
        end;
    end;
  end;
end;

function BeforeDownloadImage(const DownloadThread: TDownloadThread;
    var AURL: String; const Module: TModuleContainer): Boolean;
begin
  AURL := FillHost('http://' + serverselectionvalue[serverselection] + '.bato.to', AURL);
  Result := True;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := modulename;
    RootURL := urlroot;
    AccountSupport := True;
    SortedList := True;
    InformationAvailable := True;
    TotalDirectory := Length(dirurls);
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
    OnBeforeDownloadImage := @BeforeDownloadImage;
    OnLogin := @Login;
    AddOptionCheckBox(@showalllang,'ShowAllLang', @RS_ShowAllLang);
    AddOptionCheckBox(@showscangroup,'ShowScanGroup', @RS_ShowScanGroup);
    AddOptionComboBox(@serverselection,'ServerSelection', @RS_ServerSelection, @RS_ServerSelectionItems);
  end;
end;

initialization
  InitCriticalSection(locklogin);
  RegisterModule;

finalization
  DoneCriticalsection(locklogin);

end.
