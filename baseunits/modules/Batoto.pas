unit Batoto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  accountmanagerdb, SimpleLogger, synautil;

implementation

const
  modulename = 'Batoto';
  urlroot = 'http://bato.to';
  urllogin = 'https://bato.to/forums/index.php?app=core&module=global&section=login&do=process';
  dirurls: array [0..1] of String = (
    '/comic/_/sp/',
    '/comic/_/comics/');
  perpage = 1000;
  dirparam = '?sort_col=record_saved&sort_order=desc&per_page=';

var
  locklogin: TRTLCriticalSection;
  onlogin: Boolean = False;

function Login(var AHTTP: THTTPSendThread): Boolean;
var
  query: TXQueryEngineHTML;
  loginform: THTMLForm;
  key: string;
begin
  Result := False;
  if AHTTP = nil then Exit;
  if Account.Enabled[modulename] = False then Exit;
  if Account.Username[modulename] = '' then Exit;

  if TryEnterCriticalsection(locklogin) > 0 then
    with AHTTP do begin
      onlogin := True;
      Account.Status[modulename] := asChecking;
      Reset;
      Cookies.Clear;
      Writelog_V('Batoto, login: get login form');
      if GET(urlroot) then begin
        loginform := THTMLForm.Create;
        query := TXQueryEngineHTML.Create;
        try
          query.ParseHTML(StreamToString(Document));
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
            Writelog_V('Batoto, login: send authentification');
            if POST(urllogin, loginform.GetData) then begin
              if ResultCode = 200 then begin
                Result := Cookies.Values['pass_hash'] <> '';
                if Result then begin
                  Writelog_V('Batoto, login: success');
                  Account.Cookies[modulename] := GetCookies;
                  Account.Status[modulename] := asValid;
                end else begin
                  Writelog_V('Batoto, login: failed, wrong user/password?');
                  Account.Status[modulename] := asInvalid;
                end;
                Account.Save;
              end
              else
                Writelog_V(['Batoto, login: failed, unexpected server reply: ',ResultCode,' ',ResultString]);
            end
            else
              Writelog_V('Batoto, login: connection failed');
          end;
        finally
          query.Free;
          loginform.Free
        end;
      end;
      onlogin := False;
      if Account.Status[modulename] = asChecking then
        Account.Status[modulename] := asUnknown;
      LeaveCriticalsection(locklogin);
    end
  else
  begin
    while onlogin do Sleep(1000);
    if Result then AHTTP.Cookies.Text := Account.Cookies[modulename];
  end;
end;

function GETWithLogin(var AHTTP: THTTPSendThread; AURL: String): Boolean;
var
  s: String;
begin
  Result:=False;
  AHTTP.Cookies.Text:=Account.Cookies['Batoto'];
  Result:=AHTTP.GET(AURL);
  if Result then begin
    Result:=True;
    if Account.Enabled[modulename]=False then Exit;
    if Account.Username[modulename]='' then Exit;
    if Account.Status[modulename]=asInvalid then Exit;
    s:=StreamToString(AHTTP.Document);
    Result := (Pos('class=''logged_in''', s) > 0) or (Pos('class="logged_in"', s) > 0);
    if not Result then begin
      Result:=Login(AHTTP);
      if Result then
        Result:=AHTTP.GET(AURL)
      else
      begin
        Result:=True;
        AHTTP.Document.Clear;
        WriteStrToStream(AHTTP.Document, s);
      end;
    end;
  end;
end;

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  s: String;
begin
  Result:=NET_PROBLEM;
  Page:=1;
  if MangaInfo =nil then Exit(UNKNOWN_ERROR);
  MangaInfo.FHTTP.Cookies.Text:=Account.Cookies[modulename];
  if MangaInfo.FHTTP.GET(Module.RootURL+dirurls[Module.CurrentDirectoryIndex]+dirparam+IntToStr(perpage)) then begin
    Result:=NO_ERROR;
    query:=TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      s:=Trim(query.XPathString('//ul[@class="ipsList_inline left pages"]/li/a'));
      if s<>'' then
        Page:=StrToIntDef(Trim(SeparateRight(LowerCase(s),'page 1 of ')),1);
    finally
      query.Free;
    end;
  end;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  s: String;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  s:=Module.RootURL+dirurls[Module.CurrentDirectoryIndex]+dirparam+IntToStr(perpage);
  if AURL<>'0' then s+='&st='+IntToStr(StrToInt(AURL)*perpage);
  MangaInfo.FHTTP.Cookies.Text:=Account.Cookies[modulename];
  if MangaInfo.FHTTP.GET(s) then begin
    Result:=NO_ERROR;
    query:=TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//table[@class="ipb_table topic_list hover_rows"]/tbody/tr/td/h4/a') do begin
        WriteLog_D('name: '+v.toString);
        ALinks.Add(v.toNode.getAttribute('href'));
        ANames.Add(v.toString);
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
  v, w: IXQValue;
  s, t, l: String;
  i: Integer;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  with MangaInfo do begin
    mangaInfo.website := modulename;
    mangaInfo.url := FillHost(urlroot, AURL);
    while onlogin do Sleep(1000);
    FHTTP.Cookies.Text := Account.Cookies[modulename];
    if GETWithLogin(FHTTP, mangaInfo.url) then begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(FHTTP.Document));
        with mangaInfo do begin
          coverLink := Query.XPathString('//div[@class="ipsBox"]//img/@src');
          if title = '' then
            title := Query.XPathString('//h1[@class="ipsType_pagetitle"]');
          for v in query.XPath('//table[@class="ipb_table"]//tr') do begin
            s := v.toString;
            if Pos('Author:', s) > 0 then authors:= GetRightValue('Author:', s)
            else if Pos('Artist:', s) > 0 then artists:= GetRightValue('Artist:', s)
            else if Pos('Description:', s) > 0 then summary:= GetRightValue('Description:', s)
            else if Pos('Status:', s) > 0 then begin
              if Pos('Ongoing', s) > 0 then status := '1'
              else status := '0';
            end;
          end;
          v := query.XPath('//table[@class="ipb_table"]//tr[starts-with(*, "Genres:")]/td/a');
          if v.Count > 0 then begin
            genres := '';
            for i := 1 to v.Count do AddCommaString(genres, v.get(i).toString);
          end;

          if OptionBatotoShowAllLang then
            s := '//table[@class="ipb_table chapters_list"]//tr[starts-with(@class, "row lang")]'
          else s := '//table[@class="ipb_table chapters_list"]//tr[starts-with(@class, "row lang_English")]';
          for v in query.XPath(s) do begin
            w := query.XPath('td[1]/a', v.toNode);
            chapterLinks.Add(w.toNode.getAttribute('href'));
            t := w.toString;
            if OptionBatotoShowAllLang then begin
              l := query.XPath('td[2]/div', v.toNode).toNode.getAttribute('title');
              if l <> '' then t += ' ['+ l +']';
            end;
            if OptionBatotoShowScanGroup then begin
              l := query.XPath('td[3]', v.toNode).toString;
              if l <> '' then t += ' ['+ l +']';
            end;
            chapterName.Add(t);
          end;
          InvertStrings([chapterLinks, chapterName])
        end;
      finally
        query.Free;
      end;
    end else Result := INFORMATION_NOT_FOUND;
  end;
end;

function GetPageNumber(var DownloadThread: TDownloadThread; const AURL: String;
  Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  cid: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container, DownloadThread.FHTTP do begin
    Cookies.Text := Account.Cookies[modulename];
    Headers.Values['Referer'] := ' ' + urlroot + '/reader';
    cid := SeparateRight(AURL, '/reader#');
    if GET(urlroot + '/areader?id=' + cid + '&p=1') then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        PageContainerLinks.Text := cid;
        if query.XPathString('//select[@id="page_select"]') <> '' then
          PageNumber := Query.XPath('//div[1]/ul/li/select[@id="page_select"]/option/@value').Count
        else begin
        // long-strip view
          PageLinks.Clear;
          for v in query.XPath('//div/img/@src') do
            PageLinks.Add(v.toString);
        end;
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
  rurl: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container, DownloadThread.FHTTP do begin
    if PageContainerLinks.Text = '' then Exit;
    Cookies.Text := Account.Cookies[modulename];
    rurl := urlroot + '/areader?id=' + PageContainerLinks[0] + '&p=' + IntToStr(DownloadThread.WorkCounter + 1);
    Headers.Values['Referer'] := ' ' + Module.RootURL + '/reader';
    if GET(rurl) then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        PageLinks[DownloadThread.workCounter] := query.XPathString('//div[@id="full_image"]//img/@src');
      finally
        query.Free;
      end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := modulename;
    RootURL := urlroot;
    MaxTaskLimit := 1;
    MaxConnectionLimit := 2;
    AccountSupport := True;
    SortedList := True;
    InformationAvailable := True;
    TotalDirectory := Length(dirurls);
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
    OnLogin := @Login;
  end;
end;

initialization
  InitCriticalSection(locklogin);
  RegisterModule;

finalization
  DoneCriticalsection(locklogin);

end.
