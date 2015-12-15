unit Batoto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  accountmanagerdb, synautil, HTMLUtil, RegExpr;

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
  onlogin: Boolean = False;

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  Parse: TStringList;

  procedure ScanParse;
  var
    i: Integer;
    s: String;
  begin
    if Parse.Count > 0 then
      for i := 0 to Parse.Count - 1 do
        if (Pos('Page 1 of ', Parse.Strings[i]) > 0) then
        begin
          s := GetString(Parse.Strings[i] + '~!@', 'Page 1 of ', '~!@');
          Page := StrToInt(TrimLeft(TrimRight(s)));
          Break;
        end;
  end;

begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit;
  Parse := TStringList.Create;
  try
    MangaInfo.FHTTP.Cookies.Text := Account.Cookies[modulename];
    if MangaInfo.GetPage(TObject(Parse),
      Module.RootURL + dirurls[Module.CurrentDirectoryIndex] +
      dirparam + IntToStr(perpage), 3) then
    begin
      Result := INFORMATION_NOT_FOUND;
      ParseHTML(Parse.Text, Parse);
      if Parse.Count > 0 then
      begin
        Result := NO_ERROR;
        ScanParse;
      end;
    end;
  finally
    Parse.Free;
  end;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String; Module: TModuleContainer): Integer;
var
  Parse: TStringList;
  s: String;
  p: Integer;

  procedure ScanParse;
  var
    i, j: Integer;
  begin
    j := -1;
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(Parse[i]) = 'table') and
        (GetVal(Parse[i], 'class') = 'ipb_table topic_list hover_rows') then
      begin
        j := i;
        Break;
      end;
    if (j > -1) and (j < Parse.Count) then
      for i := j to Parse.Count - 1 do
        if Pos('</table', Parse[i]) <> 0 then
          Break
        else
        if GetTagName(Parse[i]) = 'a' then
        begin
          Links.Add(GetVal(Parse[i], 'href'));
          Names.Add(CommonStringFilter(Parse[i + 1]));
        end;
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  s := Module.RootURL + dirurls[Module.CurrentDirectoryIndex] +
    dirparam + IntToStr(perpage);
  p := StrToIntDef(URL, 0);
  if p > 0 then
    s += '&st=' + (IntToStr(p * perpage));
  Parse := TStringList.Create;
  try
    MangaInfo.FHTTP.Cookies.Text := Account.Cookies[modulename];
    if MangaInfo.GetPage(TObject(Parse), s, 3) then
    begin
      Result := INFORMATION_NOT_FOUND;
      ParseHTML(Parse.Text, Parse);
      if Parse.Count > 0 then
      begin
        Result := NO_ERROR;
        ScanParse;
      end;
    end;
  finally
    Parse.Free;
  end;
end;

function CheckLogin(const source: string): Boolean;
begin
  if source = '' then Exit(False);
  Result := (Pos('class=''logged_in''', source) > 0) or (Pos('class="logged_in"', source) > 0);
end;

function Login(http: THTTPSendThread): Boolean;
var
  query: TXQueryEngineHTML;
  source: TStringList;
  s, key, user, pass: string;
begin
  Result := False;
  if http = nil then Exit;
  if Account.Enabled[modulename] = False then Exit;
  if Account.Username[modulename] = '' then Exit;

  if TryEnterCriticalsection(locklogin) > 0 then
    with http do begin
      onlogin := True;
      Reset;
      Cookies.Clear;
      if Get(urlroot) then begin
        source := TStringList.Create;
        query := TXQueryEngineHTML.Create;
        try
          source.LoadFromStream(Document);
          query.ParseHTML(source.Text);
          key := query.XPathString('//input[@name="auth_key"]/@value');
          if key <> '' then begin
            user:= Account.Username[modulename];
            pass:= Account.Password[modulename];
            s := 'auth_key='+key+'&referer=https%3A%2F%2Fbato.to%2F'+'&ips_username='+user+'&ips_password='+pass+'&rememberMe=1';
            Clear;
            Headers.Values['Referer'] := ' https://bato.to/';
            if Post(urllogin, s) then begin
              if ResultCode = 200 then begin
                Result := Cookies.Values['pass_hash'] <> '';
                if Result then begin
                  Account.Cookies[modulename] := GetCookies;
                  Account.Status[modulename] := 'OK';
                end else begin
                  Account.Status[modulename] := 'Invalid user/password!';
                end;
                Account.Save;
              end;
            end;
          end;
        finally
          query.Free;
          source.Free
        end;
      end;
      onlogin := False;
      LeaveCriticalsection(locklogin);
    end
  else
  begin
    while onlogin do Sleep(1000);
    if Result then http.Cookies.Text := Account.Cookies[modulename];
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
var
  source: TStringList;
  query: TXQueryEngineHTML;
  v: IXQValue;
  s: String;
  i: Integer;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  with MangaInfo do begin
    mangaInfo.website := modulename;
    mangaInfo.url := FillHost(urlroot, URL);
    while onlogin do Sleep(1000);
    FHTTP.Cookies.Text := Account.Cookies[modulename];
    if FHTTP.GET(mangaInfo.url) then begin
      Result := NO_ERROR;
      source := TStringList.Create;
      query := TXQueryEngineHTML.Create;
      try
        source.LoadFromStream(FHTTP.Document);
        if not CheckLogin(source.Text) then begin
          if Login(FHTTP) then begin
            FHTTP.GET(mangaInfo.url, source);
          end;
        end;

        query.ParseHTML(source.Text);
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
            for i := 0 to v.Count do AddCommaString(genres, v.get(i).toString);
          end;

          if OptionBatotoShowAllLang then
            s := '//table[@class="ipb_table chapters_list"]//tr[starts-with(@class, "row lang")]/td[1]/a'
          else s := '//table[@class="ipb_table chapters_list"]//tr[starts-with(@class, "row lang_English")]/td[1]/a';
          for v in query.XPath(s) do begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(v.toString);
          end;
          InvertStrings([chapterLinks, chapterName])
        end;
      finally
        query.Free;
        source.Free;
      end;
    end else Result := INFORMATION_NOT_FOUND;
  end;
end;

function GetPageNumber(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  source: TStringList;
  query: TXQueryEngineHTML;
  v: IXQValue;
  cid: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container, DownloadThread.FHTTP do begin
    Cookies.Text := Account.Cookies[modulename];
    Headers.Values['Referer'] := ' ' + urlroot + '/reader';
    cid := SeparateRight(URL, '/reader#');
    if GET(urlroot + '/areader?id=' + cid + '&p=1') then begin
      Result := True;
      source := TStringList.Create;
      query := TXQueryEngineHTML.Create;
      try
        source.LoadFromStream(Document);
        query.ParseHTML(source.Text);
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
        source.Free;
      end;
    end;
  end;
end;

function GetImageURL(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  source: TStringList;
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
      source := TStringList.Create;
      query := TXQueryEngineHTML.Create;
      try
        source.LoadFromStream(Document);
        query.ParseHTML(source.Text);
        PageLinks[DownloadThread.workCounter] := query.XPathString('//div[@id="full_image"]//img/@src');
      finally
        query.Free;
        source.Free;
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
    MaxConnectionLimit := 3;
    SortedList := True;
    InformationAvailable := True;
    TotalDirectory := Length(dirurls);
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  InitCriticalSection(locklogin);
  RegisterModule;

finalization
  DoneCriticalsection(locklogin);

end.
