unit EHentai;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  RegExpr;

implementation

const
  dirURL = 'f_doujinshi=on&f_manga=on&f_western=on&f_apply=Apply+Filter';

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit;
  Source := TStringList.Create;
  try
    if GetPage(MangaInfo.FHTTP, TObject(Source), Module.RootURL + '/?' + dirURL, 3) then
    begin
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          Page := StrToIntDef(Query.CSSString(
            'table.ptt>tbody>tr>td:nth-last-child(2)>a'), 1) + 1;
        finally
          Query.Free;
        end;
      end
      else Result := INFORMATION_NOT_FOUND;
    end;
  finally
    Source.Free;
  end;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String; Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  rurl: String;
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  Source := TStringList.Create;
  try
    if URL = '0' then rurl := Module.RootURL + '/?' + dirURL
    else rurl := Module.RootURL + '/?page=' + IncStr(URL) + '&' + dirURL;
    if GetPage(MangaInfo.FHTTP, TObject(Source), rurl, 3) then
    begin
      Result := INFORMATION_NOT_FOUND;
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          for v in Query.XPath('//table[@class="itg"]/tbody/tr/td/div/div/a') do
          begin
            Names.Add(v.toString);
            Links.Add(v.toNode.getAttribute('href'));
          end;
        finally
          Query.Free;
        end;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
var
  info: TMangaInfo;
  Source: TStringList;
  Query: TXQueryEngineHTML;
  v: IXQValue;

  procedure ScanParse;
  var
    getOK: Boolean;
  begin
    getOK := True;
    // check content warning
    if Pos('Content Warning', Query.XPathString('//div/h1')) > 0 then
    begin
      getOK := GetPage(MangaInfo.FHTTP, TObject(Source), info.url +
        '?nw=session', Reconnect);
      if getOK then
        Query.ParseHTML(Source.Text);
    end;
    if getOK then
    begin
      with info do begin
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
        chapterLinks.Add(url);
        chapterName.Add(title);
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
  info := MangaInfo.mangaInfo;
  info.website := Module.Website;
  info.url := FillHost(Module.RootURL, URL);
  Source := TStringList.Create;
  try
    if GetPage(MangaInfo.FHTTP, TObject(Source), info.url, Reconnect) then
    begin
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        // if there is only 1 line, it's banned message!
        if Source.Count = 1 then
          info.summary := Source.Text
        else
        begin
          Query := TXQueryEngineHTML.Create(Source.Text);
          try
            ScanParse;
          finally
            Query.Free;
          end;
        end;
      end
      else
        Result := INFORMATION_NOT_FOUND;
    end;
  finally
    Source.Free;
  end;
end;

function GetPageNumber(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Container: TTaskContainer;
  Source: TStringList;
  Query: TXQueryEngineHTML;
  v: IXQValue;
  rurl: String;

  procedure GetImageLink;
  begin
    for v in Query.XPath('//*[@class="gdtm"]/div/a/@href') do
    begin
      Container.PageLinks.Add('G');
      Container.PageContainerLinks.Add(v.toString);
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
    if Pos('Content Warning', Query.XPathString('//div/h1')) > 0 then
    begin
      getOK := GetPage(DownloadThread.FHTTP, TObject(Source), rurl + '?nw=session',
        Container.Manager.retryConnect);
      if getOK then
        Query.ParseHTML(Source.Text);
    end;
    if getOK then
    begin
      GetImageLink;
      //get page count
      p := StrToIntDef(Query.CSSString(
        'table.ptt>tbody>tr>td:nth-last-child(2)>a'), 1) - 1;
      if p > 0 then
        for i := 1 to p do
          if GetPage(DownloadThread.FHTTP, TObject(Source), rurl + '?p=' + IntToStr(i),
            Container.Manager.retryConnect) then
          begin
            Query.ParseHTML(Source.Text);
            GetImageLink;
          end;
    end;
  end;

begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  Container.PageLinks.Clear;
  Container.PageContainerLinks.Clear;
  Container.PageNumber := 0;
  Source := TStringList.Create;
  try
    rurl := AppendURLDelim(FillHost(Module.RootURL, URL));
    if GetPage(DownloadThread.FHTTP, TObject(Source), rurl,
      Container.Manager.retryConnect) then
    begin
      if Source.Count > 0 then
      begin
        Result := True;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          ScanParse;
        finally
          Query.Free;
        end;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function DownloadImage(var DownloadThread: TDownloadThread;
  const URL, Path, Name, Prefix: String; Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Container: TTaskContainer;
  Query: TXQueryEngineHTML;
  iurl: String;
  reconnect: Integer;

  function DoDownloadImage: Boolean;
  var
    i, rcount: Integer;
    base_url, startkey, gid, startpage, nl: String;
  begin
    rcount := 0;
    Result := False;
    while (not Result) and (not DownloadThread.IsTerminated) do begin
      Query.ParseHTML(Source.Text);
      iurl := Query.XPathString('//*[@id="img"]/@src');
      if iurl = '' then
        iurl := Query.XPathString('//a/img/@src[not(contains(.,"ehgt.org/"))]');
      if iurl <> '' then
        Result := SaveImage(DownloadThread.FHTTP, -1, iurl, Path,
          Name, Prefix);
      if DownloadThread.IsTerminated then Break;
      if not Result then
      begin
        base_url := '';
        startkey := '';
        gid := '';
        startpage := '';
        nl := '';
        //get url param
        for i := 0 to Source.Count - 1 do begin
          if Pos('var base_url=', Source[i]) > 0 then
            base_url := RemoveURLDelim(GetValuesFromString(Source[i], '='))
          else if Pos('var startkey=', Source[i]) > 0 then
            startkey := GetValuesFromString(Source[i], '=')
          else if Pos('var gid=', Source[i]) > 0 then
            gid := GetValuesFromString(Source[i], '=')
          else if Pos('var startpage=', Source[i]) > 0 then
            startpage := GetValuesFromString(Source[i], '=')
          else if Pos('return nl(', Source[i]) > 0 then
            nl := ReplaceRegExpr('(?i)^.*nl\([''"](.*)[''"]\).*$',
              Source[i], '$1', True);
        end;
        if (base_url <> '') and (startkey <> '') and (gid <> '') and
          (startpage <> '') then
          iurl := base_url + '/s/' + startkey + '/' + gid + '-' + startpage
        else iurl := FillHost(Module.RootURL, URL);
        if nl <> '' then begin
          iurl := iurl + '?nl=' + nl;
          if not GetPage(DownloadThread.FHTTP, TObject(Source), iurl, reconnect) then
            Break;
        end else Break;
        if rcount >= reconnect then Break
        else Inc(rcount);
      end;
    end;
  end;

begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  reconnect := Container.Manager.retryConnect;
  iurl := FillHost(Module.RootURL, URL);
  Source := TStringList.Create;
  try
    if GetPage(DownloadThread.FHTTP, TObject(Source), iurl, reconnect) then
    begin
      if Source.Count > 0 then
      begin
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          Result := DoDownloadImage;
        finally
          Query.Free;
        end;
      end;
    end;
  finally
    Source.Free;
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
