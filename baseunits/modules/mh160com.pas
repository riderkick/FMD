unit mh160com;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  synacode, synautil, LConvEncoding, HTMLUtil, base64, RegExpr;

implementation

const
  diralpha = 'abcdefghijklmnopqrstuvwxyz';

function GetDirectoryPageNumber(var {%H-}MangaInfo: TMangaInformation;
  var Page: Integer; {%H-}Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  Page := Length(diralpha);
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String; Module: TModuleContainer): Integer;
var
  Source, Parse: TStringList;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
    begin
      if Parse[i] = '<dt>' then
        if GetTagName(Parse[i + 1]) = 'a' then
        begin
          Links.Add(GetVal(Parse[i + 1], 'href'));
          Names.Add(CommonStringFilter(Parse[i + 2]));
        end;
    end;
  end;

begin
  Result := INFORMATION_NOT_FOUND;
  if MangaInfo = nil then
    Exit;
  Source := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Source), Module.RootURL + '/kanmanhua/' +
      diralpha[StrToInt(URL) + 1] + '.html', 3) then
    begin
      Result := NO_ERROR;
      Source.Text := CP936ToUTF8(Source.Text);
      Parse := TStringList.Create;
      try
        ParseHTML(Source.Text, Parse);
        if Parse.Count > 0 then
          ScanParse;
      finally
        Parse.Free;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
  const Reconnect: Cardinal; Module: TModuleContainer): Integer;
var
  Source, Parse: TStringList;

  procedure ScanChapters(const StartIndex: Integer);
  var
    i: Integer;
    g: String = '';
  begin
    with MangaInfo.mangaInfo do
    begin
      for i := StartIndex to Parse.Count - 1 do
      begin
        if Parse[i] = '</div>' then
          Break;
        if GetTagName(Parse[i]) = 'a' then
        begin
          chapterLinks.Add(GetVal(Parse[i], 'href'));
          chapterName.Add(g + Trim(Parse[i + 1]));
        end;
      end;
      //invert chapters
      if MangaInfo.mangaInfo.chapterLinks.Count > 0 then
        InvertStrings([chapterLinks, chapterName]);
    end;
  end;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      with MangaInfo.mangaInfo do
      begin
        //title
        if title = '' then
          if GetVal(Parse[i], 'class') = 'intro_l' then
            if GetVal(Parse[i + 2], 'class') = 'title' then
              title := CommonStringFilter(Parse[i + 5]);

        //cover
        if coverLink = '' then
          if GetVal(Parse[i], 'class') = 'cover' then
            if GetTagName(Parse[i + 1]) = 'img' then
            begin
              coverLink := GetVal(Parse[i + 1], 'src');
              if Pos('http', coverLink) <> 1 then
                coverLink := Module.RootURL + coverLink;
            end;

        //author
        if (Pos('原著作者：', Parse[i]) <> 0) and
          (Parse[i + 1] = '</em>') then
          authors := CommonStringFilter(Parse[i + 2]);

        //genres
        if (Pos('剧情类别：', Parse[i]) <> 0) and
          (Parse[i + 1] = '</em>') then
          genres := Trim(Parse[i + 3]);

        //summary
        if Pos('class="introduction"', Parse[i]) <> 0 then
          summary := Trim(Parse[i + 2]);

        //chapters
        if GetVal(Parse[i], 'class') = 'plist pnormal' then
          ScanChapters(i);
      end;
  end;

begin
  Result := INFORMATION_NOT_FOUND;
  if MangaInfo = nil then
    Exit;
  MangaInfo.mangaInfo.website := Module.Website;
  MangaInfo.mangaInfo.url := FillHost(Module.RootURL, URL);
  Source := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Source), MangaInfo.mangaInfo.url, Reconnect) then
    begin
      Result := NO_ERROR;
      Source.Text := CP936ToUTF8(Source.Text);
      Parse := TStringList.Create;
      try
        ParseHTML(Source.Text, Parse);
        if Parse.Count > 0 then
          ScanParse;
      finally
        Parse.Free;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function padZero(const S: String; len: Integer): String;
begin
  Result := S;
  while Length(Result) < len do
    Result := '0' + Result;
end;

function getcurpic_skin4_20110501(const URL: String): String;

  function getrealurl(const S: String): String;
  begin
    Result := S;
    if Pos('img1.fshmy.com', S) <> 0 then
      Result := StringReplace(S, 'img1.fshmy.com', 'img1.hgysxz.cn', [])
    else if Pos('imgs.k6188.com', S) <> 0 then
      Result := StringReplace(S, 'imgs.k6188.com', 'imgs.zhujios.com', [])
    else if Pos('073.k6188.com', S) <> 0 then
      Result := StringReplace(S, '073.k6188.com', 'cartoon.zhujios.com', [])
    else if Pos('cartoon.jide123.cc', S) <> 0 then
      Result := StringReplace(S, 'cartoon.jide123.cc',
        'cartoon.shhh88.com', [])
    else if Pos('www.jide123.com', S) <> 0 then
      Result := StringReplace(S, 'www.jide123.com', 'cartoon.shhh88.com', [])
    else if Pos('cartoon.chuixue123.com', S) <> 0 then
      Result := StringReplace(S, 'cartoon.chuixue123.com',
        'cartoon.shhh88.com', [])
    else if Pos('p10.tuku.cc:8899', S) <> 0 then
      Result := StringReplace(S, 'p10.tuku.cc:8899', 'tkpic.tukucc.com', []);
  end;

  function getkekerealurl(const S: String): String;
  var
    i: Integer;
    sn: String;
  begin
    Result := S;
    for i := 1 to 15 do
    begin
      sn := '/dm' + padZero(IntToStr(i), 2) + '/';
      if Pos(sn, S) <> 0 then
      begin
        Result := 'http://2.cococomic.com:9115' + sn + SeparateRight(S, sn);
        Break;
      end;
    end;
  end;

  function getremoteqqurl(const S: String): String;
  var
    qqfilename: String;
  begin
    Result := S;
    qqfilename := SeparateRight(S, 'dir_path=/');
    qqfilename := StringReplace(qqfilename, '&name', '', []);
    qqfilename := StringReplace(qqfilename, 'mif2', 'jpg', []);
    qqfilename := StringReplace(qqfilename, '/', '_', [rfReplaceAll]);
    Result := 'http://img11.aoyuanba.com/pictmdown.php?p=' +
      EncodeStringBase64(S) + '$sf=' + qqfilename +
      '&ym=' + 'http://img11.hgysxz.cn';
  end;

begin
  Result := URL;
  if Pos('qq.com/store_file_download', URL) <> 0 then
    Result := getremoteqqurl(URL)
  else
  if Pos('/ok-comic', URL) <> 0 then
    Result := getkekerealurl(URL)
  else if Pos('mangafiles.com', URL) <> 0 then
    Result := 'http://img6.aoyuanba.com:8056/pictmdown.php?p=' + EncodeStringBase64(URL)
  else if Pos('imgs.gengxin123.com', URL) <> 0 then
  begin
    Result := StringReplace(URL, 'imgs.gengxin123.com', 'imgs1.ysryd.com',
      [rfIgnoreCase]);
    Result := 'http://imgsty1.aoyuanba.com/pictmdown.php?bu=' + 'http://www.kxdm.com/' +
      '&p=' + EncodeStringBase64(Result);
  end
  else if Pos('imgs1.ysryd.com', URL) <> 0 then
    Result := 'http://imgsty1.aoyuanba.com/pictmdown.php?bu=' +
      'http://www.kxdm.com/' + '&p=' + EncodeStringBase64(URL)
  else if Pos('dmzj.com', URL) <> 0 then
    Result := 'http://imgsty.aoyuanba.com:8056/pictmdown.php?bu=' +
      'http://manhua.dmzj.com/' + '&p=' + EncodeStringBase64(EncodeURL(URL))
  else if Pos('imgsrc.baidu.com', URL) <> 0 then
    Result := 'http://img7.aoyuanba.com:8056/picinc/qTcms.Pic.FangDao.asp?p=' +
      EncodeStringBase64(URL)
  else if Pos('sinaimg.cn', URL) <> 0 then
    Result := 'http://img7.aoyuanba.com:8056/picinc/qTcms.Pic.FangDao.asp?p=' +
      EncodeStringBase64(URL)
  else if Pos('jumpcn.cc', URL) <> 0 then
    Result := 'http://img7.aoyuanba.com:8056/picinc/qTcms.Pic.FangDao.asp?p=' +
      EncodeStringBase64(URL)
  else if Pos('JLmh160', URL) <> 0 then
    Result := 'http://img3.aoyuanba.com/picinc/qTcms.Pic.FangDao.asp?p=' +
      EncodeStringBase64(URL)
  else
    Result := getrealurl(URL);
end;

function GetPageNumber(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Container: TTaskContainer;
  Regx: TRegExpr;
  picTree: String;

  procedure GetURLs;
  var
    realurls: TStringList;
    i, j: Integer;
    r, u: String;
  begin
    realurls := TStringList.Create;
    try
      realurls.NameValueSeparator := '=';
      Regx.Expression := '(?ig)^.*realurl\.replace\("(.+)","(.+)".*$';
      for i := 0 to Source.Count - 1 do
      begin
        if Regx.Exec(Source[i]) then
        begin
          r := Trim(LowerCase(Regx.Replace(Source[i], '$1', True)));
          u := Trim(LowerCase(Regx.Replace(Source[i], '$2', True)));
          if (r <> '') and (u <> '') then
            realurls.Values[r] := u;
        end;
      end;

      if (realurls.Count > 0) and (Container.PageLinks.Count > 0) then
      begin
        for i := 0 to Container.PageLinks.Count - 1 do
        begin
          for j := 0 to realurls.Count - 1 do
          begin
            if Pos(realurls.Names[j], LowerCase(Container.PageLinks[i])) <> 0 then
              Container.PageLinks[i] := StringReplace(Container.PageLinks[i],
                realurls.Names[j], realurls.ValueFromIndex[j], [rfIgnoreCase]);
          end;
        end;
      end;
    finally
      realurls.Free;
    end;
  end;

  function ScanSource: Boolean;
  var
    i: Integer;
    //jsurl: String = '';
  begin
    Result := False;
    Regx.Expression := '(?ig)^.*var\spicTree\s*=[''"](.+?)[''"].*$';
    picTree := Regx.Replace(Source.Text, '$1', True);

    if picTree <> '' then
    begin
      picTree := DecodeStringBase64(picTree);
      picTree := StringReplace(picTree, '$qingtiandy$', #13#10, [rfReplaceAll]);
      Container.PageLinks.AddText(picTree);
      if Container.PageLinks.Count > 0 then
        for i := 0 to Container.PageLinks.Count - 1 do
          Container.PageLinks[i] := getcurpic_skin4_20110501(Container.PageLinks[i]);

      {
      if Container.PageLinks.Count > 0 then
      begin
        for i := 0 to Source.Count - 1 do
          if (Pos('<script', Source[i]) <> 0) and
            (Pos('/base64.js', Source[i]) <> 0) then
          begin
            jsurl := GetVal(Source[i], 'src');
            jsurl := StringReplace(jsurl, '../..', Module.RootURL, []);
            Break;
          end;
        if jsurl <> '' then
        begin
          Source.Clear;
          if DownloadThread.GetPage(TObject(Source), jsurl,
            Container.Manager.retryConnect) then
            if Source.Count > 0 then
            begin
              Result := True;
              GetURLs;
            end;
        end;
      end;
      }
    end;
  end;

begin
  Result := False;
  if DownloadThread = nil then
    Exit;
  Container := DownloadThread.manager.container;
  Container.PageLinks.Clear;
  Container.PageContainerLinks.Clear;
  Container.PageNumber := 0;
  Source := TStringList.Create;
  try
    if DownloadThread.GetPage(TObject(Source), FillHost(Module.RootURL, URL),
      Container.Manager.retryConnect) then
    begin
      if Source.Count > 0 then
      begin
        Regx := TRegExpr.Create;
        try
          Source.Text := CP936ToUTF8(Source.Text);
          Result := ScanSource;
        finally
          Regx.Free;
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
    Website := 'mh160';
    RootURL := 'http://www.mh160.com';
    SortedList := False;
    InformationAvailable := True;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
