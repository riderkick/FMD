unit MangaGo;

{$mode objfpc}{$H+}

interface

implementation

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, BaseCrypto, synautil, Graphics,
  Interfaces, Math, RegExpr, strutils, JSUnpack, syncobjs;

var
  initstr: String;
  cs: TCriticalSection;
  ks: TStringList;

const
  dirurl= '/list/directory/all/';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  MangaInfo.FHTTP.Headers.Values['Referer'] := Module.RootURL;
  if Mangainfo.FHTTP.GET(Module.RootURL + dirurl + '1/') then
  begin
    Result := NO_ERROR;
    Page := StrToIntDef(XPathString('count(//div[@class="pagination"]//ol/li//select/option)', MangaInfo.FHTTP.Document), 1);
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  ref: String;
begin
  Result := NET_PROBLEM;
  if AURL = '0' then ref := Module.RootURL
  else ref := Module.RootURL + dirurl + AURL + '/';
  MangaInfo.FHTTP.Headers.Values['Referer'] := ref;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + IncStr(AURL) + '/') then
  begin
    Result := NO_ERROR;
    XPathHREFtitleAll('//div[@class="directory_left"]//li/h3/a', MangaInfo.FHTTP.Document, ALinks, ANames);
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then
    begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//div[@class="left cover"]/img/@src'));
          title := XPathString('//h1');
          if RightStr(title, 6) = ' manga' then SetLength(title, Length(title) - 6);
          status := MangaInfoStatusIfPos(XPathString('//div[@class="manga_right"]//td/label[.="Status:"]/following-sibling::*/text()'));
          authors := XPathString('//div[@class="manga_right"]//td/label[.="Author:"]/string-join(following-sibling::*/text(),", ")');
          genres := XPathString('//div[@class="manga_right"]//td/label[.="Genre(s):"]/string-join(following-sibling::*/text(),", ")');
          summary := XPathString('//div[@class="manga_summary"]/string-join(text(),codepoints-to-string(10))');
          XPathHREFAll('//table[@id="chapter_table"]//td//a[not(@style)]', chapterLinks, chapterName);
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
      Reset;
      Headers.Values['Referer'] := url;
    end;
  end;
end;

function ReplaceHex(script: String): String;
var
  rg: TRegExpr;
  c: Char;
  s: String;
begin
  Result := Copy(script, 1, Length(script));
  rg := TRegExpr.Create('\\x[\da-f]{1,2}');
  try
    if rg.Exec(script) then begin
      s := SeparateRight(rg.Match[0], '\x');
      c := Chr(Byte(Hex2Dec(s) and $ff));
      Result := ReplaceString(Result, rg.Match[0], c);
      while rg.ExecNext do begin
        s := SeparateRight(rg.Match[0], '\x');
        c := Chr(Byte(Hex2Dec(s) and $ff));
        Result := ReplaceString(Result, rg.Match[0], c);
      end;
    end;
  finally
    rg.Free;
  end;
end;

function GetScriptText(script: String): String;
begin
  Result := SeparateRight(script, '(''');
  Result := SeparateLeft(Result, ''',');
end;

function GetA(script: String): Integer;
var s: String;
begin
  s := SeparateRight(script, ''',');
  s := SeparateLeft(s, ',');
  Result := StrToIntDef(s, 1);
end;

function GetC(script: String): Integer;
var s: String;
begin
  s := SeparateRight(script, ''',');
  s := SeparateRight(s, ',');
  s := SeparateLeft(s, ',');
  Result := StrToIntDef(s, 1);
end;

function GetWords(script: String): String;
var s: String;
begin
  s := SeparateRight(script, '(''');
  s := SeparateRight(s, ',"');
  Result := SeparateLeft(s, '"');
end;

function GetSeparator(script: String): String;
var
  s: String;
begin
  s := ReplaceHex(script);
  s := s.Substring(RPos('split', s));
  Result := GetBetween('(', ')', s);
  Result := ReplaceString(Result, '''', '');
  Result := ReplaceString(Result, '"', '');
end;

function DecryptScript(script: String): String;
var
  u: TJSUnpack36;
  words, sep, text: String;
begin
  Result := '';
  u := TJSUnpack36.Create;
  try
    text := ReplaceHex(GetScriptText(script));
    words := ReplaceHex(GetWords(script));
    sep := GetSeparator(script);
    Result := u.Unpack(text, GetA(script), GetC(script), words.Split(sep));
  finally
    u.Free;
  end;
end;

function GetKey(script: String): String;
var
  rg: TRegExpr;
begin
  Result := '';
  rg := TRegExpr.Create('\=\s*["'']@(.+?)!["'']');
  try
    if rg.Exec(script) then
      Result := '@' + rg.Match[1] + '!';
  finally
    rg.Free;
  end;
end;

function GetIV(script: String): String;
begin
  Result := GetKey(script);
end;

procedure FillK(script: String);
var
  rg1, rg2: TRegExpr;
begin
  if ks.Count > 0 then Exit;
  cs.Enter;
  rg1 := TRegExpr.Create('\[\s*["'']([0-9a-f]+)["'']\s*\]\s*\=\s*["'']([a\d]+)["'']');
  rg2 := TRegExpr.Create('\w\s*\=\s*["'']([a\d]+)["'']');
  try
    if ks.Count > 0 then Exit;
    if rg1.Exec(script) then begin
      ks.Values[rg1.Match[1]] := rg1.Match[2];
      while rg1.ExecNext do
        ks.Values[rg1.Match[1]] := rg1.Match[2];
    end;
    if rg2.Exec(script) then
      initstr := rg2.Match[1];
  finally
    rg1.Free;
    rg2.Free;
    cs.Leave;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  s, rurl, script: String;
  i, cnt: Integer;
  a: TStringArray;
  t: TStringList;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    rurl := MaybeFillHost(Module.RootURL, AURL);
    if not GET(rurl) then Exit;

    Result := True;
    with TXQueryEngineHTML.Create(Document) do
      try
        cnt := XPathCount('//ul[@id="dropdown-menu-page"]/li/a');
        s := XPathString('//script[contains(.,"imgsrcs")]');
      finally
        Free;
      end;

    if s = '' then Exit;
    if Pos('imgsrcs = new Array', s) <> 0 then
      s := GetString(s, '(', ')')
    else
      s := GetString(s, ' = ''', ''';');
    s := StringReplace(s, '''', '', [rfReplaceAll]);
    script := XPathString('//script[contains(@src, "chapter.js")]/@src', Document);

    Reset;
    Headers.Values['Referer'] := rurl;
    if GET(script) then begin
      t := TStringList.Create;
      try
        t.LoadFromStream(Document);
        script := DecryptScript(t.Text);
        if Pos(',', s) = 0 then
          s := AESDecryptCBCMD5Base64ZerosPadding(s, GetKey(script), GetIV(script));
        FillK(script);
      finally
        t.Free;
      end;
    end;

    a := s.Split([',']);
    for i := 0 to Math.min(cnt - 1, Length(a) - 1) do
      PageLinks.Add(SeparateRight(a[i], '//'));
  end;
end;

function BeforeDownloadImage(const DownloadThread: TDownloadThread;
  var AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := True;
  if DownloadThread = nil then Exit;
  DownloadThread.FHTTP.Headers.Values['Referer'] := ' ' + Module.RootURL;
end;

function DeScramble(AURL: String; image: TPicture): TPicture;
var
  a: TStringArray;
  i, size, heightnum, aint: Integer;
  sw, h, j, y, rxpos, py, canvasx: float;
  r: TPicture;
  rect1, rect2: TRect;
  tmp: String;
begin
  tmp := initstr;
  for i := 0 to ks.Count - 1 do
    if Pos(ks.Names[i], AURL) > 0 then begin
      tmp := ks.Values[ks.Names[i]];
      Break;
    end;

  a := tmp.Split(['a']);
  r := TPicture.Create;
  r.Bitmap.SetSize(image.Width, image.Height);

  size := 9;
  heightnum := 9;
  sw := float(image.Width) / size;
  h := float(image.Height) / heightnum;
  for i := 0 to size * heightnum - 1 do begin
    aint := StrToIntDef(a[i], 0);
    j := Floor(float(aint) / float(heightnum));
    y := j * h;
    rxpos := (aint - j * size) * sw;
    j := Floor(float(i) / float(size));
    py := j * h;
    canvasX := (i - j * size) * sw;
    rect1 := Rect(Trunc(rxpos), Trunc(y), Trunc(rxpos) + Trunc(sw), Trunc(y) + Trunc(h));
    rect2 := Rect(Trunc(canvasX), Trunc(py), Trunc(canvasX) + Trunc(sw), Trunc(py) + Trunc(h)) ;
    r.Bitmap.Canvas.CopyRect(rect1, image.Bitmap.Canvas, rect2);
  end;

  Result := r;
end;

function DownloadImage(const DownloadThread: TDownloadThread;
    const AURL: String; const Module: TModuleContainer): Boolean;
var
  image, r: TPicture;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  if DownloadThread.FHTTP.GET(AURL) then begin
    if (Pos('mangapicgallery.com/r/cspiclink', LowerCase(AURL))) = 0 then begin
      Result := True;
      Exit;
    end;
    image := TPicture.Create;
    r := nil;
    try
      image.LoadFromStream(DownloadThread.FHTTP.Document);
      r := DeScramble(AURL, image);
      DownloadThread.FHTTP.Document.Position := 0;
      r.SaveToStreamWithFileExt(DownloadThread.FHTTP.Document, 'jpg');
      Result := True;
    finally
      r.Free;
      image.Free;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaGo';
    RootURL := 'http://www.mangago.me';
    Category := 'English';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnBeforeDownloadImage := @BeforeDownloadImage;
    OnDownloadImage := @DownloadImage;
  end;
end;

initialization
  cs := TCriticalSection.Create;
  initstr := '';
  ks := TStringList.Create;
  RegisterModule;

finalization
  ks.Free;
  cs.Free;

end.
