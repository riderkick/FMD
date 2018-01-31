unit MangaGo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, BaseCrypto, synautil, Graphics,
  Interfaces, Math, RegExpr, strutils;

implementation

var
  deskeys: TStringList;

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
          XPathHREFAll('//table[@id="chapter_table"]//td//a', chapterLinks, chapterName);
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
      Reset;
      Headers.Values['Referer'] := url;
    end;
  end;
end;

function DecryptScript(script: String): String;
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

function GetKey(script: String): String;
var
  rg: TRegExpr;
begin
  Result := '';
  rg := TRegExpr.Create('body\s+ready\s+(\S+)');
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

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  s, rurl, script: String;
  i: Integer;
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
    s := XPathString('//script[contains(.,"imgsrcs")]', Document);
    if s = '' then Exit;

    if Pos('imgsrcs = new Array', s) <> 0 then
      s := GetString(s, '(', ')')
    else
      s := GetString(s, ' = ''', ''';');
    s := StringReplace(s, '''', '', [rfReplaceAll]);

    if Pos(s, ',') = 0 then begin
      script := XPathString('//script[contains(@src, "chapter.js")]/@src', Document);
      Reset;
      Headers.Values['Referer'] := rurl;
      if GET(script) then begin
        t := TStringList.Create;
        try
          t.LoadFromStream(Document);
          script := DecryptScript(t.Text);
          s := AESDecryptCBCMD5Base64ZerosPadding(s, GetKey(script), GetIV(script));
        finally
          t.Free;
        end;
      end;
    end;

    a := s.Split([',']);
    for i := 0 to Length(a) - 1 do
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
  tmp: String = '18a72a69a64a13a1a43a3aa42a23a66a26a19a51a54a78a34a17a31a35a15a58a29a61a48a73a74a44a52a60a24a63a20a32a7a45a53a75a55a62a59a41a76a68a2a36a21a10a38a33a71a40a67a22a4a50a80a65a27a37a47a70a14a28a16a6a56a30a57a5a11a79a9a77a46a39a25a49a8a12';
  a: TStringArray;
  i, size, heightnum, aint: Integer;
  sw, h, j, y, rxpos, py, canvasx: float;
  r: TPicture;
  rect1, rect2: TRect;
begin
  for i := 0 to deskeys.Count - 1 do
    if Pos(deskeys.Names[i], AURL) > 0 then begin
      tmp := deskeys.Values[deskeys.Names[i]];
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
    if Pos('cspiclink', LowerCase(AURL)) = 0 then begin
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
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnBeforeDownloadImage := @BeforeDownloadImage;
    OnDownloadImage := @DownloadImage;
  end;
end;

initialization
  deskeys := TStringList.Create;
  deskeys.Values['60a2b0ed56cd458c4633d04b1b76b7e9'] := '18a72a69a64a13a1a43a3aa42a23a66a26a19a51a54a78a34a17a31a35a15a58a29a61a48a73a74a44a52a60a24a63a20a32a7a45a53a75a55a62a59a41a76a68a2a36a21a10a38a33a71a40a67a22a4a50a80a65a27a37a47a70a14a28a16a6a56a30a57a5a11a79a9a77a46a39a25a49a8a12';
  deskeys.Values['400df5e8817565e28b2e141c533ed7db'] := '61a74a10a45a3a37a72a22a57a39a25a56a52a29a70a60a67a41a63a55a27a28a43a18a5a9a8a40a17a48a44a79a38a47a32a73a4a6a13a34a33a49a2a42a50a76a54a36a35a14a58a7a69a46a16a30a21a11aa51a53a77a26a31a1a19a20a80a24a62a68a59a66a75a12a64a78a71a15a65a23';
  deskeys.Values['84ba0d8098f405b14f4dbbcc04c93bac'] := '61a26a35a16a55a10a72a37a2a60a66a65a33a44a7a28a70a62a32a56a30a40a58a15a74a47aa36a78a75a11a6a77a67a39a23a9a31a64a59a13a24a80a14a38a45a21a63a19a51a17a34a50a46a5a29a73a8a57a69a48a68a49a71a41a12a52a18a79a76a54a42a22a4a1a3a53a20a25a43a27';
  deskeys.Values['56665708741979f716e5bd64bf733c33'] := '23a7a41a48a57a27a69a36a76a62a40a75a26a2a51a6a10a65a43a24a1aa20a71a28a30a13a38a79a78a72a14a49a55a56a58a25a70a12a80a3a66a11a39a42a17a15a54a45a34a74a31a8a61a46a73a63a22a64a19a77a50a9a59a37a68a52a18a32a16a33a60a67a21a44a53a5a35a4a29a47';
  deskeys.Values['a67e15ed870fe4aab0a502478a5c720f'] := '8a12a59a52a24a13a37a21a55a56a41a71a65a43a40a66a11a79a67a44a33a20a72a2a31a42a29a34a58a60a27a48a28a15a35a51a76a80a0a63a69a53a39a46a64a50a75a1a57a9a62a74a18a16a73a14a17a6a19a61a23a38a10a3a32a26a36a54a4a30a45a47a70a22a7a68a49a77a5a25a78';
  deskeys.Values['b6a2f75185754b691e4dfe50f84db57c'] := '47a63a76a58a37a4a56a21a1a48a62a2a36a44a34a42a23a9a60a72a11a74a70a20a77a16a15a35a69a0a55a46a24a6a32a75a68a43a41a78a31a71a52a33a67a25a80a30a5a28a40a65a39a14a29a64a3a53a49a59a12a66a38a27a79a45a18a22a8a61a50a17a51a10a26a13a57a19a7a54a73';
  deskeys.Values['db99689c5a26a09d126c7089aedc0d86'] := '57a31a46a61a55a41a26a2a39a24a75a4a45a13a23a51a15a8a64a37a72a34a12a3a79a42a80a17a62a49a19a77a48a68a78a65a14a10a29a16a20a76a38a36a54a30a53a40a33a21a44a22a32a5a1a7a70a67a58a0a71a74a43a66a6a63a35a56a73a9a27a25a59a47a52a11a50a18a28a60a69';
  deskeys.Values['37abcb7424ce8df47ccb1d2dd9144b49'] := '67a45a39a72a35a38a61a11a51a60a13a22a31a25a75a30a74a43a69a50a6a26a16a49a77a68a59a64a17a56a18a1a10a54a44a62a53a80a5a23a48a32a29a79a24a70a28a58a71a3a52a42a55a9a14a36a73a34a2a27a57a0a21a41a33a37a76a8a40a65a7a20a12a19a47a4a78a15a63a66a46';
  deskeys.Values['874b83ba76a7e783d13abc2dabc08d76'] := '26a59a42a43a4a20a61a28a12a64a37a52a2a77a34a13a46a74a70a0a44a29a73a66a55a38a69a67a62a9a63a6a54a79a21a33a8a58a40a47a71a49a22a50a57a78a56a25a17a15a36a16a48a32a5a10a14a80a24a72a76a45a3a53a23a41a60a11a65a19a27a51a68a35a31a1a75a39a30a7a18';
  deskeys.Values['d320d2647d70c068b89853e1a269c609'] := '77a38a53a40a16a3a20a18a63a9a24a64a50a61a45a59a27a37a8a34a11a55a79a13a47a68a12a22a46a33a1a69a52a54a31a23a62a43a0a2a35a28a57a36a51a78a70a5a32a75a41a30a4a80a19a21a42a71a49a10a56a74a17a7a25a6a14a73a29a44a48a39a60a58a15a66a67a72a65a76a26';
  RegisterModule;

finalization
  deskeys.Free();

end.
