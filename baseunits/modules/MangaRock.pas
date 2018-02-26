unit MangaRock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, webp, MemBitmap, Math, xquery;

implementation

var
  mangaList: TStringList = nil;

const
  ModuleApiUrl: String = 'https://api.mangarockhd.com';
  DirRequest: String = '{"status":"all","genres":{},"rank":"all","order":"name"}';
  PerPage = 500;

function DecryptImage(const imageData: TStream): TMemoryStream;
var
  n, i: Integer;
  tmp: TMemoryStream;
begin
  n := imageData.Size + 7;
  tmp := TMemoryStream.Create;

  // RIFF header
  tmp.WriteByte(82);
  tmp.WriteByte(73);
  tmp.WriteByte(70);
  tmp.WriteByte(70);

  // image size
  tmp.WriteByte(n and $FF);
  tmp.WriteByte((n shr 8) and $FF);
  tmp.WriteByte((n shr 16) and $FF);
  tmp.WriteByte((n shr 24) and $FF);

  // WEBPVP8 header
  tmp.WriteByte(87);
  tmp.WriteByte(69);
  tmp.WriteByte(66);
  tmp.WriteByte(80);
  tmp.WriteByte(86);
  tmp.WriteByte(80);
  tmp.WriteByte(56);

  for i := 0 to imageData.Size - 1 do
    tmp.WriteByte(imageData.ReadByte xor 101);

  tmp.Seek(0, TSeekOrigin.soBeginning);
  Result := tmp;
end;

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  mangaId, name: String;
  query: TXQueryEngineHTML;
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  mangaId := TrimRightChar(SeparateRight(AURL, 'manga/'), ['/']);
  if MangaInfo.FHTTP.GET(ModuleApiUrl + '/query/web400/info?oid=' + mangaId) then
  begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document);
    with MangaInfo.mangaInfo do
    begin
      title := query.XPathString('json(*).data.name');
      coverLink := query.XPathString('json(*).data.thumbnail');
      summary := query.XPathString('json(*).data.description');

      genres := '';
      for v in query.XPath('json(*).data.rich_categories().name') do
        AddCommaString(genres, v.toString());

      authors := '';
      artists := '';
      for v in query.XPath('json(*).data.authors()') do
      begin
        name := v.getProperty('name').toString();
        if CompareText(v.getProperty('role').toString(), 'art') = 0 then
          AddCommaString(artists, name)
        else
          AddCommaString(authors, name)
      end;

      status := MangaInfoStatusIfPos(query.XPathString('json(*).data.completed'), 'false', 'true');

      for v in query.XPath('json(*).data.chapters()') do
      begin
        chapterName.Add(v.getProperty('name').toString());
        chapterLinks.Add(v.getProperty('oid').toString());
      end;
    end;
    query.Free;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  chapterId: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  DownloadThread.Task.Container.PageLinks.Clear;
  DownloadThread.Task.Container.PageNumber := 0;
  chapterId := TrimLeftChar(AURL, ['/']);
  if DownloadThread.FHTTP.GET(ModuleApiUrl + '/query/web400/pages?oid=' + chapterId) then
  begin
    Result := True;
    XPathStringAll('json(*).data()',
      DownloadThread.FHTTP.Document,
      DownloadThread.Task.Container.PageLinks);
  end;
end;

function DownloadImage(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  decoded: TMemoryStream;
begin
  decoded := nil;
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP do
    if GET(AURL) then
      try
        decoded := DecryptImage(Document);
        Document.Size := decoded.Size;
        Document.Position := 0;
        decoded.SaveToStream(Document);
        Result := True;
      finally
        decoded.Free;
      end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  offset, i, ubound: Integer;
  request, name: String;
begin
  Result := NET_PROBLEM;
  if (MangaInfo = nil) or (mangaList = nil) then Exit(UNKNOWN_ERROR);

  offset := StrToInt(AURL) * PerPage;
  ubound := Min(offset + PerPage - 1, mangaList.Count - 1);
  request := '["' + mangaList[offset] + '"';
  for i := offset + 1 to ubound do
    request += ',"' + mangaList[i] + '"';
  request += ']';

  MangaInfo.FHTTP.MimeType := 'application/json';
  if MangaInfo.FHTTP.POST(ModuleApiUrl + '/meta', request) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for i := offset to ubound do begin
          name := XPathString('json(*).data("' + mangaList[i] + '").name');
          ANames.Add(name);
          ALinks.Add(Module.RootURL + '/manga/' + mangaList[i]);
        end;
      finally
        Free;
      end;
  end;
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  MangaInfo.FHTTP.MimeType := 'application/json';
  if MangaInfo.FHTTP.POST(ModuleApiUrl + '/query/web400/mrs_filter', DirRequest) then
  begin
    Result := NO_ERROR;
    mangaList.Clear;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        XPathStringAll('json(*).data()', mangaList);
        Page := mangaList.Count div PerPage;
        if mangaList.Count mod PerPage <> 0 then
           Inc(Page);
      finally
        Free;
      end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaRock';
    RootURL := 'https://mangarock.com';
    Category := 'English';
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnDownloadImage := @DownloadImage;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
  end;
end;

initialization
  mangaList := TStringList.Create;
  RegisterModule;

finalization
  mangaList.Free;

end.

