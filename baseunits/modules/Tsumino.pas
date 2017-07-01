unit Tsumino;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil,synacode, RegExpr;

implementation

const
  //dirurl = '/Browse/Index/1/';
  // '/?pageNumber=532&RawSearch=&SortOptions=Newest&PageMinimum=1&PageMaximum=10000&RateMinimum=0&RateMaximum=5'
  dirurl = '/Browse/Query';
  dirurldata = 'pageNumber=';
  dirurldataend = '&RawSearch=&SortOptions=Newest&PageMinimum=1&PageMaximum=10000&RateMinimum=0&RateMaximum=5';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.POST(Module.RootURL + dirurl, dirurldata + '1' + dirurldataend) then
  begin
    Result := NO_ERROR;
    Page := StrToIntDef(XPathString('json(*)("PageCount")', MangaInfo.FHTTP.Document), 1);
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
  if MangaInfo.FHTTP.POST(Module.RootURL + dirurl,
    dirurldata + IncStr(AURL) + dirurldataend) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        s := XPathString('json(*)("Data")');
        if s <> '' then
        begin
          ParseHTML(s);
          for v in XPath('//div[@class="overlay"]/a/@href') do
            ALinks.Add(v.toString);
          for v in XPath('//div[@class="overlay"]/div[@class="overlay-data"]/div[@class="overlay-title"]') do
            ANames.Add(v.toString);
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//img[@class="book-page-image img-responsive"]/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString(
              '//div[@class="book-line"][starts-with(.,"Title")]/div[@class="book-data"]');
          artists := XPathString('//div[@class="book-line"][starts-with(.,"Artist")]/div[@class="book-data"]');
          genres := XPathStringAll(
            '//div[@class="book-line"][starts-with(.,"Parody") or starts-with(.,"Characters") or starts-with(.,"Tags")]/div[@class="book-data"]/*');
          if title <> '' then begin
            chapterLinks.Add(url);
            chapterName.Add(title);
          end;
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  bookid: String;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    bookid := RegExprGetMatch('(?i)/info/(\d+)/?',AURL,1);
    Headers.Values['Referer'] := ' ' + Module.RootURL + '/Read/View/' + bookid;
    if POST(Module.RootURL + '/Read/Load', 'q=' + bookid) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('json(*).reader_page_urls()') do
            PageLinks.Add(Module.RootURL + '/Image/Object?name=' + EncodeURLElement(v.toString));
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Tsumino';
    RootURL := 'http://www.tsumino.com';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    SortedList := True;
  end;
end;

initialization
  RegisterModule;

end.
