unit MintMangaRU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, FMDOptions, httpsendthread, synautil;

implementation

const
  dirurl = '/list?sortType=created';
  perpage = 70;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        s := XPathString('//*[@class="pagination"]/a[@class="step"][last()]/@href');
        if s <> '' then Page := StrToIntDef(GetBetween('offset=', '&', s), 1);
        if Page > 1 then
          Page := ceil(Page / perpage) + 1;
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
  s := Module.RootURL + dirurl;
  if AURL <> '0' then s += '&offset=' + IntToStr(StrToInt(AURL) * perpage) + '&max=' + IntToStr(perpage);
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//*[@class="tiles row"]/div/div[@class="desc"]/h3/a') do begin
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
  v: IXQValue;
  rname, s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//*[@class="picture-fotorama"]/img/@src');
          rname := XPathString('//h1[@class="names"]/span[@class="name"]');
          if title = '' then title := XPathString('//h1[@class="names"]/span[@class="eng-name"]');
          if title = '' then title := rname;
          authors := Trim(SeparateRight(
            XPathString('//*[starts-with(@class,"subject-meta")]/*[starts-with(.,"Автор")]'), ':'));
          genres := Trim(SeparateRight(
            XPathString('//*[starts-with(@class,"subject-meta")]/*[starts-with(.,"Категория")]'), ':'));
          for v in XPath('//p[@class="elementList"]/span[starts-with(@class,"elem_genre")]') do
            AddCommaString(genres, v.toString);
          s := XPathString('//*[starts-with(@class,"subject-meta")]/*[starts-with(.,"Перевод")]');
          if s <> '' then begin
            if Pos('продолжается', s) > 0 then status := '1'
            else status := '0';
          end;
          summary := XPathString('//*[@class="manga-description"]');
          for v in XPath('//table[@class="table table-hover"]/tbody/tr/td/a') do begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            s := CleanString(v.toString);
            if OptionRemoveMangaNameFromChapter and (rname <> '') then
              if Pos(rname, LowerCase(s)) = 1 then s := Trim(StringReplace(s, rname, '', [rfIgnoreCase]));
            chapterName.Add(s);
          end;
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  i, j, x: Integer;
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    s := AURL;
    if Pos('mature=1', LowerCase(s)) = 0 then s += '?mature=1';
    if GET(FillHost(Module.RootURL, s)) then begin
      Result := True;
      Source := TStringList.Create;
      try
        Source.LoadFromStream(Document);
        if Source.Count > 0 then begin
          s := '';
          for i := 0 to Source.Count - 1 do
            if Pos('rm_h.init(', Source[i]) > 0 then begin
              s := Trim(Source[i]);
              Break;
            end;
          if s <> '' then begin
            s := GetBetween('[[', ']]', Source[i]);
            s := StringReplace(s, '[', '', [rfReplaceAll]);
            s := StringReplace(s, ']', '', [rfReplaceAll]);
            s := StringReplace(s, '"', '', [rfReplaceAll]);
            s := StringReplace(s, '''', '', [rfReplaceAll]);
            Source.CommaText := s;
            if (Source.Count > 0) and (frac(Source.Count / 5) = 0) then begin
              j := Source.Count div 5;
              for i := 0 to j - 1 do begin
                x := i * 5;
                PageLinks.Add(Source[x + 1] + Source[x] + Source[x + 2]);
              end;
            end;
          end;
        end;
      finally
        Source.Free;
      end;
    end;
  end;
end;

function BeforeDownloadImage(const DownloadThread: TDownloadThread;
  var AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := True;
  if DownloadThread = nil then Exit;
  DownloadThread.FHTTP.Headers.Values['Referer'] := ' ' + Module.RootURL;
end;

procedure RegisterModule;

  function AddWebsiteModule(AWebsite, ARootURL: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do begin
      Website := AWebsite;
      RootURL := ARootURL;
      Category := 'Russian';
      SortedList := True;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnBeforeDownloadImage := @BeforeDownloadImage;
    end;
  end;

begin
  AddWebsiteModule('MintMangaRU', 'http://mintmanga.com');
  AddWebsiteModule('ReadMangaRU', 'http://readmanga.me');
  AddWebsiteModule('SelfMangaRU', 'http://selfmanga.ru');
end;

initialization
  RegisterModule;

end.
