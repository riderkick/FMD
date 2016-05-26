unit GMangaMe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/mangas';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        s := XPathString('//ul[starts-with(@class,"pagination")]/li[last()-1]/a/@href');
        if Pos('page=', s) <> 0 then
        begin
          s := SeparateRight(s, 'page=');
          Page := StrToIntDef(s, 1);
        end;
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
  if AURL <> '0' then
    s := s + '?&page=' + IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//dd[@class="small-dd"]/a') do
        begin
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
  v, x: IXQValue;
  s, t: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//div/img[starts-with(@class,"img-responsive")]/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//div[@class="section-title en-text-center"]');
          summary := XPathString('//div/dl[starts-with(@class,"dl-horizontal")]/dd[contains(@class,"summary")]');
          authors := XPathString('//div/dl[starts-with(@class,"dl-horizontal")]/dt[.="المؤلفون"]/following-sibling::dd[1]');
          artists := XPathString('//div/dl[starts-with(@class,"dl-horizontal")]/dt[.="الرسامون"]/following-sibling::dd[1]');
          genres := XPathStringAll('//div/dl[starts-with(@class,"dl-horizontal")]/dt[.="التصنيف"]/following-sibling::dd[1]/span');
          status := MangaInfoStatusIfPos(XPathString('//div/dl[starts-with(@class,"dl-horizontal")]/dt[ends-with(.,"الـمانهوا")]/following-sibling::dd[1]'),
            'مستمرة',
            'منتهية');
          for v in XPath('//table[1]/tbody/tr') do
          begin
            s := XPathString('td/div[@class="c_edit"]', v.toNode);
            for x in XPath('td/span[@dir="ltr"]/a', v.toNode) do
            begin
              chapterLinks.Add(x.toNode.getAttribute('href'));
              t := x.toString;
              if t <> '' then
                t := ' [' + t + ']';
              chapterName.Add(s + t);
            end;
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
  source: TStringList;
  i: Integer;
  images: String;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      images := '';
      source := TStringList.Create;
      try
        source.LoadFromStream(Document);
        if source.Count > 0 then
          for i := 0 to source.Count - 1 do
            if Pos('chapterImgs = [', source[i]) <> 0 then
            begin
               images := SeparateRight(source[i], '[');
               Break;
            end;
      finally
        source.Free;
      end;
      if images <> '' then
        with TXQueryEngineHTML.Create('[' + images) do
          try
            for v in XPath('json(*)()') do
              PageLinks.Add(v.toString);
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
    Website := 'GManga';
    RootURL := 'http://gmanga.me';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
