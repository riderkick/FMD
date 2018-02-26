unit MangaSaurus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, RegExpr;

implementation

const
  dirurl = '/browse/added/';
  domainImage = 'http://img.mangasaurus.com';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '1') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        s := XPathString('//span[@class="pager__jumpLast"]/a/@href');
        if s <> '' then
        begin
          s := ReplaceRegExpr('^.+/(\d+$)', s, '$1', True);
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
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + IncStr(AURL)) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//span[@class="comicInfo__comicTitle"]/a') do
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
  v: IXQValue;
  s: String;
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
          coverLink := XPathString('//div[@class="gallery-info__cover"]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//h1[@class="title"]');
          authors := XPathStringAll(
            '//table[@class="table--info"]/tbody/tr/td[.="Author"]/following-sibling::td/ul[@class="tag-list"]/li/a');
          artists := XPathStringAll(
            '//table[@class="table--info"]/tbody/tr/td[.="Artist"]/following-sibling::td/ul[@class="tag-list"]/li/a');
          genres := XPathStringAll(
            '//table[@class="table--info"]/tbody/tr/td[.="Genres" or .="Contents" or .="Category"]/following-sibling::td/ul[@class="tag-list"]/li/a');
          summary := XPathString(
            '//table[@class="table--info"]/tbody/tr/td[.="Description"]/following-sibling::td');
          s := XPathString('//table[@class="table--info"]/tbody/tr/td[.="Status"]/following-sibling::td/span');
          if s <> '' then
          begin
            s := LowerCase(s);
            if Pos('ongoing', s) <> 0 then
              status := MangaInfo_StatusOngoing
            else if Pos('completed', s) <> 0 then
              status := MangaInfo_StatusCompleted;
          end;
          for v in XPath('//table[@class="table--data table--chapters js-chapterList"]/tbody/tr/td/a') do
          begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(XPathString('text()', v.toNode));
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
  source: TStringList;
  v: IXQValue;
  i: Integer;
  comicA, imagesA, slug, key, ext: String;
  images: array of String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      comicA := '';
      imagesA := '';
      source := TStringList.Create;
      try
        source.LoadFromStream(Document);
        if source.Count > 0 then
          for i := 0 to source.Count - 1 do
          begin
            if Pos('Mangasaurus.ImageReader.setComic(', source[i]) <> 0 then
              comicA := TrimRightChar(Trim(SeparateRight(source[i], '{')), [')', ';'])
            else
            if Pos('Mangasaurus.ImageReader.setImages(', source[i]) <> 0 then
            begin
              imagesA := TrimRightChar(Trim(SeparateRight(source[i], '{')), [')', ';']);
              Break;
            end;
          end;
      finally
        source.Free;
      end;
      if (comicA <> '') and (imagesA <> '') then
        with TXQueryEngineHTML.Create('{' + comicA) do
          try
            slug := XPathString('json(*).slug');
            if slug <> '' then
            begin
              ParseHTML('{' + imagesA);
              SetLength(images, 0);
              for v in XPath('json(*)()') do
              begin
                SetLength(images, Length(images) + 1);
                images[High(images)] := v.toString;
              end;
              if Length(images) <> 0 then
              begin
                for i := Low(images) to High(images) do
                begin
                  key := XPathString('json(*)(' + images[i] + ').original.file');
                  if key <> '' then
                  begin
                    ext := SeparateRight(key, '.');
                    key := SeparateLeft(key, '.');
                    if ext <> '' then
                      PageLinks.Add(domainImage + '/original/' + key + '/' + slug + '-' + images[i] + '.' + ext);
                  end;
                end;
                SetLength(images, 0);
              end;
            end;
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
    Website := 'MangaSaurus';
    RootURL := 'http://mangasaurus.com';
    Category := 'English';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
