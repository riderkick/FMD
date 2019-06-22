unit BlogTruyen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML;

implementation

uses RegExpr, synacode;

const
  dirurl = '/ajax/Search/AjaxLoadListManga?key=tatca&orderBy=1&p=';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer; const WorkPtr: Integer;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.XHR(Module.RootURL + dirurl + '1') then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(XPathString('//*[@class="paging"]/span[last()]/a/@href/substring-before(substring-after(.,"("),")")'), 1);
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.XHR(Module.RootURL + dirurl + IncStr(AURL)) then
  begin
    Result := NO_ERROR;
    XPathHREFAll('//*[@class="list"]//span/a', MangaInfo.FHTTP.Document, ALinks, ANames);
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
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do try
        coverLink := XPathString('//*[@class="thumbnail"]/img/@src');
        if title = '' then title := XPathString('//title/substring-before(.," | BlogTruyen")');
        authors := XPathString('//*[@class="description"]/p[starts-with(.,"Tác giả")]/string-join(.//a,", ")');
        genres := XPathString('//*[@class="description"]/p[starts-with(.,"Thể loại")]/string-join(.//a,", ")');
        summary := XPathString('//*[@class="detail"]/*[@class="content"]');
        XPathHREFAll('//*[@id="list-chapters"]//span[@class="title"]/a', chapterLinks, chapterName);
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
  i: Integer;
  image: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      XPathStringAll('//article[@id="content"]/img[not(contains(@src,"credit"))]/@src', Document, PageLinks);
      // http://blogtruyen.com/scripts/image-url-filter.js?ver=1
      with TRegExpr.Create do try
        ModifierI := True;
        for i := 0 to PageLinks.Count - 1 do
        begin
          image := PageLinks[i];
          image := DecodeURL(image);
          Expression := '^.+(&|\?)url=';
          image := Replace(image, '', True);
          Expression := '(https?:\/\/)lh(\d)(\.bp\.blogspot\.com)';
          image := Replace(image, '$1$2$3', True);
          Expression := '(https?:\/\/)lh\d\.(googleusercontent|ggpht)\.com';
          image := Replace(image, '$1\4.bp.blogspot.com', True);
          Expression := '\?.+$';
          image := Replace(image, '', False);
          if Pos('blogspot.com', image) <> 0 then begin
            Expression := '\/([^\/]+\-)?(Ic42)(\-[^\/]+)?\/';
            image := Replace(image, '/$2/', True);
            Expression := '\/(((s|w|h)\d+|(w|h)\d+\-(w|h)\d+))?\-?(c|d|g)?\/([^\/]+$)';
            image := Replace(image, '/$1', True);
            image += '?imgmax=0';
          end;
          if Pos('i.imgur.com', image) <> 0 then begin
            Expression := '(\/)(\w{5}|\w{7})(s|b|t|m|l|h)(\.(jpe?g|png|gif))$';
            image := Replace(image, '$1$2$4', True);
          end;
          if (Pos('i.imgur.com', image) <> 0) or (Pos('manga.truyentranh8.', image) <> 0) then begin
            image := 'http://images-focus-opensocial.googleusercontent.com/gadgets/proxy?container=focus&url=' + image;
          end;
          image := image.Replace('https://', 'http://');
          PageLinks[i] := image;
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
    Website := 'BlogTruyen';
    RootURL := 'http://blogtruyen.com';
    Category := 'Vietnamese';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
