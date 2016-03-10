unit Luscious;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML;

implementation

uses
  synautil, RegExpr;

const
  dirURL = '/c/-/albums/t/manga/sorted/new/page/';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
begin
  Page := 100;
  Result := NO_ERROR;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  v: IXQValue;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  Source := TStringList.Create;
  try
    if GetPage(MangaInfo.FHTTP, TObject(Source),
      Module.RootURL + dirURL + IncStr(AURL) + '/', 3) then
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          for v in Query.XPath('//*[@id="albums_wrapper"]//*[@class="caption"]//a') do begin
            ALinks.Add(TrimLeftChar(v.toNode.getAttribute('href'), ['.']));
            ANames.Add(v.toString);
          end;
        finally
          Query.Free;
        end;
      end
      else
        Result := INFORMATION_NOT_FOUND;
  finally
    Source.Free;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  info: TMangaInfo;
  v: IXQValue;
  s: String;
  regx: TRegExpr;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  info := MangaInfo.mangaInfo;
  info.website := Module.Website;
  info.url := AppendURLDelim(FillHost(Module.RootURL, AURL));
  Source := TStringList.Create;
  regx := TRegExpr.Create;
  try
    regx.ModifierI := True;
    regx.Expression := '/page/\d+/?$';
    if regx.Exec(info.url) then
      info.url := regx.Replace(info.url, '/', False);
    if RightStr(info.url, 5) <> 'view/' then info.url += 'view/';
    if Pos('/pictures/album/', info.url) > 0 then
      info.url := StringReplace(info.url, '/pictures/album/', '/albums/', []);
    if MangaInfo.FHTTP.GET(info.url, TObject(Source)) then
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          with info do begin
            coverLink := Query.XPathString('//*[@class="album_cover_item"]//img/@src');
            if coverLink <> '' then
              coverLink := TrimLeftChar(coverLink, ['/']);
            title := Query.XPathString('//*[@class="album_cover"]/h2');
            genres := '';
            for v in Query.XPath('//*[@id="tag_section"]//li') do begin
              s := v.toString;
              if LeftStr(s, 7) = 'author:' then
                authors := SeparateRight(s, ':')
              else if LeftStr(s, 7) = 'artist:' then
                artists := SeparateRight(s, ':')
              else
                AddCommaString(genres, s);
            end;
            //section, languge
            for v in Query.XPath('//*[@class="content_info"]//p/*') do
              AddCommaString(genres, v.toString);
            //chapter
            s := info.url;
            if RightStr(s, 5) = 'view/' then SetLength(s, Length(s) - 5);
            chapterLinks.Add(s);
            chapterName.Add(title);
          end;
        finally
          Query.Free;
        end;
      end
      else
        Result := INFORMATION_NOT_FOUND;
  finally
    regx.Free;
    Source.Free;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  Container: TTaskContainer;
  v: IXQValue;
  rurl: String;
  p: Integer;
  nextpage: Boolean;
  regx: TRegExpr;
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  with Container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    rurl := AppendURLDelim(FillHost(Module.RootURL, AURL));
    Source := TStringList.Create;
    regx:=TRegExpr.Create;
    try
      regx.ModifierI := True;
      regx.Expression := '/page/\d+/?$';
      if regx.Exec(rurl) then rurl := regx.Replace(rurl, '/', False);
      if RightStr(rurl, 5) = 'view/' then SetLength(rurl, Length(rurl) - 5);
      if RightStr(rurl, 5) <> 'page/' then rurl += 'page/';
      if Pos('/albums/', rurl) > 0 then
        rurl := StringReplace(rurl, '/albums/', '/pictures/album/', []);
      Query := TXQueryEngineHTML.Create;
      try
        regx.Expression := '\.\d+x\d+(\.\w+)$';
        p := 1;
        nextpage := True;
        while nextpage do begin
          nextpage := False;
          if GetPage(DownloadThread.FHTTP, TObject(Source), rurl + IntToStr(p) + '/',
            Manager.retryConnect) then
          begin
            Result := True;
            Query.ParseHTML(Source.Text);
            for v in Query.XPath('//*[@class="picture_page"]//img/@src') do begin
              s := TrimLeftChar(v.toString, ['/']);
              if regx.Exec(s) then s := regx.Replace(s, '$1', True);
              Container.PageLinks.Add(s);
            end;
            if Query.XPathString('//*[@id="next_page"]//a/@href') <> '' then begin
              Inc(p);
              nextpage := True;
            end;
          end;
        end;
      finally
        Query.Free;
      end;
    finally
      regx.Free;
      Source.Free;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Luscious';
    RootURL := 'http://luscious.net/';
    SortedList := True;
    FavoriteAvailable := False;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
