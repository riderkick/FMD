unit MangaFox;

{$mode objfpc}{$H+}

interface

implementation

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, MangaFoxWatermark, JSUtils, synautil;

var
  removewatermark: Boolean = True;
  saveaspng: Boolean = False;

resourcestring
  RS_RemoveWatermark = 'Remove watermark';
  RS_SaveAsPNG = 'Save as PNG';

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  s: string;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL + '/directory/' + IncStr(AURL) + '.html?az';
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    XPathHREFtitleAll('//ul[contains(@class, "manga-list")]/li/a', MangaInfo.FHTTP.Document, ALinks, ANames);
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do begin
    url := MaybeFillHost(Module.RootURL, AURL);
    Cookies.Values['isAdult'] := '1';
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//img[@class="detail-info-cover-img"]/@src'));
          if title = '' then title := XPathString('//span[@class="detail-info-right-title-font"]');
          authors := XPathStringAll('//p[@class="detail-info-right-say"]/a');
          genres := XPathStringAll('//p[@class="detail-info-right-tag-list"]/a');
          summary := XPathString('//p[@class="fullcontent"]');
          status := MangaInfoStatusIfPos(XPathString('//span[@class="detail-info-right-title-tip"]'));
          for v in XPath('//ul[@class="detail-main-list"]/li/a') do
          begin
            chapterLinks.Add(StringReplace(v.toNode.getAttribute('href'), '1.html', '', [rfReplaceAll]));
            chapterName.Add(XPathString('./div/p[@class="title3"]', v));
          end;
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  s, key, cid: string;
  query: TXQueryEngineHTML;
  page: Integer;
  lst: TStringList;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    Cookies.Values['isAdult'] := '1';
    if GET(MaybeFillHost(Module.RootURL, AURL) + '1.html') then begin
      Result := True;
      query := TXQueryEngineHTML.Create(Document);
      try
        s := query.XPathString('//script[contains(., "eval")]');
        s := 'var $=function(){return{val:function(){}}},newImgs,guidkey;' + s;
        s := s + ';newImgs||guidkey;';
        key := ExecJS(s);
        if Length(key) > 16 then
          PageLinks.CommaText := key
        else if Length(key) > 0 then begin
          s := query.XPathString('//script[contains(., "chapterid")]');
          cid := Trim(ReplaceString(GetBetween('chapterid', ';', s), '=', ''));
          PageNumber := StrToIntDef(Trim(ReplaceString(GetBetween('imagecount', ';', s), '=', '')), 0);
          page := 1;
          while page <= PageNumber do begin
            Reset;
            Headers.Values['Pragma'] := 'no-cache';
            Headers.Values['Cache-Control'] := 'no-cache';
            Headers.Values['Referer'] := MaybeFillHost(Module.RootURL, AURL) + '1.html';
            s := 'chapterfun.ashx?cid=' + cid + '&page=' + IntToStr(page) + '&key=' + key;
            if XHR(MaybeFillHost(Module.RootURL, AURL) + s) then begin
              s := Trim(StreamToString(Document));
              if s <> '' then begin
                s := ExecJS(s + ';d;');
                lst := TStringList.Create;
                try
                  lst.CommaText := s;
                  PageLinks.AddStrings(lst);
                  page := PageLinks.Count + 1;
                finally
                  lst.Free;
                end;
              end else begin
                key := '';
                Sleep(200);
                Continue;
              end;
            end;
            if PageLinks.Count >= PageNumber then Break;
            Sleep(3000);
          end;
        end;
      finally
        query.Free;
      end;
    end;
  end;
end;

function AfterImageSaved(const AFilename: String; const Module: TModuleContainer): Boolean;
begin
  Result := True;
  if removewatermark then
    Result := MangaFoxWatermark.RemoveWatermark(AFilename, saveaspng);
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/directory/?az') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(XPathString('//div[@class="pager-list"]//a[last()-1]'), 1);
      finally
        Free;
      end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaFox';
    RootURL := 'http://fanfox.net';
    Category := 'English';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnAfterImageSaved := @AfterImageSaved;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    AddOptionCheckBox(@removewatermark, 'RemoveWatermark', @RS_RemoveWatermark);
    AddOptionCheckBox(@saveaspng, 'SaveAsPNG', @RS_SaveAsPNG);
  end;
end;

initialization
  RegisterModule;

end.
