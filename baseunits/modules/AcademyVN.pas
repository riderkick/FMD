unit AcademyVN;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/manga/all';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        page := StrToIntDef(XPathString('(//*[starts-with(@class,"pagination")]//a)[last()-1]'), 1);
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
  if AURL <> '0' then s += '?page=' + IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//table[1]/tbody/tr/td[1]/a') do begin
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
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//*[@class="__image"]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//*[@class="__info"]/h3');
          authors := SeparateRight(XPathString('//*[@class="__info"]/p[starts-with(.,"Tác giả:")]'), ':');
          genres := SeparateRight(XPathString('//*[@class="__info"]/p[starts-with(.,"Thể loại:")]'), ':');
          s := XPathString('//*[@class="__info"]/p[starts-with(.,"Tình trạng:")]');
          if s <> '' then begin
            if (Pos('Đang tiến hành', s) > 0) or (Pos('Ngưng', s) > 0) then status := '1'
            else status := '0';
          end;
          summary := XPathString('//*[@class="__info"]/*[@class="__description"]');
          for v in XPath('//*[@class="table-scroll"]/table/tbody/tr/td[1]/a') do begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(v.toString);
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
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//*[@class="manga-container"]/img/@src') do
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
    Website := 'AcademyVN';
    RootURL := 'http://truyen.academyvn.com';
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
