unit Tsumino;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, synautil;

implementation

const
  dirurl='/Browse/Index/1/';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
begin
  Result:=NET_PROBLEM;
  Page:=1;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL+dirurl+'10000000') then begin
    Result:=NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        page:=StrToIntDef(XPathString('//ul[@class="pagination"]/li[last()-1]'),1);
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
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  s:=Module.RootURL;
  if AURL<>'0' then s:=s+dirurl+IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then begin
    Result:=NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//div[@class="row row-no-padding"]//div[@class="overlay"]') do begin
          ALinks.Add(XPathString('a/@href',v.toNode));
          ANames.Add(XPathString('div/div[@class="overlay-title"]',v.toNode));
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  i: Integer;
  v: IXQValue;
const
  g: array[0..2] of String = ('Parody','Characters','Tags');
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP,MangaInfo.mangaInfo do begin
    url:=FillHost(Module.RootURL,AURL);
    if GET(url) then begin
      Result:=NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink:=XPathString('//img[@class="book-page-image img-responsive"]/@src');
          if coverLink<>'' then coverLink:=MaybeFillHost(Module.RootURL,coverLink);
          if title=''then title:=XPathString('//div[@class="book-line"][starts-with(.,"Title")]/div[@class="book-data"]');
          artists:=XPathString('//div[@class="book-line"][starts-with(.,"Artist")]/div[@class="book-data"]');
          genres:='';
          for i:=Low(g) to High(g) do
            for v in XPath('//div[@class="book-line"][starts-with(.,"'+g[i]+'")]/div[@class="book-data"]/*') do
              AddCommaString(genres,v.toString);
          if title<>'' then begin
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
  v: IXQValue;
  s: String;
begin
  Result:=False;
  if DownloadThread=nil then Exit;
  with DownloadThread.FHTTP,DownloadThread.manager.container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL,AURL)) then begin
      Result:=True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//div[@class="no-border book-grid-item"]/a/img/@data-original') do begin
            s:=Trim(v.toString);
            if s<>'' then begin
              s:=StringReplace(s,'/Thumb/','/Image/',[rfIgnoreCase]);
              PageLinks.Add(MaybeFillHost(Module.RootURL,s));
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
    Website:='Tsumino';
    RootURL:='http://www.tsumino.com';
    OnGetDirectoryPageNumber:=@GetDirectoryPageNumber;
    OnGetNameAndLink:=@GetNameAndLink;
    OnGetInfo:=@GetInfo;
    OnGetPageNumber:=@GetPageNumber;
    SortedList:=True;
  end;
end;

initialization
  RegisterModule;

end.
