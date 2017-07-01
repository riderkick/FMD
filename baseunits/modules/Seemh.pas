unit Seemh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  RegExpr, synautil;

implementation

const
  dirurl='/list/';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL+dirurl) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      Page:=StrToIntDef(query.XPathString('//div[@class="result-count"]/strong[2]'),1);
    finally
      query.Free;
    end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  s: String;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  s:=Module.RootURL+dirurl;
  if AURL<>'0' then s+='/index_p'+IncStr(AURL)+'.html';
  if MangaInfo.FHTTP.GET(s) then begin
    Result:=NO_ERROR;
    query:=TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//ul[@id="contList"]/li/p/a') do begin
        ALinks.Add(v.toNode.getAttribute('href'));
        ANames.Add(v.toString);
      end;
    finally
      query.Free;
    end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP,MangaInfo.mangaInfo do begin
    if GET(FillHost(Module.RootURL,AURL)) then begin
      Result:=NO_ERROR;
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        coverLink:=query.XPathString('//p[@class="hcover"]/img/@src');
        if title=''then title:=query.XPathString('//div[@class="book-title"]/h1');
        genres:=SeparateRight(query.XPathString('//ul[@class="detail-list cf"]/li[2]/span[1]'),':');
        authors:=SeparateRight(query.XPathString('//ul[@class="detail-list cf"]/li[2]/span[2]'),':');
        if Pos('连载中',query.XPathString('//ul[@class="detail-list cf"]/li[@class="status"]'))>0 then status:='1' else status:='0';
        summary:=query.XPathString('//div[@id="intro-all"]');
        for v in query.XPath('//div[@id="chapter-list-0"]/ul/li/a') do begin
          chapterLinks.Add(v.toNode.getAttribute('href'));
          chapterName.Add(v.toNode.getAttribute('title'));
        end;
        InvertStrings([chapterLinks,chapterName]);
      finally
        query.Free;
      end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
begin
  Result:=False;
  if DownloadThread=nil then Exit;
  with DownloadThread.FHTTP,DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL,AURL)) then begin
      Result:=True;
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        for v in query.XPath('//div[@id="image"]/div/img[@class="real img-responsive"][@id!="imagem-forum"]/@data-lazy') do
          PageLinks.Add(v.toString);
      finally
        query.Free;
      end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website:='Seemh';
    RootURL:='http://www.seemh.com';
    OnGetDirectoryPageNumber:=@GetDirectoryPageNumber;
    OnGetNameAndLink:=@GetNameAndLink;
    OnGetInfo:=@GetInfo;
    OnGetPageNumber:=@GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
