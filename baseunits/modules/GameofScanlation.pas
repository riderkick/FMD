unit GameofScanlation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  RegExpr, synautil;

implementation

const
  dirurl='/forums/projects-releases.9/';

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL+dirurl) then begin
    Result:=NO_ERROR;
    query:=TXQueryEngineHTML.Create;
    try
      query.ParseHTML(MangaInfo.FHTTP.Document);
      for v in query.XPath('//h4/a[@class="menuRow"]') do begin
        ALinks.Add(v.toNode.getAttribute('href'));
        ANames.Add(v.toString);
      end;
    finally
      query.Free;
    end;
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const AURL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  i, p: Integer;
  rurl: String;

  procedure GetChapters;
  begin
    for v in query.XPath('//*[@class="discussionListItems"]//div[@class="listBlock main"]/div/h3/a') do begin
      MangaInfo.mangaInfo.chapterLinks.Add(v.toNode.getAttribute('href'));
      MangaInfo.mangaInfo.chapterName.Add(v.toString);
    end;
  end;

begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP,MangaInfo.mangaInfo do begin
    rurl:=AppendURLDelim(FillHost(Module.RootURL,AURL));
    if GET(rurl) then begin
      Result:=NO_ERROR;
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(Document);
        if title=''then title:=query.XPathString('//div[@class="titleBarContent"]/h1');
        GetChapters;
        p:=StrToIntDef(query.XPathString('(//div[@class="PageNav"])[1]/@data-end'),1);
        if p>1 then
          for i:=2 to p do
            if GET(rurl+'page-'+IntToStr(i)) then begin
              query.ParseHTML(Document);
              GetChapters;
            end;
        InvertStrings([chapterLinks,chapterName]);
      finally
        query.Free;
      end;
    end;
  end;
end;

function GetPageNumber(var DownloadThread: TDownloadThread; const AURL: String;
  Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  s: RegExprString;
begin
  Result:=False;
  if DownloadThread=nil then Exit;
  with DownloadThread.FHTTP,DownloadThread.manager.container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    s:=ReplaceRegExpr('/\?\w+.*$',AURL,'/',False);
    s:=AppendURLDelim(FillHost(Module.RootURL,s))+'?chapter_view=fullstrip';
    if GET(s) then begin
      Result:=True;
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(Document);
        for v in query.XPath('//div[@id="comicMainImage"]/a/img/@src') do
          PageLinks.Add(MaybeFillHost(Module.RootURL,v.toString));
        PageNumber:=query.XPath('//select[@id="ctrl_chapter_page"]/option').Count;
      finally
        query.Free;
      end;
    end;
  end;
end;

function GetImageURL(var DownloadThread: TDownloadThread; const AURL: String;
  Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
  s: String;
begin
  Result:=False;
  if DownloadThread=nil then Exit;
  with DownloadThread.manager.container,DownloadThread.FHTTP do begin
    s:=AppendURLDelim(AURL);
    if DownloadThread.workCounter>0 then s+='?comic_page='+IncStr(DownloadThread.workCounter);
    if GET(FillHost(Module.RootURL,s)) then begin
      Result:=True;
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(Document);
        s:=query.XPathString('//img[@id="comicMainImage"]/@src');
        if s<>'' then begin
          s:=MaybeFillHost(Module.RootURL,s);
          PageLinks[DownloadThread.workCounter]:=s;
        end;
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
    Website:='GameofScanlation';
    RootURL:='https://gameofscanlation.moe';
    OnGetNameAndLink:=@GetNameAndLink;
    OnGetInfo:=@GetInfo;
    OnGetPageNumber:=@GetPageNumber;
    OnGetImageURL:=@GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
