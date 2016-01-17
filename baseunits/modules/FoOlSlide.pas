unit FoOlSlide;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  RegExpr, synautil;

implementation

const
  dirurl='/directory/';
  yomangadirurl='/reader/directory/';

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if Module.Website='YoManga' then s:=yomangadirurl
  else s:=dirurl;
  if MangaInfo.FHTTP.GET(Module.RootURL+s) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      s:=query.XPathString('//div[@class="next"]/a[contains(text(),"Last")]/@href');
      if s<>'' then begin
        s:=ReplaceRegExpr('.*/(\d+)/$',s,'$1',True);
        Page:=StrToIntDef(s,1);
      end;
    finally
      query.Free;
    end;
  end;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  s: String;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  if Module.Website='YoManga' then s:=yomangadirurl
  else s:=dirurl;
  s:=Module.RootURL+s;
  if AURL<>'0' then s+=IncStr(AURL)+'/';
  if MangaInfo.FHTTP.GET(s) then begin
    Result:=NO_ERROR;
    query:=TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//div[@class="list series"]/div/div[@class="title"]/a') do begin
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
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP,MangaInfo.mangaInfo do begin
    if GET(FillHost(Module.RootURL,AURL)) then begin
      Result:=NO_ERROR;
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        coverLink:=query.XPathString('//div[@class="thumbnail"]/img/@src');
        if title=''then title:=query.XPathString('//h1[@class="title"]');
        authors:=TrimLeftChar(query.XPathString('//div[@class="info"]/*[contains(text(),"Author")]/following-sibling::text()[1]'),[':',' ']);
        artists:=TrimLeftChar(query.XPathString('//div[@class="info"]/*[contains(text(),"Artist")]/following-sibling::text()[1]'),[':',' ']);
        summary:=TrimLeftChar(query.XPathString('//div[@class="info"]/*[contains(text(),"Synopsis")]/following-sibling::text()[1]'),[':',' ']);
        for v in query.XPath('//div[@class="list"]//div[@class="title"]/a') do
        begin
          chapterLinks.Add(v.toNode.getAttribute('href'));
          if v.toNode.getAttribute('title') <> '' then
            chapterName.Add(v.toNode.getAttribute('title'))
          else chapterName.Add(v.toString);
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
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        PageNumber:=query.XPath('//div[@class="topbar_right"]//ul[@class="dropdown"]/li').Count;
        s:=query.XPathString('//script[contains(.,"var pages")]');
        if s<>'' then begin
          s:=GetBetween('var pages = ',';',s);
          try
            query.ParseHTML(s);
            for v in query.XPath('json(*)()("url")') do
              PageLinks.Add(v.toString);
          except
          end;
        end;
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
    s:=AURL;
    if DownloadThread.workCounter>0 then s:=AppendURLDelim(s)+'page/'+IncStr(DownloadThread.workCounter);
    if GET(FillHost(Module.RootURL,s)) then begin
      Result:=True;
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        PageLinks[DownloadThread.workCounter]:=query.XPathString('//div[@id="page"]//img/@src');
      finally
        query.Free;
      end;
    end;
  end;
end;

procedure RegisterModule;

  procedure AddWebsiteModule(AWebsite,ARootURL: String);
    begin
      with AddModule do
      begin
        Website:=AWebsite;
        RootURL:=ARootURL;
        OnGetDirectoryPageNumber:=@GetDirectoryPageNumber;
        OnGetNameAndLink:=@GetNameAndLink;
        OnGetInfo:=@GetInfo;
        OnGetPageNumber:=@GetPageNumber;
        OnGetImageURL:=@GetImageURL;
      end;
    end;

begin
  AddWebsiteModule('Shoujosense','http://reader.shoujosense.com');
  AddWebsiteModule('YoManga','http://yomanga.co');
  AddWebsiteModule('RawYoManga','http://raws.yomanga.co');
end;

initialization
  RegisterModule;

end.
