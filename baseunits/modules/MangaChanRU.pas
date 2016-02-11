unit MangaChanRU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  synautil, RegExpr;

implementation

const
  dirurl='/manga/new';
  perpage=20;

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL+dirurl) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        s:=XPathString('//*[@id="pagination"]/a[last()]/@href');
        if s<>'' then Page:=StrToIntDef(SeparateRight(s,'offset='),1);
        if Page>1 then
          Page:=ceil(Page/perpage);
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String; Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  s:=Module.RootURL+dirurl;
  if AURL<>'0' then s+='?offset='+IntToStr(StrToInt(AURL)*perpage);
  if MangaInfo.FHTTP.GET(s) then begin
    Result:=NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//*[@class="content_row"]//a[@class="title_link"]') do begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(v.toString);
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const AURL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP,MangaInfo.mangaInfo do begin
    if GET(FillHost(Module.RootURL,AURL)) then begin
      Result:=NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink:=XPathString('//*[@id="manga_images"]//img[@id="cover"]/@src');
          if coverLink<>'' then coverLink:=MaybeFillHost(Module.RootURL,coverLink);
          if title=''then title:=XPathString('//*[@class="name_row"]/h1');
          authors:=XPathString('//table[@class="mangatitle"]/tbody/tr[starts-with(.,"Автор")]/td[2]');
          genres:=XPathString('//table[@class="mangatitle"]/tbody/tr[starts-with(.,"Тип")]/td[2]');
          AddCommaString(genres,XPathString('//table[@class="mangatitle"]/tbody/tr[starts-with(.,"Тэги")]/td[2]'));
          s:=XPathString('//table[@class="mangatitle"]/tbody/tr[starts-with(.,"Загружено")]/td[2]');
          if s<>'' then begin
            if Pos('продолжается',s)>0 then status:='1'
            else status:='0';
          end;
          summary:=XPathString('//*[@id="description"]/text()');
          for v in XPath('//table[@class="table_cha"]//tr/td/*[@class="manga"]/a') do begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(v.toString);
          end;
          InvertStrings([chapterLinks,chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(var DownloadThread: TDownloadThread; const AURL: String;
  Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  i, j: Integer;
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
      Source:=TStringList.Create;
      try
        Source.LoadFromStream(Document);
        if Source.Count>0 then
          for i:=0 to Source.Count-1 do
            if Pos('"fullimg":',Source[i])>0 then begin
              s:=SeparateRight(Source[i],':');
              s:=TrimChar(s,['[',']',',']);
              PageLinks.CommaText:=s;
              if PageLinks.Count>0 then
                with TRegExpr.Create('(?i)//im(\d*\.)') do
                  try
                    for j:=0 to PageLinks.Count-1 do
                      PageLinks[j]:=Replace(PageLinks[j],'//img$1',True);
                  finally
                    Free;
                  end;
              Break;
            end;
      finally
        Source.Free;
      end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website:='MangaChanRU';
    RootURL:='http://mangachan.ru';
    SortedList:=True;
    OnGetDirectoryPageNumber:=@GetDirectoryPageNumber;
    OnGetNameAndLink:=@GetNameAndLink;
    OnGetInfo:=@GetInfo;
    OnGetPageNumber:=@GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
