unit KissManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, Cloudflare, RegExpr, synautil;

implementation

const
  dirurl='/MangaList/Newest';

var
  kissmangacookies: String='';
  kissmangalockget: TRTLCriticalSection;

function GETWithCookie(const AHTTP: THTTPSendThread; const AURL: String): Boolean;
begin
  Result:=Cloudflare.GETCF(AHTTP,AURL,kissmangacookies,kissmangalockget);
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if GETWithCookie(MangaInfo.FHTTP,Module.RootURL+dirurl) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
    try
      s:=XPathString('//ul[@class="pager"]/li[last()]/a/@href');
      if s<>'' then begin
        s:=ReplaceRegExpr('^.*=(\d+)$',s,'$1',True);
        Page:=StrToIntDef(s,1);
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
  s: String;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  s:=Module.RootURL+dirurl;
  if AURL<>'0' then
  s:=s+'?page='+IncStr(AURL);
  if GETWithCookie(MangaInfo.FHTTP,s) then begin
    Result:=NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
    try
      for v in XPath('//table[@class="listing"]/tbody/tr/td[1]/a') do begin
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
  i: Integer;
  s: String;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  if GETWithCookie(MangaInfo.FHTTP,FillHost(Module.RootURL,AURL)) then begin
    Result:=NO_ERROR;
    with MangaInfo.mangaInfo,TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
    try
      coverLink:=XPathString('//div[@id="rightside"]//img/@src');
      if title=''then title:=XPathString('//div[@id="leftside"]//a[@class="bigChar"]');
      v:=XPath('//div[@id="leftside"]//div[@class="barContent"]/div/p');
      if v.Count > 0 then begin
        i:=0;
        while i<v.Count-2 do begin
          s:=v.get(i).toString;
          if Pos('Genres:',s)=1 then genres:=SeparateRight(s,':') else
          if Pos('Author:',s)=1 then authors:=SeparateRight(s,':') else
          if Pos('Artist:',s)=1 then artists:=SeparateRight(s,':') else
          if Pos('Status:',s)=1 then begin
            if Pos('ongoing',LowerCase(v.get(i).toString))>0 then status:='1'
            else status:='0';
          end else
          if Pos('Summary:',s)=1 then summary:=v.get(i+1).toString;
          Inc(i);
        end;
      end;
      for v in XPath('//table[@class="listing"]/tbody/tr/td/a') do begin
        chapterLinks.Add(v.toNode.getAttribute('href'));
        chapterName.Add(v.toString);
      end;
      InvertStrings([chapterLinks,chapterName]);
    finally
      Free;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  source: TStringList;
  i: Integer;
begin
  Result:=False;
  if DownloadThread=nil then Exit;
  with DownloadThread.FHTTP,DownloadThread.manager.container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    if GETWithCookie(DownloadThread.FHTTP,FillHost(Module.RootURL,AURL)) then begin
      Result:=True;
      source:=TStringList.Create;
      try
        source.LoadFromStream(Document);
        if source.Count>0 then
          for i:=0 to source.Count-1 do begin
            if Pos('lstImages.push',source[i])>0 then
              PageLinks.Add(GetBetween('.push("','");',source[i]));
          end;
      finally
        source.Free;
      end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website:='KissManga';
    RootURL:='http://kissmanga.com';
    SortedList:=True;
    OnGetDirectoryPageNumber:=@GetDirectoryPageNumber;
    OnGetNameAndLink:=@GetNameAndLink;
    OnGetInfo:=@GetInfo;
    OnGetPageNumber:=@GetPageNumber;
  end;
end;

initialization
  InitCriticalSection(kissmangalockget);
  RegisterModule;

finalization
  DoneCriticalsection(kissmangalockget);

end.
