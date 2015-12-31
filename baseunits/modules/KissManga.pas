unit KissManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  Cloudflare, RegExpr, synautil;

implementation

var
  kissmangacookies: String='';
  lockget: TRTLCriticalSection;
  onlockget: Boolean=False;

function GETWithCookie(const AHTTP: THTTPSendThread; AURL: String): Boolean;
begin
  AHTTP.Cookies.Text:=kissmangacookies;
  Result:=AHTTP.GET(AURL);
  if (AHTTP.ResultCode>500) and Cloudflare.AntiBotActive(AHTTP) then begin
    if TryEnterCriticalsection(lockget)>0 then begin
      onlockget:=True;
      Result:=Cloudflare.GETCF(AHTTP,AURL,kissmangacookies);
      onlockget:=False;
      LeaveCriticalsection(lockget);
    end
    else begin
      while onlockget do Sleep(1000);
      AHTTP.Cookies.Text:=kissmangacookies;
    end;
    Result:=AHTTP.GET(AURL);
  end;
end;

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if GETWithCookie(MangaInfo.FHTTP,Module.RootURL+'/MangaList/Newest') then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      s:=query.XPathString('//ul[@class="pager"]/li[last()]/a/@href');
      if s<>'' then begin
        s:=ReplaceRegExpr('^.*=(\d+)$',s,'$1',True);
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
  s:=Module.RootURL+'/Mangalist/Newest';
  if AURL<>'0' then
  s:=s+'?page='+IncStr(AURL);
  if GETWithCookie(MangaInfo.FHTTP,s) then begin
    Result:=NO_ERROR;
    query:=TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//table[@class="listing"]/tbody/tr/td[1]/a') do begin
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
  i: Integer;
  s: String;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP,MangaInfo.mangaInfo do begin
    if GETWithCookie(MangaInfo.FHTTP,FillHost(Module.RootURL,AURL)) then begin
      Result:=NO_ERROR;
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        coverLink:=query.XPathString('//div[@id="rightside"]//img/@src');
        if title=''then title:=query.XPathString('//div[@id="leftside"]//a[@class="bigChar"]');
        v:=query.XPath('//div[@id="leftside"]//div[@class="barContent"]/div/p');
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
        for v in query.XPath('//table[@class="listing"]/tbody/tr/td/a') do begin
          chapterLinks.Add(v.toNode.getAttribute('href'));
          chapterName.Add(v.toString);
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
  InitCriticalSection(lockget);
  RegisterModule;

finalization
  DoneCriticalsection(lockget);

end.
