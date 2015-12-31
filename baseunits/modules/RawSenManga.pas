unit RawSenManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  RegExpr, synautil;

implementation

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL+'/Manga/?order=text-version') then begin
    Result:=NO_ERROR;
    query:=TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//table//tr/td[2]/a') do begin
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
  cu: Boolean;
  cl: String;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  s:=RemoveURLDelim(AURL);
  cl:='';
  with TRegExpr.Create do
    try
      Expression:='/\d+(/\d+)?$';
      cu:=Exec(s);
      if cu then begin
        cl:=s;
        s:=Replace(s,'',False);
      end;
    finally
      Free;
    end;
  with MangaInfo.FHTTP,MangaInfo.mangaInfo do begin
    url:=AppendURLDelim(FillHost(Module.RootURL,s));
    if GET(url) then begin
      Result:=NO_ERROR;
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        coverLink:=query.XPathString('//img[@class="series-cover"]/@src');
        if coverLink<>'' then coverLink:=MaybeFillHost(Module.RootURL,coverLink);
        if title=''then title:=query.XPathString('//h1[@itemprop="name"]');
        v:=query.XPath('//div[@class="series_desc"]/*');
        if v.Count > 0 then begin
          i:=0;
          while i<v.Count-2 do begin
            s:=v.get(i).toString;
            if Pos('Categorize in:',s)=1 then genres:=v.get(i+1).toString else
            if Pos('Author:',s)=1 then authors:=v.get(i+1).toString else
            if Pos('Artist:',s)=1 then artists:=Trim(SeparateRight(v.get(i).toString,':')) else
            if Pos('Status:',s)=1 then if Pos('ongoing',LowerCase(v.get(i).toString))>0 then status:='1' else status:='0';
            Inc(i);
          end;
        end;
        summary:=query.XPathString('//div[@class="series_desc"]//div[@itemprop="description"]');
        if cu and (cl<>'') then
          if GET(FillHost(Module.RootURL,cl)) then
          begin
            query.ParseHTML(StreamToString(Document));
            s:=query.XPathString('//select[@name="chapter"]/option[@selected="selected"]');
            if s<>'' then begin
              chapterLinks.Add(cl);
              chapterName.Add(s);
            end;
          end;
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
  s: String;
begin
  Result:=False;
  if DownloadThread=nil then Exit;
  with DownloadThread.FHTTP,DownloadThread.manager.container do begin
    s:=RemoveURLDelim(ChapterLinks[CurrentDownloadChapterPtr]);
    with TRegExpr.Create do
      try
        Expression:='/\d+/\d+$';
        if Exec(s) then begin
          Expression:='/\d+$';
          s:=Replace(s,'',False);
        end;
        ChapterLinks[CurrentDownloadChapterPtr]:=s;
      finally
        Free;
      end;
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL,s+'/1')) then begin
      Result:=True;
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        PageNumber:=query.XPath('//select[@name="page"]/option').Count;
        if PageNumber>0 then begin
          s:=MaybeFillHost(Module.RootURL,query.XPathString('//img[@id="picture"]/@src'));
          if Pos('/raw-viewer.php?',LowerCase(s))>0 then begin
            if LowerCase(RightStr(s,7))='&page=1' then begin
              SetLength(s,Length(s)-1);
              while PageLinks.Count<PageNumber do PageLinks.Add(s+IncStr(PageLinks.Count));
            end;
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
    if GET(FillHost(Module.RootURL,AURL)+'/'+IncStr(DownloadThread.WorkCounter)) then begin
      Result:=True;
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        s:=MaybeFillHost(Module.RootURL,query.XPathString('//img[@id="picture"]/@src'));
        if s<>'' then
          PageLinks[DownloadThread.workCounter]:=s;
      finally
        query.Free;
      end;
    end;
  end;
end;

function BeforeDownloadImage(var DownloadThread: TDownloadThread;
    AURL: String; Module: TModuleContainer): Boolean;
begin
  Result:=False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container do
    if CurrentDownloadChapterPtr<ChapterLinks.Count then begin
      DownloadThread.FHTTP.Headers.Values['Referer']:=' '+FillHost(Module.RootURL,ChapterLinks[CurrentDownloadChapterPtr]);
      Result:=True;
    end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website:='RawSenManga';
    RootURL:='http://raw.senmanga.com';
    MaxTaskLimit:=1;
    MaxConnectionLimit:=4;
    OnGetNameAndLink:=@GetNameAndLink;
    OnGetInfo:=@GetInfo;
    OnGetPageNumber:=@GetPageNumber;
    OnGetImageURL:=@GetImageURL;
    OnBeforeDownloadImage:=@BeforeDownloadImage;
  end;
end;

initialization
  RegisterModule;

end.
