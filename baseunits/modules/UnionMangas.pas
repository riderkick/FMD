unit UnionMangas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, RegExpr, synautil;

implementation

const
  dirurl='/mangas';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL+dirurl) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      s:=query.XPathString('//*[@class="pagination"]/li[last()]/a/@href');
      if s<>'' then begin
        s:=ReplaceRegExpr('^.*/(\d+)/?\*?$',s,'$1',True);
        Page:=StrToIntDef(s,1);
      end;
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
  if AURL<>'0' then s+='/a-z/'+IncStr(AURL)+'/*';
  if MangaInfo.FHTTP.GET(s) then begin
    Result:=NO_ERROR;
    query:=TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//div[@class="row"]/div/a[2]') do begin
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
  s: String;
begin
  Result:=NET_PROBLEM;
  if MangaInfo=nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP,MangaInfo.mangaInfo do begin
    if GET(FillHost(Module.RootURL,AURL)) then begin
      Result:=NO_ERROR;
      query:=TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        coverLink:=query.XPathString('//img[@class="img-thumbnail"]/@src');
        if title=''then title:=query.XPathString('//div/h2');
        for v in query.XPath('//h4') do begin
          s:=v.toString;
          if Pos('Gênero(s):',s)=1 then genres:=SeparateRight(s,':') else
          if Pos('Autor:',s)=1 then authors:=SeparateRight(s,':') else
          if Pos('Artista:',s)=1 then artists:=SeparateRight(s,':') else
          if Pos('Status:',s)=1 then begin
            if Pos('Ativo',s)>0 then status:='1' else status:='0';
          end;
        end;
        summary:=query.XPathString('//div/div[@class="panel-body"]');
        for v in query.XPath('//div[@class="row lancamento-linha"]/div[1]/a') do begin
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

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
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
    Website:='UnionMangas';
    RootURL:='http://unionmangas.com.br';
    OnGetDirectoryPageNumber:=@GetDirectoryPageNumber;
    OnGetNameAndLink:=@GetNameAndLink;
    OnGetInfo:=@GetInfo;
    OnGetPageNumber:=@GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
