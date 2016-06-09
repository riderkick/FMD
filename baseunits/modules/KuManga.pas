unit KuManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/backend/ajax/searchengine.php';
  dirperpage = '15';
  dirpostdata = 'contentType=manga&retrieveCategories=true&retrieveAuthors=true&perPage=' +
    dirperpage + '&page=';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.POST(Module.RootURL + dirurl, dirpostdata + '1') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        s := XPathString('json(*).totalContents');
        Page := ceil(StrToIntDef(s, 1) / StrToInt(dirperpage));
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  c, i: Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.POST(Module.RootURL + dirurl, dirpostdata + IncStr(AURL)) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        c := XPath('json(*).contents()').Count;
        for i := 1 to c do
        begin
          ANames.Add(XPathString('json(*).contents(' + IntToStr(i) + ').name'));
          ALinks.Add(XPathString('json(*).contents(' + IntToStr(i) + ')/concat("/manga/",id,"/",slug)'));
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
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//div[@class="row"]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//h2');
          summary := XPathString('//div[@id="info"]/div[1]/div[1]/div[1]/div[2]');
          genres := XPathStringAll('//div[@id="info"]/div[1]/div[1]/div[1]/div[3]/a');
          authors := XPathString('//div[@id="info"]/div[1]/div[2]/div[1]/div[2]/p[2]/a');
          status := MangaInfoStatusIfPos(
            XPathString('//div[@id="info"]/div[1]/div[2]/div[1]/div[2]/p[3]/span'), 'Activo', 'Completo');
          for v in XPath('//div[@id="info"]/div[2]//table/tbody/tr/td/a') do
          begin
            chapterLinks.Add(StringReplace(v.toNode.getAttribute('href'),
              '/c/', '/leer/', [rfIgnoreCase]));
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
  source: TStringList;
  i, p: Integer;
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      source := TStringList.Create;
      try
        source.LoadFromStream(Document);
        if source.Count > 0 then
          for i := 0 to source.Count - 1 do
            if Pos('konekomangareader(''setup'',{', source[i]) <> 0 then
            begin
              s := GetBetween('.konekomangareader(''setup'',{', '});', source[i]);
              Break;
            end;
      finally
        source.Free;
      end;
      if s <> '' then
      begin
        s := '{' + s + '}';
        with TXQueryEngineHTML.Create(s) do
          try
            p := StrToIntDef(XPathString('json(*).pages'), 0);
            if p > 0 then
            begin
              s := XPathString('json(*).pageFormat');
              if Pos('{pnumber}', s) <> 0 then
                for i := 1 to p do
                  PageLinks.Add(StringReplace(s, '{pnumber}', IntToStr(i), [rfReplaceAll]));
            end;
          finally
            Free;
          end;
      end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'KuManga';
    RootURL := 'http://www.kumanga.com';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
