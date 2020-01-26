unit SundayWebEvery;

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, RegExpr, Dialogs;

implementation

const
  dirurl = '/comics/';

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//ul[@class="manga-list__list"]/li/h4/a') do
        begin
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
  s, n: String;
  last_eps, i: Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := AppendURLDelim(MaybeFillHost(Module.RootURL, AURL));
    if GET(url) then
    begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//*[@id="mainvisual"]/img/@src'));
          if title = '' then title := XPathString('//*[@class="title"]/h1');
          summary := XPathString('//*[@class="title"]/h2');
          { there is no chapter list?
            assuming the first chapter link in manga info is always the last chapters }
          s := XPathString('//article/a[1]/@href');
          with TRegExpr.Create do
            try
              Expression := '^(.+?)(\d+)/?$';
              last_eps := StrToIntDef(Replace(s, '$2', True), 0);
              s := Replace(s, '$1', True);
            finally
              Free;
            end;
          if (last_eps > 0) and (s <> '') then
            for i := 1 to last_eps do
            begin
              n := Format('%.3d', [i]);
              chapterLinks.Add(s + n);
              chapterName.Add(n);
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
  source: TStringList;
  i: Integer;
  key: String;
  data: IXQValue;
  regx: TRegExpr;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(AppendURLDelim(MaybeFillHost(Module.RootURL, AURL))) then
    begin
      Result := True;
      key := '';
      regx := TRegExpr.Create;
      source := TStringList.Create;
      try
        source.LoadFromStream(Document);
        if source.Count > 0 then
          for i := 0 to source.Count - 1 do
          begin
            if Pos('key: ', source[i]) <> 0 then
            begin
              regx.Expression := '^.*["''](.+)["''].*$';
              key := regx.Replace(source[i], '$1', True);
              Break;
            end;
          end;
      finally
        source.Free;
      end;
      if key <> '' then
      begin
        key := Module.RootURL + '/assets/episodes/' + key + '/';
        if GET(key + 'episode.json') then
        begin
          with TXQueryEngineHTML.Create(Document) do

          { h1536, h128, h1024
+                the available res is varie for every page/image, some only had h1024
+                try to get the biggest available }


      for data in XPath('json(*).pages()/files/h1536.jpeg') do
           PageLinks.Add(key + data.toString);



        end;
      end;
      regx.Free;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'SundayWebEvery';
    RootURL := 'https://www.sunday-webry.com';
    Category := 'Raw';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
