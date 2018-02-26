unit NewType;

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, RegExpr, Dialogs, fpjson, jsonparser;

implementation

const
  dirurl = '/contents/';

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
        for v in XPath('//li[@class="OblongCard--border"]/a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          // h3[@class="OblongCard-title"]
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
  v: IXQValue;
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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//figure/img/@src'));
          if title = '' then title := XPathString('//section[@class="WorkSummary"]/header/h1');
          summary := XPathString('//section[@id="workInfo"]/p');
          { there is no chapter list?
            assuming the first chapter link in manga info is always the last chapters }
          s := XPathString('//li[@class="ListCard"]/a[1]/@href');
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
              n := Format('%.1d', [i]);
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
  v: IXQValue;
  url,json_url,s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    url := AppendURLDelim(MaybeFillHost(Module.RootURL, AURL));
    if GET(url) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try

          { Read Json }
          json_url := XPathString('//div[@class="ViewerContainer"]/@data-url');

          if GET(Module.RootURL + json_url) then
              begin
                  with TXQueryEngineHTML.Create(Document) do

                  s := XPathString('//*');
                            if s <> '' then
                            begin
                              s := GetBetween('{', '}', s);
                              ParseHTML(s);
                              XPathStringAll('json(*)()', PageLinks);
                  end;
              end;
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'NewType';
    RootURL := 'https://comic.webnewtype.com';
    Category := 'Raw';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
