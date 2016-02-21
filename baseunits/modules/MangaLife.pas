unit MangaLife;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager;

implementation

uses
  synautil;

const
  dirURL = '/directory/';

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  v: IXQValue;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  Source := TStringList.Create;
  try
    if GetPage(MangaInfo.FHTTP, TObject(Source), Module.RootURL + dirURL, 3) then
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          for v in Query.XPath('//*[@id="content"]/p/a') do begin
            ALinks.Add(TrimLeftChar(v.toNode.getAttribute('href'), ['.']));
            ANames.Add(v.toString);
          end;
        finally
          Query.Free;
        end;
      end
      else
        Result := INFORMATION_NOT_FOUND;
  finally
    Source.Free;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  info: TMangaInfo;
  v: IXQValue;
  s: String;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  info := MangaInfo.mangaInfo;
  info.website := Module.Website;
  info.url := AppendURLDelim(FillHost(Module.RootURL, AURL));
  Source := TStringList.Create;
  try
    if MangaInfo.FHTTP.GET(info.url, TObject(Source)) then
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          with info do begin
            coverLink := Query.XPathString('//div[@class="well"]/div[1]/div/img/@src');
            if coverLink <> '' then
              coverLink := MaybeFillHost(Module.RootURL, coverLink);
            title := Query.XPathString('//h1');
            for v in Query.XPath('//span[@class="details hidden-xs"]/div') do begin
              s := v.toString;
              if Pos('Author:', s) = 1 then authors := SeparateRight(s, ':')
              else if Pos('Artist:', s) = 1 then artists := SeparateRight(s, ':')
              else if Pos('Genre:', s) = 1 then artists := SeparateRight(s, ':')
              else if Pos('Scanlation Status:', s) = 1 then begin
                if Pos('Ongoing', s) > 0 then status := '1'
                else status := '0';
              end;
            end;
            summary := Query.XPathString('//span[@class="details hidden-xs"]/div/div/div');
            //chapters
            for v in Query.XPath('//div[@class="list"]/div/div/a') do begin
              s := v.toNode.getAttribute('href');
              if RightStr(s, 6) = 'page-1' then SetLength(s, Length(s) - 6);
              chapterLinks.Add(s);
              chapterName.Add(v.toString);
            end;
            InvertStrings([chapterLinks, chapterName]);
          end;
        finally
          Query.Free;
        end;
      end
      else
        Result := INFORMATION_NOT_FOUND;
  finally
    Source.Free;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  Container: TTaskContainer;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  with Container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    Source := TStringList.Create;
    try
      if GetPage(DownloadThread.FHTTP, TObject(Source),
        AppendURLDelim(FillHost(Module.RootURL, AURL)), Manager.retryConnect) then
        if Source.Count > 0 then
        begin
          Result := True;
          Query := TXQueryEngineHTML.Create(Source.Text);
          try
            for v in Query.XPath('//*[@class="imagePage"]/img/@src') do
              Container.PageLinks.Add(v.toString);
          finally
            Query.Free;
          end;
        end;
    finally
      Source.Free;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaLife';
    RootURL := 'http://manga.life';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
