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

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String; Module: TModuleContainer): Integer;
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
            Links.Add(TrimLeftChar(v.toNode.getAttribute('href'), ['.']));
            Names.Add(v.toString, ['.']);
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

function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Query: TXQueryEngineHTML;
  info: TMangaInfo;
  v: IXQValue;
  s: String;

  function getpropvalue(aname: String): String;
  begin
    Result := Query.XPathString(
      '(/html/body/div[3]/div/div[1]/div/div[2]/div)[contains(.,"' +
      aname + '")]');
    if Result <> '' then
      Result := Trim(TrimChar(SeparateRight(Result, aname), [':', ' ']));
  end;

begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  info := MangaInfo.mangaInfo;
  info.website := Module.Website;
  info.url := AppendURLDelim(FillHost(Module.RootURL, URL));
  Source := TStringList.Create;
  try
    if GetPage(MangaInfo.FHTTP, TObject(Source), info.url, Reconnect) then
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Query := TXQueryEngineHTML.Create(Source.Text);
        try
          with info do begin
            coverLink := Query.XPathString(
              '//div[@class="col-lg-3 col-md-3 col-sm-3 col-xs-3"]/img/@src');
            if coverLink <> '' then
              coverLink := MaybeFillHost(Module.RootURL, coverLink);
            title := Query.XPathString('//h1');
            authors := getpropvalue('Author');
            genres := getpropvalue('Genre');
            summary := getpropvalue('Description');
            s := LowerCase(getpropvalue('Scanlation Status'));
            if s <> '' then begin
              if Pos('ongoing', s) > 0 then status := '1'
              else if Pos('completed', s) > 0 then status := '0'
            end;
            //chapters
            for v in Query.XPath('/html/body/div[3]/div/div/div/a') do begin
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

function GetPageNumber(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
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
        AppendURLDelim(FillHost(Module.RootURL, URL)), Manager.retryConnect) then
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
