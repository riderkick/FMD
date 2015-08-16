unit MangaStream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager;

implementation

uses
  simplehtmltreeparser, xquery, RegExpr;

const
  readURL = 'http://readms.com';

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
begin
  Page := 1;
  Result := NO_ERROR;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String; Module: TModuleContainer): Integer;
var
  Source: TStringList;
  Parser: TTreeParser;
  v: IXQValue;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  Source := TStringList.Create;
  try
    if GetPage(MangaInfo.FHTTP, TObject(Source), Module.RootURL + '/manga', 3) then
      if Source.Count > 0 then begin
        Result := NO_ERROR;
        Parser := TTreeParser.Create;
        try
          ParseHTMLTree(Parser, Source.Text);
          for v in SelectXPathIX('//table//tr/td[1]//a', Parser) do begin
            Links.Add(v.toNode.getAttribute('href'));
            Names.Add(v.toString);
          end
        finally
          Parser.Free;
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
  info: TMangaInfo;
  Source: TStringList;
  Parser: TTreeParser;
  v: IXQValue;
  s: String;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  info := MangaInfo.mangaInfo;
  info.website := Module.Website;
  info.url := FillHost(Module.RootURL, URL);
  Source := TStringList.Create;
  try
    if GetPage(MangaInfo.FHTTP, TObject(Source), info.url, Reconnect) then
      if Source.Count > 0 then
      begin
        Result := NO_ERROR;
        Parser := TTreeParser.Create;
        try
          ParseHTMLTree(Parser, Source.Text);
          with info do begin
            //title
            title := SelectXPathString('//h1', Parser);
            //chapters
            for v in SelectXPathIX('//table//td/a', Parser) do begin
              s := v.toNode.getAttribute('href');
              if (Length(s) > 2) and (RightStr(s, 2) = '/1') then
                SetLength(s, Length(s) - 2);
              chapterLinks.Add(s);
              chapterName.Add(v.toString);
            end;
            //invert chapters
            if chapterLinks.Count > 0 then
              InvertStrings([chapterLinks, chapterName]);
          end;
        finally
          Parser.Free;
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
  Container: TTaskContainer;
  Source: TStringList;
  Parser: TTreeParser;
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  Container.PageLinks.Clear;
  Container.PageContainerLinks.Clear;
  Container.PageNumber := 0;
  Source := TStringList.Create;
  try
    if GetPage(DownloadThread.FHTTP, TObject(Source), FillHost(readURL, URL + '/1'),
      Container.Manager.retryConnect) then
      if Source.Count > 0 then
      begin
        Result := True;
        Parser := TTreeParser.Create;
        try
          ParseHTMLTree(Parser, Source.Text);
          s := SelectXPathString(
            '//div[@class="controls"]/div[2]/ul[@class="dropdown-menu"]/li[last()]/a',
            Parser);
          if s <> '' then begin
            s := ReplaceRegExpr('^.*\((\d+)\).*$', s, '$1', True);
            Container.PageNumber := StrToIntDef(s, 0);
          end;
        finally
          Parser.Free;
        end;
      end;
  finally
    Source.Free;
  end;
end;

function GetImageURL(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Parser: TTreeParser;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container do begin
    Source := TStringList.Create;
    try
      if GetPage(DownloadThread.FHTTP, TObject(Source),
        AppendURLDelim(FillHost(readURL, URL)) +
        IncStr(DownloadThread.workCounter), Manager.retryConnect) then
        if Source.Count > 0 then
        begin
          Result := True;
          Parser := TTreeParser.Create;
          try
            ParseHTMLTree(Parser, Source.Text);
            PageLinks[DownloadThread.workCounter] :=
              SelectXPathString('//img[@id="manga-page"]/@src', Parser);
          finally
            Parser.Free;
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
    Website := 'MangaStream';
    RootURL := 'http://mangastream.com';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
