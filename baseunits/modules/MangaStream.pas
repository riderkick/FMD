unit MangaStream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager, httpsendthread;

implementation

uses
  simplehtmltreeparser, xquery, RegExpr;

const
  readURL = 'http://readms.com';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Page := 1;
  Result := NO_ERROR;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
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
            ALinks.Add(v.toNode.getAttribute('href'));
            ANames.Add(v.toString);
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

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
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
  info.url := FillHost(Module.RootURL, AURL);
  Source := TStringList.Create;
  try
    if MangaInfo.FHTTP.GET(info.url, TObject(Source)) then
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
              chapterLinks.Add(EncodeCriticalURLElements(s));
              chapterName.Add(v.toString);
            end;
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

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  Container: TTaskContainer;
  Source: TStringList;
  Parser: TTreeParser;
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.Task.Container;
  Container.PageLinks.Clear;
  Container.PageContainerLinks.Clear;
  Container.PageNumber := 0;
  Source := TStringList.Create;
  try
    if GetPage(DownloadThread.FHTTP, TObject(Source), FillHost(readURL, AURL + '/1'),
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

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Parser: TTreeParser;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container do begin
    Source := TStringList.Create;
    try
      if GetPage(DownloadThread.FHTTP, TObject(Source),
        AppendURLDelim(FillHost(readURL, AURL)) +
        IncStr(DownloadThread.WorkId), Manager.retryConnect) then
        if Source.Count > 0 then
        begin
          Result := True;
          Parser := TTreeParser.Create;
          try
            ParseHTMLTree(Parser, Source.Text);
            PageLinks[DownloadThread.WorkId] :=
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
