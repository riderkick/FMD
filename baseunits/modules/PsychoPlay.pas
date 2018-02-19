unit PsychoPlay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/series';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(XPathString('(//ul[contains(@class,"pagination")]//a)[last()]/replace(@href,"^.*page=(\d+)\??.*$","$1")'), 1);
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '?page=' + IncStr(AURL)) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        XPathHREFAll('//*[@class="row"]//*[@class="caption"]//a', ALinks, ANames);
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//*[@class="profile-thumb"]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//div[@class="media-body"]/h2/text()');
          summary := XPathString('//h6[text()="Synopsis"]/following-sibling::p');
          while True do
          begin
            for v in XPath('//ul[contains(@class,"media-list")]/li/a') do
            begin
              chapterLinks.Add(v.toNode.getAttribute('href'));
              chapterName.Add(XPathString('.//h6/text()', v));
            end;
            s := XPathString('//ul[contains(@class,"pagination")]/li[@class="next"]/a/@href');
            if s = '' then Break;
            if GET(MaybeFillHost(Module.RootURL, s)) then
              ParseHTML(Document)
            else Break;
            if ThreadTerminated then Break;
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
  v: IXQValue;       
begin
  Result := False;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    if GET(RemoveURLDelim(FillHost(Module.RootURL, AURL))) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//*[@class="content-wrapper"]/*[@class="row"]/img') do
            PageLinks.Add(MaybeFillHost(Module.RootURL, v.toNode.getAttribute('src')));
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
    Website := 'PsychoPlay';
    RootURL := 'https://psychoplay.co';
    Category := 'English-Scanlation';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
