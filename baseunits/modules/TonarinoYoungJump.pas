unit TonariNoYoungJump;

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML;

implementation

uses Graphics, Math, synautil;

const
  chapterListQuery = '/api/viewer/readable_products?current_readable_product_id=%s&number_since=%d&number_until=-1&read_more_num=50&type=episode';
  dirurls: array[0..2] of String = ('/series', '/series/finished', '/series/oneshot');

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurls[Module.CurrentDirectoryIndex]) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//div[@class="series-items"]/ul/li/a') do begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(XPathString('h4', v));
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
  lastChapter: Integer;
  s, episode, priv: String;
  node: TTreeNode;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then
    begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//div[@class="series-header-image-wrapper"]/img/@src'));
          if title = '' then title := XPathString('//h1[@class="series-header-title"]');
          authors := XPathString('//h2[@class="series-header-author"]');
          summary := XPathString('//*[@class="series-header-description"]');
          lastChapter := XPathCount('//ul[contains(@class, "series-episode-list")]/li') + 1;
          s := XPathString('//button[@class="js-read-more-button"]/@data-read-more-endpoint');
          if s <> '' then begin
            s := RegExprGetMatch('number_since\=(\d+)\&', s, 1);
            lastChapter += StrToIntDef(s, 0);
          end;
          episode := RegExprGetMatch('episode\/(\d+)', AURL, 1);
        finally
          Free;
        end;

      while lastChapter > 1 do begin
        if not GET(Module.RootURL + Format(chapterListQuery, [episode, lastChapter])) then Break;
        if MangaInfo.Thread.IsTerminated then Break;
        with TXQueryEngineHTML.Create(Document) do
          try
            node := XPath('json(*).html').toNode;
            for v in XPath('//li', node) do begin
              priv := '';
              if Pos('private', v.toNode.outerHTML()) > 0 then
                priv := ' [private]';
              s := XPathString('a/@href', v);
              if s <> '' then begin
                chapterLinks.Add(ReplaceString(s, '\"', ''));
                chapterName.Add(XPathString('a/div/h4', v) + priv);
              end
              else begin
                chapterLinks.Add(url);
                chapterName.Add(XPathString('div/h4', v) + priv);
              end;
            end;
          finally
            Free;
          end;
        lastChapter -= 50;
      end;
      InvertStrings([chapterLinks, chapterName]);
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      XPathStringAll('//img[contains(@class, "js-page-image")]/@data-src', Document, PageLinks);
    end;
  end;
end;

function DeScramble(AURL: String; image: TPicture): TPicture;
const
  DIVIDE_NUM = 4;
  MULTIPLE = 8;
var
  cell_width, cell_height, e, sx, sy, dx, dy, a: Integer;
  srcrect, destrect: TRect;
  r: TPicture;
begin
  Result := nil;
  cell_width := Trunc(Real(image.Width) / Real(DIVIDE_NUM * MULTIPLE)) * MULTIPLE;
  cell_height := Trunc(Real(image.Height) / Real(DIVIDE_NUM * MULTIPLE)) * MULTIPLE;
  r := TPicture.Create;
  r.Bitmap.SetSize(image.Width, image.Height);
  for e := 0 to DIVIDE_NUM * DIVIDE_NUM - 1 do begin
    sy := Trunc(Real(e) / Real(DIVIDE_NUM)) * cell_height;
    sx := (e mod DIVIDE_NUM) * cell_width;
    a := (e mod DIVIDE_NUM) * DIVIDE_NUM + Trunc(Real(e) / Real(DIVIDE_NUM));
    dx := (a mod DIVIDE_NUM) * cell_width;
    dy := Trunc(Real(a) / Real(DIVIDE_NUM)) * cell_height;
    srcrect := Rect(sx, sy, sx + cell_width, sy + cell_height);
    destrect := Rect(dx, dy, dx + cell_width, dy + cell_height);
    r.Bitmap.Canvas.CopyRect(destrect, image.Bitmap.Canvas, srcrect);
  end;
  Result := r;
end;

function DownloadImage(const DownloadThread: TDownloadThread;
    const AURL: String; const Module: TModuleContainer): Boolean;
var
  image, r: TPicture;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  if DownloadThread.FHTTP.GET(AURL) then begin
    image := TPicture.Create;
    r := nil;
    try
      image.LoadFromStream(DownloadThread.FHTTP.Document);
      r := DeScramble(AURL, image);
      if r <> nil then begin
        DownloadThread.FHTTP.Document.Clear;
        r.SaveToStreamWithFileExt(DownloadThread.FHTTP.Document, 'jpg');
        Result := True;
      end;
    finally
      r.Free;
      image.Free;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'TonarinoYoungJump';
    RootURL := 'https://tonarinoyj.jp';
    Category := 'Raw';
    TotalDirectory := Length(dirurls);
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnDownloadImage := @DownloadImage;
  end;
end;

initialization
  RegisterModule;

end.
