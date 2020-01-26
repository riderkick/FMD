unit TranslateWebtoon;

interface

implementation

uses Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, synautil, RegExpr, Graphics, Interfaces;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  rate, nextpage: Integer;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := ReplaceRegExpr('&page=\d+', MaybeFillHost(Module.RootURL, AURL), '', False);
    if GET(url) then
    begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//meta[@property="og:image"]/@content');
          if title = '' then
            title := XPathString('//h3[@class="subj"]') + ' [' +
              XPathString('//div[@class="info"]/p[contains(@class, "flag")]') + ']';
          authors := XPathString('//span[@class="author"]');
          while True do
          begin
            for v in XPath('//div[@class="detail_lst"]/ul/li/a') do
            begin
              s := XPathString('./span[@class="rate"]/em', v);
              s := RegExprGetMatch('(\d+)', s, 1);
              rate := StrToIntDef(s, 0);
              if rate >= 100 then
              begin
                chapterLinks.Add(v.toNode.getAttribute('href'));
                chapterName.Add(XPathString('./span[@class="subj"]', v));
              end;
            end;
            if ThreadTerminated then Break;
            s := XPathString('//div[@class="paginate"]/a[contains(@class, "epipage")]/following-sibling::a[1]/@href');
            s := RegExprGetMatch('page=(\d+)', s, 1);
            nextpage := StrToIntDef(s, 0);
            if nextpage = 0 then Break;
            if not GET(Format('%s&page=%d', [url, nextpage])) then Break;
            ParseHTML(Document);
          end;
        finally
          Free;
        end;
      InvertStrings([chapterLinks, chapterName]);
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  v, w: IXQValue;
  tmp: TStringList;
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
      with TXQueryEngineHTML.Create(Document) do
        try
          tmp := TStringList.Create;
          try
            for v in XPath('//div[@class="viewer_img"]/ul/li/div[@class="img_info"]') do
            begin
              tmp.Clear;
              for w in XPath('span[@class="ly_img_text"]', v) do
              begin
                tmp.Add(XPathString('img/@src', w));
                tmp.Add(w.toNode.getAttribute('style'));
              end;
              PageContainerLinks.Add(tmp.Text);
              PageLinks.Add(XPathString('img/@src', v));
            end;
            // FIXME: PageContainerLinks.Count shouldn't be equal to PageNumber
            PageContainerLinks.Add('$$$');
          finally
            tmp.Free;
          end;
        finally
          Free;
        end;
    end;
  end;
end;

function DownloadImage(const DownloadThread: TDownloadThread;
    const AURL: String; const Module: TModuleContainer): Boolean;
var
  back, text: TPicture;
  tmp: TStringList;
  i, left, top: Integer;
  src, dst: TRect;
  ext: String = 'jpg';
begin
  i := 0;
  Result := False;
  if DownloadThread = nil then Exit;
  if DownloadThread.FHTTP.GET(AURL) then
  begin
    back := TPicture.Create;
    tmp := TStringList.Create;
    try
      back.LoadFromStream(DownloadThread.FHTTP.Document);
      if back.Graphic is TPortableNetworkGraphic then ext := 'png';
      tmp.Text := DownloadThread.Task.Container.PageContainerLinks[DownloadThread.WorkId];
      while i < tmp.Count-1 do
      begin
        if DownloadThread.FHTTP.GET(tmp[i]) then
        begin
          text := TPicture.Create;
          try
            text.LoadFromStream(DownloadThread.FHTTP.Document);
            src := Rect(0, 0, text.Width, text.Height);
            left := StrToIntDef(RegExprGetMatch('left\s*:\s*(\d+)', tmp[i+1], 1), 0);
            top := StrToIntDef(RegExprGetMatch('top\s*:\s*(\d+)', tmp[i+1], 1), 0);
            dst := Rect(left, top, left + text.Width, top + text.Height);
            back.Bitmap.Canvas.CopyRect(dst, text.Bitmap.Canvas, src);
          finally
            text.Free;
          end;
        end;
        i := i + 2;
        if DownloadThread.CheckTerminated then Break;
      end;
      DownloadThread.FHTTP.Document.Position := 0;
      DownloadThread.FHTTP.Document.Size := 0;
      back.SaveToStreamWithFileExt(DownloadThread.FHTTP.Document, ext);
    finally
      back.Free;
      tmp.Free;
    end;
    Result := True;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'TranslateWebtoon';
    RootURL := 'https://translate.webtoons.com';
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnDownloadImage := @DownloadImage;
  end;
end;

initialization
  RegisterModule;

end.

