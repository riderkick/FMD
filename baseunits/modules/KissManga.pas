unit KissManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, BaseCrypto, RegExpr,
  synautil;

implementation

const
  kissmangadirurl = '/MangaList/Newest';
  readcomiconlinedirurl = '/ComicList/Newest';

var
  kissmangaiv: String ='a5e8e2e9c2721be0a84ad660c472c1f3';
  kissmangakey: String ='mshsdf832nsdbash20asdmnasdbasd612basd';

resourcestring
  RS_KissManga_Key = 'Key:';
  RS_KissManga_InitVector = 'Initialization Vector:';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL;
  if Module.Website = 'KissManga' then
    s := s + kissmangadirurl
  else if Module.Website = 'ReadComicOnline' then
    s := s + readcomiconlinedirurl;
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        s := XPathString('//ul[@class="pager"]/li[last()]/a/@href');
        if s <> '' then begin
          s := ReplaceRegExpr('^.*=(\d+)$', s, '$1', True);
          Page := StrToIntDef(s, 1);
        end;
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL;
  if Module.Website = 'KissManga' then
    s := s + kissmangadirurl
  else if Module.Website = 'ReadComicOnline' then
    s := s + readcomiconlinedirurl;
  if AURL <> '0' then
    s := s + '?page=' + IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//table[@class="listing"]/tbody/tr/td[1]/a') do begin
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
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(FillHost(Module.RootURL, AURL)) then begin
    Result := NO_ERROR;
    with MangaInfo.mangaInfo, TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        coverLink := MaybeFillHost(Module.RootURL, XPathString('//div[@id="rightside"]//img/@src'));
        if title = '' then
        begin
          title := XPathString('//title');
          if title <> '' then
          begin
            if Pos('manga | Read', title) <> 0 then
              title := SeparateLeft(title, 'manga | Read')
            else if Pos('comic | Read', title) <> 0 then
              title := SeparateLeft(title, 'comic | Read');
          end;
        end;
        genres := XPathStringAll('//div[@class="barContent"]//span[starts-with(., "Genre")]/parent::*/a');
        authors := XPathStringAll('//div[@class="barContent"]//span[starts-with(., "Author") or starts-with(., "Writer")]/parent::*/a');
        artists := XPathStringAll('//div[@class="barContent"]//span[starts-with(., "Artist")]/parent::*/a');
        status := MangaInfoStatusIfPos(XPathString(
          '//div[@class="barContent"]/div/p[starts-with(.,"Status:")]'),
          'Ongoing',
          'Completed');
        summary := XPathString('//div[@class="barContent"]/div/p[starts-with(.,"Summary:")]//following-sibling::p[1]');
        for v in XPath('//table[@class="listing"]/tbody/tr/td/a') do begin
          chapterLinks.Add(v.toNode.getAttribute('href'));
          s := v.toNode.getAttribute('title');
          if LeftStr(s, 5) = 'Read ' then
            Delete(s, 1, 5);
          if RightStr(s, 7) = ' online' then
            SetLength(s, Length(s) - 7)
          else if RightStr(s, 29) = ' comic online in high quality' then
            SetLength(s, Length(s) - 29)
          else if RightStr(s, 23) = ' online in high quality' then
            SetLength(s, Length(s) - 23);
          chapterName.Add(s);
        end;
        InvertStrings([chapterLinks, chapterName]);
      finally
        Free;
      end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  source: TStringList;
  i: Integer;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    Cookies.Values['rco_quality'] := 'hq';
    if DownloadThread.FHTTP.GET(FillHost(Module.RootURL, AURL)) then
      try
        Result := True;
        source := TStringList.Create;
        source.LoadFromStream(Document);
        if source.Count > 0 then
          for i := 0 to source.Count - 1 do begin
            if Pos('lstImages.push', source[i]) > 0 then
              PageLinks.Add(GetBetween('("', '")', source[i]));
          end;
      finally
        source.Free;
      end;
  end;
end;

function KissMangaGetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  source: TStringList;
  i, chkop, ivp: Integer;
  chkos, chko1, chko2, civ, key, iv: String;
  s: String;

  function testkeyiv(const akey, aiv: string): Boolean;
  begin
    if DownloadThread.Task.Container.PageLinks.Count=0 then Exit(False);
    Result:=Pos('://',AESDecryptCBCSHA256Base64Pkcs7(DownloadThread.Task.Container.PageLinks[0],akey,aiv))<>0;
    if Result then
    begin
      key := akey;
      iv := aiv;
    end;
  end;

  procedure testkeyivlast;
  begin
    if testkeyiv(kissmangakey, kissmangaiv) then Exit;
    SendLog('KissManga, failed to decrypt. chko1='+chko1+'; chko2= '+chko2+'; iv='+civ+'; '+ AURL, DownloadThread.Task.Container.PageLinks);
    DownloadThread.Task.Container.PageLinks.Clear;
  end;

begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if DownloadThread.FHTTP.GET(FillHost(Module.RootURL, AURL)) then
      try
        Result := True;
        chkop := -1;
        ivp := -1;
        chkos := '';
        chko1 := '';
        chko2 := '';
        civ := '';
        key := '';
        iv := '';
        s := '';
        source := TStringList.Create;
        source.LoadFromStream(Document);
        for i := 0 to source.Count - 1 do
        begin
          if Pos('lstImages.push', source[i]) <> 0 then
            PageLinks.Add(GetBetween('("', '")', source[i]))
          else if (Pos('chko', source[i]) <> 0) and (Pos('=', source[i]) <> 0) then
            chko2 := GetBetween('["', '"]', source[i]);
        end;
        if PageLinks.Count <> 0 then
        begin
          if DownloadThread.FHTTP.GET(Module.RootURL + '/Scripts/lo.js') then
          begin
            source.Text := StringReplace(StreamToString(Document), ';', LineEnding, [rfReplaceAll]);
            for i := 0 to source.Count - 1 do
              if (Pos('boxzq', source[i]) <> 0) and (Pos('=', source[i]) <> 0) then
                ivp := StrToIntDef(GetBetween('[',']', source[i]), -1)
              else if (Pos('chko', source[i]) <> 0) and (Pos('=', source[i]) <> 0) then
              begin
                chkos := GetBetween('=','[',source[i]);
                chkop := StrToIntDef(GetBetween('[',']', source[i]), -1);
                Break;
              end;
            if (chkos <> '') and (chkop <> -1) then
            begin
              for i := 0 to source.Count - 1 do
                if Pos(chkos, source[i]) <> 0 then
                begin
                  s := GetBetween('[', ']',source[i]);
                  Break;
                end;
              if s <> '' then
              begin
                source.CommaText := s;
                if chkop < source.Count then
                  chko1 := source[chkop];
                if (ivp <> -1) and (ivp < source.Count) then
                  civ := JSHexToStr(source[ivp]);
              end;
            end;
          end;
          if (chko1 <> '') or (chko2 <> '') then
          begin
            chko1 := JSHexToStr(chko1);
            chko2 := JSHexToStr(chko2);
            if civ = '' then
              civ := kissmangaiv;
            // test all possibilities
            if not testkeyiv(chko1, civ) then
            if not testkeyiv(chko2, civ) then
            if not testkeyiv(chko1+chko2, civ) then
            if not testkeyiv(chko2+chko1, civ) then
              testkeyivlast;
          end
          else
            testkeyivlast;
          if PageLinks.Count <> 0 then
          begin
            for i := 0 to PageLinks.Count - 1 do
              PageLinks[i] := AESDecryptCBCSHA256Base64Pkcs7(PageLinks[i], key, iv);
          end;
        end
        else
          SendLogError('KissManga, image list empty. '+AURL);
      finally
        source.Free;
      end;
  end;
end;

procedure RegisterModule;

  function AddWebsiteModule(AWebsite, ARootURL, ACategory: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      Category := ACategory;
      SortedList := True;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
    end;
  end;

begin
  with AddWebsiteModule('KissManga', 'https://kissmanga.com', 'English') do
  begin
    OnGetPageNumber := @KissMangaGetPageNumber;
    AddOptionEdit(@kissmangakey,'Key',@RS_KissManga_Key);
    AddOptionEdit(@kissmangaiv,'IV',@RS_KissManga_InitVector);
  end;
  AddWebsiteModule('ReadComicOnline', 'https://readcomiconline.to', 'English');
end;

initialization
  RegisterModule;

finalization

end.
