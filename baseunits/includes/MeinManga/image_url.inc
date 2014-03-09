  function GetMeinMangaImageURL: Boolean;
  var
    s,
    imageName: String;
    prefix: Cardinal = 0;
    j,
    i   : Cardinal;
    l   : TStringList;
    HTTP: THTTPSend;
    Headers: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(URL + '/' + IntToStr(workCounter+1) + '.html');
    if Pos(WebsiteRoots[MEINMANGA_ID,1], s) = 0 then
      s:= WebsiteRoots[MEINMANGA_ID,1] + s;

    HTTP:= THTTPSend.Create;
    HTTP.Headers.Insert(0, 'Accept: image/png,image/*;q=0.8,*/*;q=0.5');
    HTTP.Headers.Insert(0, 'Accept-Encoding: gzip, deflate');
    HTTP.Headers.Insert(0, 'Accept-Language: en-US,en;q=0.5');
    HTTP.Protocol:= '1.1';
    HTTP.UserAgent:= 'Mozilla/5.0 (Windows NT 6.3; WOW64; rv:27.0) Gecko/20100101 Firefox/27.0';

    Headers:= TStringList.Create;
    Result:= GetPage(HTTP,
                     TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    Headers.Text:= HTTP.Headers.Text;
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count>0 then
    begin
      for i:= 0 to parse.Count-1 do
        if (Pos('class="pic_fragment"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workCounter]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          HTTP.Headers.Text:= Headers.Text;
          DownloadImage(HTTP, '_' + IntToStr(prefix));
          Inc(prefix);
        end;
    end;
    // If prefix = 2 then there're 2 separate images. We need to merge them into one ...
    if prefix = 2 then
    begin
      imageName:= Format('%.3d', [workCounter+1]);
      Merge2Images(
        manager.container.downloadInfo.SaveTo+'/'+manager.container.chapterName.Strings[manager.container.currentDownloadChapterPtr],
        imageName + '_' + IntToStr(prefix-2) + '.jpg',
        imageName + '_' + IntToStr(prefix-1) + '.jpg',
        imageName + '.png');
    end; // Merging.
    Headers.Free;
    HTTP.Free;
    parse.Free;
    l.Free;
  end;
