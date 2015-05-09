  function GetMeinMangaImageURL: Boolean;
  var
    s, imageName: String;
    prefix: Cardinal = 0;
    i: Cardinal;
    l: TStringList;
    Parser: THTMLParser;
  begin
    s := manager.container.DownloadInfo.SaveTo +
      '/' + manager.container.ChapterName.Strings[
      manager.container.CurrentDownloadChapterPtr] + '/' +
      Format('%.3d', [workCounter + 1]);
    // Check to see if a file with similar name was already exist. If so then we
    // skip the download process.
    if (FileExists(s + '.jpg')) or
      (FileExists(s + '.png')) or
      (FileExists(s + '.gif')) then
    begin
      Result := True;
      Exit;
    end;

    l := TStringList.Create;

    FHTTP.Headers.Values['Accept'] := ' image/png,image/*;q=0.8,*/*;q=0.5';
    FHTTP.Headers.Values['Accept-Encoding'] := ' gzip, deflate';
    FHTTP.Headers.Values['Accept-Language'] := ' en-US,en;q=0.5';

    Result := GetPage(FHTTP,
      TObject(l),
      manager.container.PageLinks.Strings[workCounter],
      manager.container.Manager.retryConnect);

    if Self.Terminated then
    begin
      l.Free;
      FHTTP.Free;
      Exit;
    end;

    parse := TStringList.Create;
    try
      Parser := THTMLParser.Create(PChar(l.Text));
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
    finally
      Parser.Free;
    end;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if (Pos('class="pic_fragment"', parse.Strings[i]) > 0) then
        begin
          FHTTP.Clear;

          SaveImage(self,
            FHTTP,
            manager.container.MangaSiteID,
            GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')),
            CorrectPathSys(manager.container.DownloadInfo.SaveTo +
              manager.container.ChapterName.Strings[
            manager.container.CurrentDownloadChapterPtr]),
            Format('%.3d', [workCounter + 1]),
            '_' + IntToStr(prefix),
            manager.container.Manager.retryConnect);

          Inc(prefix);

          if Self.Terminated then
          begin
            l.Free;
            parse.Free;
            //HTTP.Free;
            Exit;
          end;
        end;
    end;
    // If prefix = 2 then there're 2 separate images. We need to merge them into one ...
    if prefix = 2 then
    begin
      imageName := Format('%.3d', [workCounter + 1]);
      Merge2Images(
        CorrectPathSys(manager.container.DownloadInfo.SaveTo + '/' +
        manager.container.ChapterName.Strings[manager.container.CurrentDownloadChapterPtr]),
        imageName + '_' + IntToStr(prefix - 2) + '.jpg',
        imageName + '_' + IntToStr(prefix - 1) + '.jpg',
        imageName + '.jpg');
    end; // Merging.
    parse.Free;
    l.Free;
  end;
