  function GetMeinMangaImageURL: Boolean;
  var
    s, imageName: String;
    prefix: Cardinal = 0;
    i: Cardinal;
    l: TStringList;
    Parser: THTMLParser;
  begin
    s := Task.Container.DownloadInfo.SaveTo +
      '/' + Task.Container.ChapterName[
      Task.Container.CurrentDownloadChapterPtr] + '/' +
      Format('%.3d', [workCounter + 1]);
    // Check to see if a file with similar name was already exist. If so then we
    // skip the download process.
    if (FileExistsUTF8(s + '.jpg')) or
      (FileExistsUTF8(s + '.png')) or
      (FileExistsUTF8(s + '.gif')) then
    begin
      Result := True;
      Exit;
    end;

    l := TStringList.Create;

    FHTTP.Headers.Values['Accept'] := ' image/png,image/*;q=0.8,*/*;q=0.5';
    FHTTP.Headers.Values['Accept-Encoding'] := ' gzip, deflate';
    FHTTP.Headers.Values['Accept-Language'] := ' en-US,en;q=0.5';

    Result := GetPage(TObject(l),
      Task.Container.PageLinks[workCounter],
      Task.Container.Manager.retryConnect);

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
        if (Pos('class="pic_fragment"', parse[i]) > 0) then
        begin
          FHTTP.Clear;

          SaveImage(
            Task.Container.MangaSiteID,
            GetVal(parse[i], 'src'),
            Task.Container.CurrentWorkingDir,
            Format('%.3d', [workCounter + 1]) +
            '_' + IntToStr(prefix),
            Task.Container.Manager.retryConnect);

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
      Merge2Image(
        Task.Container.CurrentWorkingDir,
        imageName + '_' + IntToStr(prefix - 2) + '.jpg',
        imageName + '_' + IntToStr(prefix - 1) + '.jpg',
        imageName + '.jpg');
    end; // Merging.
    parse.Free;
    l.Free;
  end;
