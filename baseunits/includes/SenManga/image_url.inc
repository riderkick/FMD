  function GetSenMangaImageURL: Boolean;
  var
    s, imageURL: string;
    i: Integer;
    l: TStringList;
  begin
    //skip already exist file
    s := lpath + '/' +  Format('%.3d', [workCounter + 1]);
    if (FileExists(s + '.jpg')) or
      (FileExists(s + '.png')) or
      (FileExists(s + '.gif')) then
    begin
      Result := True;
      Exit;
    end;

    s := manager.container.ChapterLinks[manager.container.CurrentDownloadChapterPtr] +
      '/' + IntToStr(workCounter + 1);
    s := FillMangaSiteHost(SENMANGA_ID, s);
    l := TStringList.Create;
    try
      if GetPage(FHTTP, TObject(l), s, manager.container.Manager.retryConnect) then
      begin
        if l.Count > 0 then
          for i := 0 to l.Count - 1 do
            if Pos('var new_image =', l[i]) <> 0 then
            begin
              imageURL := StringReplace(l[i], 'var new_image = ''', '', [rfIgnoreCase]);
              imageURL := Trim(TrimChar(imageURL, ['''', ';']));
              if (Length(imageURL) > 0) and (imageURL[1] = '/') then
                imageURL := WebsiteRoots[SENMANGA_ID, 1] + imageURL;
              Break;
            end;
      end;
    finally
      l.Free;
    end;

    if imageURL <> '' then
    begin
      FHTTP.Clear;
      FHTTP.Headers.Add('Referer: ' + s);
      Result := SaveImage(Self,
        FHTTP,
        manager.container.MangaSiteID,
        imageURL,
        lpath,
        Format('%.3d', [workCounter + 1]),
        prefix,
        1);
    end;
  end;
