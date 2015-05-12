  function GetSenMangaRAWImageURL: Boolean;
  var
    i: Integer;
    s: String;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    try
      FHTTP.UserAgent := RandomString(20+Random(100));
      s := manager.container.ChapterLinks[manager.container.CurrentDownloadChapterPtr] +
        '/' + IntToStr(workCounter + 1);
      s := FillMangaSiteHost(SENMANGARAW_ID, s);
      if GetPage(TObject(l), s, manager.container.Manager.retryConnect) then
      begin
        TURL := '';
        if l.Count > 0 then
          for i := 0 to l.Count - 1 do
          begin
            if Pos('var new_image =', l[i]) <> 0 then
              begin
                TURL := StringReplace(l[i], 'var new_image = ''', '', [rfIgnoreCase]);
                TURL := Trim(TrimChar(TURL, ['''', ';']));
                if (Length(TURL) > 0) and (TURL[1] = '/') then
                  TURL := WebsiteRoots[SENMANGARAW_ID, 1] + TURL;
                Break;
              end;
          end;
        Result := (TURL <> '');
        FHTTP.Clear;
        FHTTP.Headers.Values['Referer'] := ' ' + s;
      end
      else
        Result := False;
    finally
      parse.Free;
      l.Free;
    end;
  end;
