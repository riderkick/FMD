  function GetSenMangaRAWImageURL: Boolean;
  var
    i: Integer;
    s: String;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    try
      s := manager.container.ChapterLinks[manager.container.CurrentDownloadChapterPtr] +
        '/' + IntToStr(QWord(workCounter) + 1);
      s := FillHost(WebsiteRoots[SENMANGARAW_ID, 1], s);
      if GetPage(TObject(l), s, manager.container.Manager.retryConnect) then
      begin
        TURL := '';
        if l.Count > 0 then
          for i := 0 to l.Count - 1 do
          begin
            if Pos('var new_image =', l[i]) > 0 then begin
              TURL := GetValuesFromString(l[i], '=');
              TURL := MaybeFillHost(WebsiteRoots[SENMANGARAW_ID, 1], TURL);
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
