  function GetDynastyScansPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    imgs_: string;
  begin
    l := TStringList.Create;

    Result := GetPage(TObject(l),
      DecodeUrl(FillMangaSiteHost(DYNASTYSCANS_ID, URL)),
      Task.Container.manager.retryConnect);

    imgs_ := '';
    if l.Count > 0 then
    begin
      for i := 0 to l.Count-1 do
        if Pos('var pages = [', l[i]) > 0 then
        begin
          imgs_ := l[i];
          imgs_ := StringReplace(imgs_, 'var pages =', '', [rfIgnoreCase]);
          imgs_ := Trim(TrimChar(imgs_, [';', ' ']));
          Break;
        end;
    end;
    l.Free;

    with Task.Container do
    begin
      if imgs_ <> '' then
      begin
        PageLinks.Clear;
        ParseJSONArray(imgs_, 'image', PageLinks);
        PageNumber := PageLinks.Count;
      end;

      if PageLinks.Count > 0 then
        for i := 0 to PageLinks.Count-1 do
        begin
          if Length(PageLinks[i]) > 0 then
            if PageLinks[i][1] = '/' then
              PageLinks[i] := WebsiteRoots[DYNASTYSCANS_ID, 1] + PageLinks[i];
        end;
    end;
  end;
