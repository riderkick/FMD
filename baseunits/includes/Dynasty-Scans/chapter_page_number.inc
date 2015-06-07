  function GetDynastyScansPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    imgs: string;
  begin
    l := TStringList.Create;

    Result := GetPage(TObject(l),
      DecodeUrl(FillMangaSiteHost(DYNASTYSCANS_ID, URL)),
      manager.container.manager.retryConnect);

    ParseHTML(l.Text, parse);
    if l.Count > 0 then
    begin
      for i := 0 to l.Count-1 do
        if Pos('var pages = [', l[i]) > 0 then
        begin
          imgs := l[i];
          imgs := StringReplace(imgs, 'var pages =', '', [rfIgnoreCase]);
          imgs := Trim(TrimChar(imgs, [';', ' ']));
          Break;
        end;
    end;
    l.Free;

    with manager.container do
    begin
      if imgs <> '' then
      begin
        PageLinks.Clear;
        ParseJSONArray(imgs, 'image', PageLinks);
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
