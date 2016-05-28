  function GetScanMangaImageURL: Boolean;
  var
    s, h: String;
    i, j: Cardinal;
    l, t: TStringList;

  begin
    l := TStringList.Create;
    t := TStringList.Create;

    s := DecodeUrl(FillMangaSiteHost(SCANMANGA_ID, URL));
    Result := GetPage(TObject(l),
      s,
      Task.Container.Manager.retryConnect);

    for i := 0 to l.Count - 1 do
    begin
      if (Pos('$(''#image_lel'')', l[i]) > 0) and
        (Pos('[id_page]', l[i]) > 0) then
      begin
        s := Trim(l[i]);
        s := GetString(s, '''src'',''', '''+');
        h := s;
        Break;
      end;
    end;
    Task.Container.PageLinks.Clear;
    for i := 0 to l.Count - 1 do
    begin
      if (Pos('var c = new Array;', l[i]) > 0) and
        (Pos(';check = false;', l[i]) > 0) then
      begin
        s := Trim(l[i]);
        s := GetString(s, 'var c = new Array;', ';check = false;');
        t.Delimiter := ';';
        t.DelimitedText := s;
        for j := 0 to t.Count - 1 do
          if Pos(']="', t[j]) > 0 then
            Task.Container.PageLinks.Add(h + GetString(t[j], '="', '"'));
        Break;
      end;
    end;
    t.Free;
    l.Free;
  end;
