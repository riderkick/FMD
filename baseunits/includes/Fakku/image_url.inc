  function GetFakkuImageURL: Boolean;
  var
    i, totalPages: Cardinal;
    l: TStringList;
    imgURL, imgExt, s: String;

  begin
    totalPages := 0;
    manager.container.PageLinks.Clear;
    l := TStringList.Create;
    s := FillMangaSiteHost(FAKKU_ID, URL);
    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);

    if l.Count > 0 then
    begin
      //Get total pages
      for i := 0 to l.Count - 1 do
      begin
        if (Pos('window.params.thumbs = ', l.strings[i]) > 0) then
        begin
          s := GetString(l.Strings[i], '= [', '];');
          with TStringList.Create do
            try
              DelimitedText := s;
              totalPages := Count;
            finally
              Free;
            end;
          Break;
        end;
      end;

      if totalPages = 0 then
      begin
        Result := False;
      end;

      //Get imgurl
      for i := 0 to l.Count - 1 do
      begin
        if (Pos('return ''//', l.Strings[i]) > 0) then
        begin
          imgURL := GetString(l.Strings[i], 'return ''//', ''' +');
          imgExt := GetString(l.Strings[i], 'x + ''', ''';');
          Break;
        end;
      end;

      //Build image links
      if imgExt = '' then
        imgExt := '.jpg';

      if imgURL <> '' then
      begin
        imgURL := TrimLeftChar(imgURL, ['/', '\', ':']);
        imgURL := 'http://' + imgURL;
        for i := 1 to totalPages do
        begin
          s := IntToStr(i);
          while Length(s) < 3 do
            s := '0' + s;
          manager.container.PageLinks.Add(imgURL + s + imgExt);
        end;
      end;
    end;
    l.Free;
  end;
