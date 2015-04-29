  function GetMangaMintImageURL: Boolean;
  var
    s: String;
    i, j: Integer;
    l, ts: TStringList;
    isExtractImage: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    try
      s := FillMangaSiteHost(MANGAMINT_ID, URL);
      if workCounter > 0 then
       s := s + '?page=' + IntToStr(workCounter);
      Result := GetPage(TObject(l),
        s,
        manager.container.Manager.retryConnect);

      Parser := THTMLParser.Create(PChar(l.Text));
      try
        Parser.OnFoundTag := OnTag;
        Parser.OnFoundText := OnText;
        Parser.Exec;
      finally
        Parser.Free;
      end;

      if parse.Count > 0 then
        for i := 0 to parse.Count - 1 do
        begin
          if (GetTagName(parse[i]) = 'div') and (GetVal(parse[i], 'id') = 'images') then
            isExtractImage := True;
          if isExtractImage and (GetTagName(parse[i]) = 'img') then
          begin
            if Pos('src =', parse[i]) > 0 then
              parse[i] := StringReplace(parse[i], 'src =', 'src=', []);
            manager.container.PageLinks[workCounter] := GetVal(parse[i], 'src');
            Break;
          end;
        end;
    finally
      parse.Free;
      l.Free;
    end;
  end;
