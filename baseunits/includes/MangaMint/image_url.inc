  function GetMangaMintImageURL: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
    isExtractImage: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    try
      s := FillMangaSiteHost(MANGAMINT_ID, URL);
      if Task.Container.WorkCounter > 0 then
       s := s + '?page=' + IntToStr(Task.Container.WorkCounter);
      Result := GetPage(TObject(l),
        s,
        Task.Container.Manager.retryConnect);

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
            Task.Container.PageLinks[Task.Container.WorkCounter] := GetVal(parse[i], 'src');
            Break;
          end;
        end;
    finally
      parse.Free;
      l.Free;
    end;
  end;
