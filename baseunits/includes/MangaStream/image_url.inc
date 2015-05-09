  function GetMangaStreamImageURL: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := URL + '/' + IntToStr(workCounter + 1);
    if Pos('http', LowerCase(s)) <> 1 then
      s := MANGASTREAM_ROOT2 + s;

    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);

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
        if (GetTagName(parse[i]) = 'img') and (GetVal(parse[i], 'id') = 'manga-page') then
        begin
          manager.container.PageLinks.Strings[workCounter] := GetVal(parse[i], 'src');
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
