  function GetMangaStreamImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := DecodeUrl(URL + '/' + IntToStr(workCounter + 1));
    if not (Pos('http', LowerCase(s)) = 1) then
      s := MANGASTREAM_ROOT2 + s;
    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);
    if Self.Terminated then
    begin
      l.Free;
      Exit;
    end;

    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if (Pos('id="manga-page"', parse.Strings[i]) > 0) then
        begin
          manager.container.PageLinks.Strings[workCounter] :=
            GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
