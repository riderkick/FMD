  function GetMangaArPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(MANGAAR_ID, URL) + '/1';
    Result := GetPage(TObject(l),
      s,
      manager.container.manager.retryConnect);

    // convert charset
    l.Text := CP1256ToUTF8(l.Text);

    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('</select>', parse.Strings[i]) > 0) then
        begin
          s := TrimLeft(TrimRight(parse.Strings[i - 3]));
          manager.container.PageNumber := StrToInt(s);
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
