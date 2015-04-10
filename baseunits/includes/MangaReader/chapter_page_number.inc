  function GetMangaReaderPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(MANGAREADER_ID, URL);
    Result := GetPage(TObject(l), s, manager.container.manager.retryConnect);
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
        if (Pos('</select>', parse.Strings[i]) > 0) and
          (Pos('</div>', parse.Strings[i + 2]) > 0) then
        begin
          s := parse.Strings[i + 1];
          Delete(s, Pos(' of ', s), 4);
          manager.container.PageNumber := StrToInt(TrimLeft(TrimRight(s)));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
