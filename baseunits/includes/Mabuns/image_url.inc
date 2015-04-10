  function GetMabunsImageURL: Boolean;
  var
    s: String;
    j, i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(MABUNS_ID, URL);
    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
      begin
        if Pos('addpage(''', parse.Strings[i]) > 0 then
        begin
          s := parse.Strings[i];
          s := StringReplace(s, 'https://', 'http://', [rfReplaceAll]);
          repeat
            j := Pos('addpage(''', s);
            if Pos('googleusercontent', s) > 0 then
              manager.container.PageLinks.Add(
                EncodeUrl(GetString(s, 'addpage(''', ''',')))
            else
              manager.container.PageLinks.Add(
                EncodeUrl(GetString(s, 'addpage(''', ');')));
            Delete(s, Pos('addpage(''', s), 16);
            j := Pos('addpage(''', s);
          until j = 0;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
