  function GetPururinPageNumber: Boolean;
  var
    s: String;
    i, g: Cardinal;
    l: TStringList;
    isStartGetPageNumber: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(PURURIN_ID, URL);
    s := StringReplace(s, '_1.html', '.html', []);
    s := StringReplace(s, '/view/', '/gallery/', []);
    s := DecodeUrl(StringReplace(s, '/00/', '/', []));
    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);
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
        if (Pos('class="square"', parse.Strings[i]) > 0) then
          isStartGetPageNumber := True;

        if (isStartGetPageNumber) and
          (Pos('class="square"', parse.Strings[i]) > 0) then
        begin
          s := parse.Strings[i + 1];
          g := length(s);
          Delete(s, g - 10, g - 3);
          Delete(s, 1, 9);
          g := StrToInt(s);
          manager.container.PageNumber := g;
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
