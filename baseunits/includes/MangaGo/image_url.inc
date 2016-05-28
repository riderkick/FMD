  function GetMangaGoImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
    regx: TRegExpr;
    s: String;
  begin
    l := TStringList.Create;
    regx := TRegExpr.Create;
    regx.Expression := '^(.*/pg-)\d+/$';
    s := regx.Replace(URL, '$1', True);
    //regx.Expression := '\.html?$';
    regx.Expression := '\-$';
    if (not regx.Exec(s)) and (s[Length(s)] <> '/') then
      s := s + '/';
    s := FillMangaSiteHost(MANGAGO_ID, s);
    Result := GetPage(TObject(l), s + IntToStr(workCounter + 1) + '/',
      Task.Container.Manager.retryConnect);

    if Self.Terminated then
    begin
      l.Free;
	  parse.Free;
      regx.Free;
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
        if (Pos('imgReady(''', parse[i]) > 0) then
        begin
          regx.Expression := '^.*imgReady\(''([^'']*)''.*$';
          Task.Container.PageLinks[workCounter] := regx.Replace(parse[i], '$1', True);
          Break;
        end;
    end;
    regx.Free;
    parse.Free;
    l.Free;
  end;
