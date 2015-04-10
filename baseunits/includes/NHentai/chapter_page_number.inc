  function GetNHentaiPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    s: String;
  begin
    manager.container.PageNumber := 0;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(NHENTAI_ID, URL);
    if Length(s) > 0 then
      if s[Length(s)] <> '/' then
         s := s + '/';
    Result := GetPage(TObject(l), s + '1/', manager.container.Manager.retryConnect);

    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    l.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<a', parse[i]) > 0) and (Pos('class="last"', parse[i]) > 0) and
          (Pos('/g/', parse[i]) > 0) then
        begin
          manager.container.PageNumber :=
            StrToIntDef(ReplaceRegExpr('^.*\/(\d+)\/.*$', GetVal(parse[i], 'href'), '$1', True), 1);
          Break;
        end;
      end;
    end;
    parse.Free;
  end;
