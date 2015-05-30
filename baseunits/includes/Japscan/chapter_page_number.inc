  function GetJapscanPageNumber: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
    regx: TRegExpr;
    isGetPageNumber: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(JAPSCAN_ID, URL));
    if Pos('.htm', s) > 0 then
      s := ReplaceRegExpr('/\d+\.html?$', s, '', False);
    Result := GetPage(TObject(l),
      s,
      manager.container.manager.retryConnect);

    CleanHTMLComments(l);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    manager.container.PageNumber := 0;
    if parse.Count > 0 then
    begin
      regx := TRegExpr.Create;
      try
        regx.Expression := '^.*/(\d+)\.html?.*$';
        regx.ModifierI := True;
        for i := 0 to parse.Count - 1 do
        begin
          if (GetTagName(parse[i]) = 'nav') and
          (GetVal(parse[i], 'id') = 'pagination') then
            isGetPageNumber := True;
          if isGetPageNumber and ((GetTagName(parse[i]) = '/nav') or
            (GetVal(parse[i], 'id') = 'next_link') or
            (GetVal(parse[i], 'id') = 'next_chapter')) then
            Break;
          if isGetPageNumber and (GetTagName(parse[i]) = 'a') then
          begin
            s := GetVal(parse[i], 'href');
            s := regx.Replace(s, '$1', True);
            if manager.container.PageNumber < StrToIntDef(s, 0) then
              manager.container.PageNumber := StrToIntDef(s, 0);
          end;
        end;
      finally
        regx.Free;
      end;
    end;
    parse.Free;
    l.Free;
  end;
