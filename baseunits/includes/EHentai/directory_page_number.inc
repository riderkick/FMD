  function GetEHentaiDirectoryPageNumber: Byte;
  var
    i, g: Cardinal;
    regx: TRegExpr;
  begin

    Result := INFORMATION_NOT_FOUND;
    s := WebsiteRoots[EHENTAI_ID, 1] + '/?' + EHENTAI_BROWSER;
    //s:= s+'?nw=session';

    if not GetPage(TObject(Source), s, 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    Page := 0;
    g := 0;
    regx := TRegExpr.Create;
    regx.Expression := '^.*onclick="sp\((\d+)\).*$';
    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('onclick="sp(', parse.Strings[i]) > 0) then
        begin
          g := StrToIntDef(regx.Replace(parse.Strings[i], '$1', True), 0);
          if g > Page then
            Page := g;
        end;
      end;
    regx.Free;

    Page := Page + 1; //page1=0

    Source.Free;
    Result := NO_ERROR;
  end;