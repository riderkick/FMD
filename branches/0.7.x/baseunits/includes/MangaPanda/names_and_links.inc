function   MangaPandaGetNamesAndLinks: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
    isExtractInfo: Boolean = FALSE;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAPANDA_ID,1] + MANGAPANDA_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (NOT isExtractInfo) AND (Pos('ul class="series_alpha"', parse.Strings[i]) > 0) then
        isExtractInfo:= TRUE;
      if (isExtractInfo) AND
         (Pos('<li>', parse.Strings[i]) > 0) AND
         (Pos('<a', parse.Strings[i+1]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+2])));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href="'));
        s:= StringReplace(s, WebsiteRoots[MANGAPANDA_ID,1], '', []);
        links.Add(s);
      end
      else
      if (isExtractInfo) AND
         (Pos('div id="wrapper_footer"', parse.Strings[i]) > 0) then
        break;
    end;
    source.Free;
  end;    