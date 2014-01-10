function   MangacanGetNamesAndLinks: Byte;
var
  tmp: Integer;
  i: Cardinal;
  s: String;
begin
  Result:= INFORMATION_NOT_FOUND;
  if NOT GetPage(TObject(source), WebsiteRoots[MANGACAN_ID,1] + MANGACAN_BROWSER, 0) then
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
    if (GetTagName(parse.Strings[i]) = 'a') AND
       (Pos('baca-komik-', parse.Strings[i])>0) then
    begin
      Result:= NO_ERROR;
      s:= StringFilter(parse.Strings[i+1]);
      names.Add(HTMLEntitiesFilter(s));
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[MANGACAN_ID,1], '', []);
      links.Add(s);
    end;
  end;
  source.Free;
end;
