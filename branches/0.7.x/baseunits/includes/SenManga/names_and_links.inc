function   SenMangaGetNamesAndLinks: Byte;
var
  tmp: Integer;
  i: Cardinal;
  s: String;
begin
  Result:= INFORMATION_NOT_FOUND;
  if NOT GetPage(TObject(source), WebsiteRoots[SENMANGA_ID,1] + SENMANGA_BROWSER, 0) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.SlowExec;
  Parser.Free;
  if parse.Count=0 then
  begin
    source.Free;
    exit;
  end;
  for i:= 0 to parse.Count-1 do
  begin
    if ((Pos('class=''even'' style=''pad', parse.Strings[i]) > 0) OR
        (Pos('class=''odd'' style=''pad', parse.Strings[i]) > 0)) AND
       (Pos('</tr>', parse.Strings[i-1]) > 0) then
    begin
      Result:= NO_ERROR;
      s:= StringFilter(parse.Strings[i+3]);
      names.Add(HTMLEntitiesFilter(s));
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'href=')), WebsiteRoots[SENMANGA_ID,1], '', []);
      links.Add(s);
    end;
  end;
  source.Free;
end;
  
