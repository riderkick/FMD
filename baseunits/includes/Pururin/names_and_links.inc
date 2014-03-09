function   PururinGetNamesAndLinks: Byte;
var
  tmp, numPage: Integer;
  i: Cardinal;
  s: String;
begin
  Result:= INFORMATION_NOT_FOUND;
  numPage:= StrToInt(URL);
  if NOT GetPage(TObject(source), WebsiteRoots[PURURIN_ID,1] + PURURIN_BROWSER + '0/1' + IntToStr(numPage) + '/' + IntToStr(numPage+1) + '.html' , 0) then
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
    if (Pos('class="overlay', parse.Strings[i]) > 0) then
    begin
      Result:= NO_ERROR;
      s:= StringFilter(parse.Strings[i+7]);
      names.Add(HTMLEntitiesFilter(s));
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i-2], 'href=')), WebsiteRoots[PURURIN_ID,1], '', []);
      links.Add(s);
    end;
  end;
  source.Free;
end;
