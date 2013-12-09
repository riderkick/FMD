function   MangaInnGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAINN_ID,1] + MANGAINN_BROWSER, 0) then
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
      if (GetTagName(parse.Strings[i]) = 'li') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='mangalistItems') then
      begin
        Result:= NO_ERROR;
        s:= TrimRight(StringFilter(parse.Strings[i+2]));
      //  if s <> 'Hajimete no Aku' then
        begin
          names.Add(s);
          links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), WebsiteRoots[MANGAINN_ID,1], '', []));
        end;
      end;
    end;
    source.Free;
  end; 