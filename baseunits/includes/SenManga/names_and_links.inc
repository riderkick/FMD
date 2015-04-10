  function SenMangaGetNamesAndLinks: Byte;
  var
    i: LongInt;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[SENMANGA_ID, 1] +
      SENMANGA_BROWSER + '?order=title&page=' + IntToStr(StrToInt(URL) + 1) , 0) then
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
    
    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (GetTagName(parse[i]) = 'span') and (GetVal(parse[i], 'class') = 'title') then
        begin
          links.Add(RemoveHostFromURL(GetVal(parse[i - 2], 'href')));
          names.Add(CommonStringFilter(parse[i + 1]));
        end;
      end;
      Result := NO_ERROR;
    end;
    Source.Free;
  end;
