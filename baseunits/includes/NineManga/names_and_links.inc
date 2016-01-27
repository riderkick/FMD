  function NineMangaGetNamesAndLinks: Byte;
  var
    SiteID, i: Cardinal;

  begin
    Result := INFORMATION_NOT_FOUND;
    SiteID := GetMangaSiteID(website);
    if not GetPage(TObject(Source), WebsiteRoots[SiteID, 1] +
      NINEMANGA_BROWSER + '&page=' + IntToStr(StrToIntDef(URL, 0) + 1) + '.html', 0) then
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
    if parse.Count = 0 then
    begin
      Source.Free;
      Exit;
    end;
    for i := 0 to parse.Count - 1 do
    begin
      if (Pos('class="bookname"', parse[i]) > 0) then
      begin
        Result := NO_ERROR;
        links.Add(
          StringReplace(GetVal(parse[i], 'href'),
          WebsiteRoots[SiteID, 1], '', [rfReplaceAll]));
        names.Add(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1]))));
      end;
    end;
    Source.Free;
  end;