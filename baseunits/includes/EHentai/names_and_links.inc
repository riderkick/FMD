  function EHentaiGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s, posc: String;
    isThumbnailsON: Boolean = False;

  begin

    Result := INFORMATION_NOT_FOUND;
    //s:='http://g.e-hentai.org/?page=5377&f_doujinshi=on&f_manga=on&f_western=on&f_search=Search+Keywords&f_apply=Apply+Filter';
    if URL = '0' then
      s := WebsiteRoots[EHENTAI_ID, 1] + '/?' + EHENTAI_BROWSER
    else
      s := WebsiteRoots[EHENTAI_ID, 1] + '/?page=' + URL + '&' + EHENTAI_BROWSER;

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

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('/?inline_set=dm_l', parse.Strings[i]) > 0) then
        begin
          isThumbnailsON := True;
          Break;
        end;
      end;

      if isThumbnailsON then
        posc := 'class="id2"'
      else
        posc := 'class="it5"';

      for i := 0 to parse.Count - 1 do
      begin
        if (Pos(posc, parse.Strings[i]) > 0) then
        begin
          Result := NO_ERROR;
          s := StringFilter(parse.Strings[i + 2]);
          names.Add(HTMLEntitiesFilter(s));
          s := GetAttributeValue(GetTagAttribute(parse.Strings[i + 1], 'href='));
          s := StringReplace(s, WebsiteRoots[EHENTAI_ID, 1], '', []);
          links.Add(s);
        end;
      end;
    end;
    Source.Free;
  end;