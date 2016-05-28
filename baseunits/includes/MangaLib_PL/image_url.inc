  function GetMangaLib_PLImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
    cf: Boolean = False;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(MANGALIB_PL_ID, URL) + ',' + IntToStr(workCounter + 1);

    Result := GetPage(TObject(l),
      s,
      Task.Container.Manager.retryConnect);

    //check confirm
    if l.Count > 0 then
    begin
      for i := 0 to l.Count - 1 do
      begin
        if (Pos('<form ', l[i]) > 0) and
          (Pos('name="confirm_', l[i]) > 0) then
        begin
          if (Length(URL) > 1) and
            (URL[1] = '/') then
            s := Copy(URL, 2, Length(URL) - 1)
          else
            s := URL;
          s := FillMangaSiteHost(MANGALIB_PL_ID, '/page/' +
            GetVal(l[i], 'name') + '?backlink=' + s);
          cf := True;
          Break;
        end;
      end;
      if cf then
      begin
        MANGALIB_PL_COOKIES := FHTTP.Cookies.Text;
        l.Clear;
        Result := GetPage(TObject(l),
          s,
          Task.Container.Manager.retryConnect);
      end;
    end;

    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if (Pos('<img ', parse[i]) > 0) and
          (Pos('id="img_curr"', parse[i]) > 0) then
        begin
          Task.Container.PageLinks[workCounter] := GetVal(parse[i], 'src');
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
