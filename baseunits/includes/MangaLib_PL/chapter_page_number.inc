  function GetMangaLib_PLPageNumber: Boolean;
  var
    s: String;
    i: Cardinal = 0;
    l: TStringList;
    cf: Boolean = False;
    isExtractPage: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MANGALIB_PL_ID, URL));
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
          s := FillMangaSiteHost(MANGALIB_PL_ID, '/page/' + GetVal(l[i], 'name') +
            '?backlink=' + s);
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

    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := @OnTag;
    Parser.OnFoundText := @OnText;
    Parser.Exec;
    Parser.Free;
    Task.Container.PageNumber := 0;
    if parse.Count > 0 then
    begin
      Task.Container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<select ', parse[i]) > 0) and
          (Pos('class="ch_page"', parse[i]) > 0) then
          isExtractPage := True;
        if isExtractPage and
          (Pos('</select', parse[i]) > 0) then
        begin
          isExtractPage := False;
          Break;
        end;
        if isExtractPage and
          (Pos('<option ', parse[i]) > 0) then
          Inc(Task.Container.PageNumber);
      end;
    end;
    parse.Free;
    l.Free;
  end;
