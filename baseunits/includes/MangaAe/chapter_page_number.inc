  function GetMangaAePageNumber: Boolean;
  var
    s: String;
    i, p: LongInt;
    l: TStringList;
    isExtractPage: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MANGAAE_ID, URL) + '/1');
    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<a', parse[i]) <> 0) and (Pos('class="active"', parse[i]) <> 0) then
          isExtractPage := True;
        if isExtractPage and (Pos('</div', parse[i]) <> 0) then
          Break;
        if isExtractPage and
          (Pos('<a', parse[i]) <> 0) and (Pos(URL, parse[i]) <> 0) then
        begin
          p := StrToIntDef(Trim(parse[i + 1]), 0);
          if p > manager.container.PageNumber then
            manager.container.PageNumber := p;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
