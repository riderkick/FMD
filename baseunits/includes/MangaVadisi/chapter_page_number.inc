  function GetMangaVadisiPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MANGAVADISI_ID, MANGAVADISI_BROWSER + URL) + '/1');
    Result := GetPage(TObject(l),
      s,
      manager.container.manager.retryConnect);
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
        if (Pos('title="Sonraki Sayfa"', parse[i]) > 0) then
        begin
          s := parse[i - 6];
          manager.container.PageNumber := StrToInt(GetString(s, '"', '"'));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
