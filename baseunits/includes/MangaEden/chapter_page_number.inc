  function GetMangaEdenPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    if manager.container.MangaSiteID = MANGAEDEN_ID then
      s := FillMangaSiteHost(MANGAEDEN_ID, URL) + '1/'
    else
      s := FillMangaSiteHost(PERVEDEN_ID, URL) + '1/';
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
      manager.container.pageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('span class="next"', parse.Strings[i]) > 0) then
        begin
          s := parse.Strings[i - 3];
          manager.container.PageNumber := StrToInt(TrimLeft(TrimRight(s)));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
