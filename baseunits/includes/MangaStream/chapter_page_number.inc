  function GetMangaStreamPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MANGASTREAM_ID, URL) + '/1');
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
      manager.container.pageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('Last Page (', parse.Strings[i]) > 0) then
        begin
          manager.container.PageNumber :=
            StrToInt(TrimLeft(TrimRight(GetString(parse.Strings[i], 'Last Page (', ')'))));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
