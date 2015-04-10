  function GetKivmangaPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(KIVMANGA_ID, URL) + '/1');
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
        if (Pos('title="Next Page"', parse.Strings[i]) > 0) then
        begin
          s := parse.Strings[i - 6];
          manager.container.PageNumber := StrToInt(GetString(s, '"', '"'));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
