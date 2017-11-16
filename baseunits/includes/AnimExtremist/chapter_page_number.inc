  function GetAnimeExtremistPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    Result := GetPage(TObject(l),
      StringReplace(FillMangaSiteHost(ANIMEEXTREMIST_ID, URL),
        '.html', '', []) + '-1.html',
      Task.Container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := @OnTag;
    Parser.OnFoundText := @OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if Pos('</select>', parse[i]) > 0 then
        begin
          Task.Container.PageNumber :=
            StrToInt(GetString(TrimLeft(TrimRight(parse[i - 3] + '~!@')), 'Pagina ', '~!@'));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
