  function GetMangaPandaPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MANGAPANDA_ID, URL));
    if (Pos('.html', URL) > 0) and (Pos(SEPERATOR2, URL) > 0) then
      s := StringReplace(s, SEPERATOR2, '-1/', []);
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
      for i := 1 to parse.Count - 1 do
      begin
        if (Pos(' of ', parse.Strings[i]) > 0) and
          (Pos('select', parse.Strings[i - 1]) > 0) then
        begin
          s := GetString(parse.Strings[i] + '~!@', ' of ', '~!@');
          manager.container.PageNumber := StrToInt(TrimLeft(TrimRight(s)));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
