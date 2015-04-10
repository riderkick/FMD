  function GetMangaStreamToPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(MANGASTREAMTO_ID, URL);
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
      for i := parse.Count - 1 downto 0 do
      begin
        if (Pos('option value=', parse.Strings[i]) > 0) then
        begin
          try
            manager.container.PageNumber :=
              StrToInt(Trim(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value='))));
          except
            manager.container.PageNumber := 0;
          end;
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
