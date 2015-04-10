  function GetHugeMangaPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(HUGEMANGA_ID, URL) + '/1');
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
      for i := parse.Count - 1 downto 5 do
      begin
        if (Pos('</option>', parse.Strings[i]) > 0) then
        begin
          s := parse.Strings[i - 2];
          manager.container.pageNumber :=
            StrToInt(GetAttributeValue(GetTagAttribute(s, 'value=')));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
