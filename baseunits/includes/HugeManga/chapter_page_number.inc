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
      Task.Container.manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      Task.Container.pageNumber := 0;
      for i := parse.Count - 1 downto 5 do
      begin
        if (Pos('</option>', parse[i]) > 0) then
        begin
          s := parse[i - 2];
          Task.Container.pageNumber :=
            StrToInt(GetVal(s, 'value'));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
