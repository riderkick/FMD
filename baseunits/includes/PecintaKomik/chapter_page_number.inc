  function GetPecintaKomikPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(PECINTAKOMIK_ID, URL) + '/1');
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
      for i := parse.Count - 1 downto 5 do
      begin
        if (Pos('</option>', parse.Strings[i]) > 0) then
        begin
          s := parse.Strings[i - 1];
          manager.container.pageNumber := StrToInt(TrimLeft(TrimRight(s)));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
