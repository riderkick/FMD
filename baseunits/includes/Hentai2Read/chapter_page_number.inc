  function GetHentai2ReadPageNumber: Boolean;
  var
    i, j: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(HENTAI2READ_ID, URL),
      manager.container.Manager.retryConnect);
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
        if (GetTagName(parse[i]) = 'select') and
          (GetVal(parse[i], 'class') =
          'cbo_wpm_pag') then
        begin
          j := i + 1;
          while GetTagName(parse[j]) = 'option' do
          begin
            Inc(manager.container.PageNumber);
            Inc(j, 3);
          end;
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
