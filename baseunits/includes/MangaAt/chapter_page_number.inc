  function GetMangaAtPageNumber: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
    isExtractPage: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MANGAAT_ID, URL + '/1/'));
    Result := GetPage(TObject(l),
      s,
      manager.container.manager.retryConnect);

    Parser := THTMLParser.Create(l.Text);
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
    finally
      Parser.Free;
    end;

    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if (GetTagName(parse[i]) = 'a') and (GetVal(parse[i], 'class') = 'active') then
          isExtractPage := True;
        if isExtractPage and (GetTagName(parse[i]) = 'a') then
          if GetVal(parse[i], 'class') = 'chapter' then
            Break
          else
            Inc(manager.container.PageNumber);
      end;
    end;
    parse.Free;
    l.Free;
  end;
