  function GetMangaKuPageNumber: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MANGAKU_ID, URL));
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
      manager.container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
        if (GetTagName(parse[i]) = 'a') and (GetVal(parse[i], 'imageanchor') = '1') then
          if GetTagName(parse[i + 1]) = 'img' then
            manager.container.PageLinks.Add(GetVal(parse[i + 1], 'src'));
      manager.container.PageNumber := manager.container.PageLinks.Count;
    end;
    parse.Free;
    l.Free;
  end;
