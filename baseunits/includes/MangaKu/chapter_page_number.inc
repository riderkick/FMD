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
      Task.Container.manager.retryConnect);

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
      Task.Container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
        if (GetTagName(parse[i]) = 'a') and (GetVal(parse[i], 'imageanchor') = '1') then
          if GetTagName(parse[i + 1]) = 'img' then
            Task.Container.PageLinks.Add(GetVal(parse[i + 1], 'src'));
      Task.Container.PageNumber := Task.container.PageLinks.Count;
    end;
    parse.Free;
    l.Free;
  end;
