  function GetMangaAtImageURL: Boolean;
  var
    s: String;
    i, j: Integer;
    l, ts: TStringList;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(MANGAAT_ID, URL) +
      '/' + IntToStr(workCounter + 1) + '/';
    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);

    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
    finally
      Parser.Free;
    end;

    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        if GetVal(parse[i], 'id') = 'showchaptercontainer' then
          if (GetTagName(parse[i + 3]) = 'img') then
          begin
            manager.container.PageLinks[workCounter] := GetVal(parse[i + 3], 'src');
            Break;
          end;
    parse.Free;
    l.Free;
  end;
