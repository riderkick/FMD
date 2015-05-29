  function GetReadMangaTodayPageNumber: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(READMANGATODAY_ID, URL)+'/all-pages');
    Result := GetPage(TObject(l),
      s,
      manager.container.manager.retryConnect);

    parse.Text := ParseHTML(l.Text);
    if parse.Count > 0 then
    begin
      manager.container.PageLinks.Clear;
      for i := 0 to parse.Count-1 do
        if (GetTagName(parse[i]) = 'img') and
          (Pos('img-responsive-2', GetVal(parse[i], 'class')) <> 0) then
          manager.container.PageLinks.Add(GetVal(parse[i], 'src'));
      manager.container.PageNumber := manager.container.PageLinks.Count;
    end;
    parse.Free;
    l.Free;
  end;
