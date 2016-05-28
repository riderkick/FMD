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
      Task.Container.manager.retryConnect);

    ParseHTML(l.Text, parse);
    if parse.Count > 0 then
    begin
      Task.Container.PageLinks.Clear;
      for i := 0 to parse.Count-1 do
        if (GetTagName(parse[i]) = 'img') and
          (Pos('img-responsive-', parse[i]) <> 0) then
          Task.Container.PageLinks.Add(GetVal(parse[i], 'src'));
      Task.Container.PageNumber := Task.Container.PageLinks.Count;
    end;
    parse.Free;
    l.Free;
  end;
