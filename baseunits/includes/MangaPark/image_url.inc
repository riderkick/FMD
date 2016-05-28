  function GetMangaParkImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(MANGAPARK_ID, URL) +
      'all',//IntToStr(Task.Container.WorkCounter+1),
      Task.Container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      Task.Container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
        // if GetTagName(parse[i]) = 'img' then
        if (Pos('a target="_blank"', parse[i]) > 0) then
        begin
          Task.Container.PageLinks.Add(GetVal(parse[i], 'href'));
          //    break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
