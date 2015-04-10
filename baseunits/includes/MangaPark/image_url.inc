  function GetMangaParkImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(MANGAPARK_ID, URL) +
      'all',//IntToStr(workCounter+1),
      manager.container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      manager.container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
        // if GetTagName(parse.Strings[i]) = 'img' then
        if (Pos('a target="_blank"', parse.Strings[i]) > 0) then
        begin
          manager.container.PageLinks.Add(GetAttributeValue(
            GetTagAttribute(parse.Strings[i], 'href=')));
          //    break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
