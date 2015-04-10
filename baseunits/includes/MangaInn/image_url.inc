  function GetMangaInnImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(MANGAINN_ID, URL) +
      '/page_' + IntToStr(workCounter + 1),
      manager.container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if GetTagName(parse.Strings[i]) = 'img' then
          if GetAttributeValue(GetTagAttribute(parse.Strings[i], 'id=')) = 'imgPage' then
          begin
            manager.container.PageLinks.Strings[workCounter] :=
              GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
            Break;
          end;
    end;
    parse.Free;
    l.Free;
  end;
