  function GetMangaInnImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(MANGAINN_ID, URL) +
      '/page_' + IntToStr(WorkId + 1),
      Task.Container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if GetTagName(parse[i]) = 'img' then
          if GetVal(parse[i], 'id') = 'imgPage' then
          begin
            Task.Container.PageLinks[WorkId] :=
              GetVal(parse[i], 'src');
            Break;
          end;
    end;
    parse.Free;
    l.Free;
  end;
