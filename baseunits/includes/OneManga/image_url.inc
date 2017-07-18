  function GetOneMangaImageURL: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(ONEMANGA_ID, URL);
    s := s + IntToStr(WorkId + 1) + '/';
    Result := GetPage(TObject(l),
      s,
      Task.Container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        if (Pos('<img ', parse[i]) > 0) and
          (Pos('class="manga-page"', parse[i]) > 0) then
        begin
          Task.Container.PageLinks[WorkId] := GetVal(parse[i], 'src');
          Break;
        end;
    parse.Free;
    l.Free;
  end;
