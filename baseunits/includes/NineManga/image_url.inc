  function GetNineMangaImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(manager.container.MangaSiteID, URL);
    s := StringReplace(s, '.html', '', []);
    s := StringReplace(s, '.htm', '', []);
    s := s + '-' + IntToStr(workCounter + 1) + '.html';

    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);
    if Self.Terminated then
    begin
      l.Free;
      Exit;
    end;

    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if (Pos('<img', parse.Strings[i]) > 0) and
          (Pos('class="manga_pic', parse.Strings[i]) > 0) then
        begin
          manager.container.PageLinks.Strings[workCounter] :=
            GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
