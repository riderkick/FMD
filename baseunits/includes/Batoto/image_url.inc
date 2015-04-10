  function GetBatotoImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
    s: String;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(BATOTO_ID, URL);
    Result := GetPage(TObject(l), s + '/' + IntToStr(workCounter + 1),
      manager.container.manager.retryConnect);

    if l.Count > 0 then
    begin
      Parser := THTMLParser.Create(PChar(l.Text));
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
      Parser.Free;
    end;

    if Self.Terminated then
    begin
      parse.Free;
      l.Free;
      Exit;
    end;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<img', parse.Strings[i]) > 0) and
          (Pos('id="comic_page"', parse.Strings[i]) > 0) then
        begin
          s := GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          manager.container.PageLinks.Strings[workCounter] := s;
          Break;
        end;
      end;
    end;

    parse.Free;
    l.Free;
  end;
