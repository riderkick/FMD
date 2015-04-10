  function GetBlogTruyenImageURL: Boolean;
  var
    isExtrackLink: Boolean = False;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(BLOGTRUYEN_ID, URL),
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
      begin
        if not (isExtrackLink) and (Pos('id="content"', parse.Strings[i]) > 0) then
          isExtrackLink := True;
        if (isExtrackLink) and (GetTagName(parse.Strings[i]) = 'img') then
          manager.container.PageLinks.Add(
            EncodeUrl(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='))))
        else
        if (isExtrackLink) and (Pos('</article>', parse.Strings[i]) > 0) then
          Break;
      end;
    end;
    parse.Free;
    l.Free;
  end;
