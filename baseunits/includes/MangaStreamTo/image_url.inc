  function GetMangaStreamToImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := StringReplace(URL, '.html', '', []);
    s := s + '-page-' + IntToStr(workCounter + 1) + '.html';
    s := FillMangaSiteHost(MANGASTREAMTO_ID, s);
    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if (Pos('class="manga-page"', parse.Strings[i]) > 0) and
          (Pos('<img', parse.Strings[i]) > 0) then
        begin
          if not Terminated then
            manager.container.PageLinks.Strings[workCounter] :=
              GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
