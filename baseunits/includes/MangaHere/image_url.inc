  function GetMangaHereImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    if workCounter > 0 then
      Result := GetPage(TObject(l),
        FillMangaSiteHost(MANGAHERE_ID, URL) +
        IntToStr(workCounter + 1) + '.html',
        manager.container.Manager.retryConnect)
    else
      Result := GetPage(TObject(l),
        FillMangaSiteHost(MANGAHERE_ID, URL),
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
        if Pos('class="read_img"', parse.Strings[i]) <> 0 then
        begin
          manager.container.PageLinks.Strings[workCounter] :=
            GetAttributeValue(GetTagAttribute(parse.Strings[i + 4], 'src'));
          parse.Free;
          l.Free;
          Exit;
        end;
      //for c:= 'a' to 'z' do
      //  if (Pos('http://'+c+'.mhcdn.net/store/', parse.Strings[i])<>0) then
      //  begin
      //    manager.container.PageLinks.Strings[workCounter]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
      //    parse.Free;
      //    l.Free;
      //    exit;
      //  end;
    end;
    parse.Free;
    l.Free;
  end;
