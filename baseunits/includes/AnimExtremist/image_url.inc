  function GetAnimeExtremistImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := DecodeUrl(StringReplace(FillMangaSiteHost(ANIMEEXTREMIST_ID, URL),
      '.html', '', []) + '-' + IntToStr(workCounter + 1) + '.html');
    Result := GetPage(TObject(l),
      s,
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
        if (Pos('id="photo"', parse.Strings[i]) > 0) then
        begin
          s := GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          manager.container.pageLinks.Strings[workCounter] :=
            GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
