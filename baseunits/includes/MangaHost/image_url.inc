  function GetMangaHostImageURL: Boolean;
  var
    i: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      DecodeUrl(FillMangaSiteHost(MANGAHOST_ID, URL) + '/' + IntToStr(workCounter + 1)),
      manager.container.manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if (Pos('<img', parse[i]) > 0) and (Pos('class="open', parse[i]) > 0) then
        begin
          manager.container.PageLinks.Strings[workCounter] := GetVal(parse[i], 'src');
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
