  function GetTruyenTranhTuanImageURL: Boolean;
  var
    s, s2: String;
    j, i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(TRUYENTRANHTUAN_ID, URL),
      Task.Container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
      begin
        if Pos('var slides_page = ["', parse[i]) > 0 then
        begin
          s := parse[i];
          repeat
            j := Pos('"/manga/', s);
            s2 := EncodeUrl(WebsiteRoots[TRUYENTRANHTUAN_ID, 1] + '/manga/' +
              GetString(s, '"/manga/', '"'));
            Task.Container.PageLinks.Add(s2);
            Delete(s, Pos('"/manga/', s), 10);
            j := Pos('"/manga/', s);
          until j = 0;
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
