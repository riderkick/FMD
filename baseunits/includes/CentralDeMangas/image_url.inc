  function GetCentralDeMangasImageURL: Boolean;
  var
    s: String;
    j, i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := EncodeUrl(FillMangaSiteHost(CENTRALDEMANGAS_ID, URL));
    // + IntToStr(workCounter+1));
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
    begin
      Task.Container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
      begin
        if Pos('var pages = ', parse[i]) > 0 then
        begin
          s := StringReplace(parse[i], '\/', '/', [rfReplaceAll]);
          repeat
            j := Pos('http://', s);
            Task.Container.PageLinks.Add(EncodeURL(GetString(s, '"', '"')));
            s := StringReplace(s, '"', '', []);
            s := StringReplace(s, '"', '', []);
            Delete(s, j, 10);
            j := Pos('http://', s);
          until j = 0;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
