  function GetVnSharingImageURL: Boolean;
  var
    s: String;
    j, i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(VNSHARING_ID, URL),
      Task.Container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := @OnTag;
    Parser.OnFoundText := @OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
      begin
        if Pos('lstImages.push("', parse[i]) > 0 then
        begin
          s := parse[i];
          repeat
            j := Pos('lstImages.push("', s);
            Task.Container.PageLinks.Add(
              EncodeUrl(GetString(s, 'lstImages.push("', '");')));
            Delete(s, Pos('lstImages.push("', s), 16);
            j := Pos('lstImages.push("', s);
          until j = 0;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
