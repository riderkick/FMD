  function GetMangaTownImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
    s: String;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(MANGATOWN_ID, URL);
    if Length(s) > 0 then
      if s[Length(s)] <> '/' then
         s := s + '/';
    s := s + IntToStr(workCounter + 1) + '.html';
    Result := GetPage(TObject(l), s , manager.container.Manager.retryConnect);

    if Self.Terminated then
    begin
      l.Free;
      parse.Free;
      Exit;
    end;

    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    l.Free;
    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<img', parse[i]) > 0) and (Pos('onerror=', parse[i]) > 0) then
        begin
          manager.container.PageLinks[workCounter] := GetVal(parse[i], 'src');
          Break;
        end;
      end;
    end;
    parse.Free;
  end;
