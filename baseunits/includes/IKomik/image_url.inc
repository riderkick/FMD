  function GetIKomikImageURL: Boolean;
  var
    i, j: Integer;
    l: TStringList;
    s: String;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(IKOMIK_ID, URL);
    if Length(s) > 0 then
      if s[Length(s)] <> '/' then
         s := s + '/';
    s := s + IntToStr(QWord(WorkId) + 1);
    Result := GetPage(TObject(l), s , Task.Container.Manager.retryConnect);

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
        if (Pos('<div', parse[i]) > 0) and (Pos('style="overflow', parse[i]) > 0) then
        begin
          for j := i + 1 to parse.Count - 1 do
          begin
            if Pos('<img', parse[j]) > 0 then
            begin
              Task.Container.PageLinks[WorkId] := GetVal(parse[j], 'src');
              Break;
            end;
          end;
          Break;
        end;
      end;
    end;
    parse.Free;
  end;
