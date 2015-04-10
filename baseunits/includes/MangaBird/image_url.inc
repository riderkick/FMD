  function GetMangaBirdImageURL: Boolean;
  var
    i, j: Integer;
    l: TStringList;
    s: String;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(MANGABIRD_ID, URL);
    s := s + '?page=' + IntToStr(QWord(workCounter));
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
        if (Pos('class="content"', parse[i]) > 0) and (Pos('<div', parse[i]) > 0) then
        begin
          j := i + 1;
          while j < parse.Count - 1 do
          begin
            if Pos('<img', parse[j]) > 0 then
            begin
              manager.container.PageLinks[workCounter] := GetVal(parse[j], 'src');
              Break;
            end;
            Inc(j);
          end;
          Break;
        end;
      end;
    end;
    parse.Free;
  end;
