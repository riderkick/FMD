  function GetExtremeMangasPageNumber: Boolean;
  var
    i, j: Integer;
    l: TStringList;
    s: String;
    //isExtractPageNumber: Boolean = False;
  begin
    Task.Container.PageNumber := 0;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(EXTREMEMANGAS_ID, URL);
    Result := GetPage(TObject(l), s, Task.Container.Manager.retryConnect);

    l.Text := FixHTMLTagQuote(l.Text);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      Task.Container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('addpage(', parse[i]) > 0) then
        begin
          l.Clear;
          if ExtractStrings([','], [], PChar(Trim(parse[i])), l) > 0 then
            for j := l.Count - 1 downto 0 do
              if (Pos('addpage(', l[j]) = 0) or (Pos('/extrememangas.gif', l[j]) > 0) then
                l.Delete(j)
              else
                l[j] := Trim(TrimChar(StringReplace(l[j], 'addpage(', '', [rfIgnoreCase]), [',', '''']));
          Task.Container.PageLinks.AddStrings(l);
          Task.Container.PageNumber := Task.Container.PageLinks.Count;
          Break;
        end;
      end;

    end;
    l.Free;
    parse.Free;
  end;
