  function GetMangaFrameImageURL: Boolean;
  var
    i: Integer;
    l: TStringList;
    s: String;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(MANGAFRAME_ID, URL);
    if not ExecRegExpr('/$', s) then
       s := s + '/';
    s := s + 'page/' + IntToStr(QWord(workCounter) + 1);
    Result := GetPage(TObject(l), s , manager.container.Manager.retryConnect);

    if Self.Terminated then
    begin
      l.Free;
      parse.Free;
      Exit;
    end;

    l.Text := FixHTMLTagQuote(l.Text);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    l.Free;
    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        //if i + 2 < parse.Count - 1 then
          if (Pos('class="open"', parse[i]) > 0) and (Pos('<img', parse[i]) > 0) then
          begin
            manager.container.PageLinks[workCounter] := Trim(GetVal(parse[i], 'src'));
            Break;
          end;
    parse.Free;
  end;
