  function GetAnimeAImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(ANIMEA_ID,
      StringReplace(URL, '.html', '', []) +
      '-page-' + IntToStr(Task.Container.WorkCounter + 1) + '.html'),
      Task.Container.Manager.retryConnect);

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
        if (Pos('<img', parse[i]) > 0) and
          (Pos('class="scanmr"', parse[i]) > 0) and
          (Pos('id="scanmr"', parse[i])> 0) then
          Task.Container.PageLinks[Task.Container.WorkCounter] := GetVal(parse[i], 'src');
      end;
    end;
    parse.Free;
  end;
