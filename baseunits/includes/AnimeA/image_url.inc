  function GetAnimeAImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(ANIMEA_ID,
      StringReplace(URL, '.html', '', []) +
      '-page-' + IntToStr(workCounter + 1) + '.html'),
      manager.container.Manager.retryConnect);

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
          manager.container.PageLinks[workCounter] := GetVal(parse[i], 'src');
      end;
    end;
    parse.Free;
  end;
