  function GetJapanShinImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(JAPANSHIN_ID, URL) + '/page/' + IntToStr(workCounter),
      manager.container.Manager.retryConnect);

    if Self.Terminated then
    begin
      l.Free;
      Exit;
    end;

    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if (Pos('<img', parse[i]) > 0) and
          (Pos('class="open"', parse[i]) > 0) then
        begin
          manager.container.PageLinks[workCounter] :=
            GetVal(parse[i], 'src');
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
