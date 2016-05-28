  function GetMangaTradersImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(MANGATRADERS_ID, URL);
    s := s + '/page-' + IntToStr(Task.Container.WorkCounter + 1);
    Result := GetPage(TObject(l), s, Task.Container.manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if (Pos('<img', parse[i]) > 0) and (Pos('onerror=', parse[i]) > 0) then
        begin
          //fix line
          parse[i] := RemoveBreaks(parse[i]);
          if Pos(#9, parse[i]) > 0 then
            parse[i] := StringReplace(parse[i], #9, ' ', [rfReplaceAll, rfIgnoreCase]);
          Task.Container.PageLinks[Task.Container.WorkCounter] := GetVal(parse[i], 'src');
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
