  function GetUnixMangaImageURL: Boolean;
  var
    i: Integer;
    l: TStringList;
    s: String;
  begin
    l := TStringList.Create;
    s := Task.Container.PageContainerLinks[Task.Container.WorkCounter];
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
        if (Pos('STYLE="border', parse[i]) > 0) and (Pos('<IMG', parse[i]) > 0) then
        begin
          Task.Container.PageLinks[Task.Container.WorkCounter] := Trim(GetVal(parse[i], 'SRC'));
		  Break;
        end;
      end;
    end;
    parse.Free;
  end;
