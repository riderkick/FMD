  function GetPornComixImageURL: Boolean;
  var
    i: LongInt;
    s, surl: String;
    l: TStringList;
  begin
    surl := Task.Container.PageContainerLinks[WorkId];
    if not ((Pos('imagetwist.', surl) <> 0) or
      (Pos('imgmega.', surl) <> 0) or
      (Pos('imgchili.', surl) <> 0)) then
      Exit(False);
    l := TStringList.Create;
    Result := GetPage(TObject(l), DecodeUrl(surl), Task.Container.manager.retryConnect);

    //broken source
    if l.Count > 0 then
      if (Pos('imgmega.', surl) <> 0) or
        (Pos('imagetwist.', surl) <> 0) then
      begin
        for i := 0 to l.Count - 1 do
          if (Pos('<img', l[i]) <> 0) and (Pos('class="pic', l[i]) <> 0) then
          begin
            s := l[i];
            Break;
          end;
        if s <> '' then
          l.Text := s;
      end;

    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      // imgmega, imgaetwist
      if (Pos('imgmega.', surl) <> 0) or
        (Pos('imagetwist.', surl) <> 0) then
      begin
        for i := 0 to parse.Count - 1 do
          if (Pos('<img', parse[i]) <> 0) and (Pos('class="pic', parse[i]) <> 0) then
          begin
            Task.Container.PageLinks[WorkId] := GetVal(parse[i], 'src');
            Break;
          end;
      end
      else
      // imgchili
      if Pos('imgchili.', surl) <> 0 then
      begin
        for i := 0 to parse.Count - 1 do
          if (Pos('<img', parse[i]) <> 0) and (Pos('id="show_image', parse[i]) <> 0) then
          begin
            Task.Container.PageLinks[WorkId] := GetVal(parse[i], 'src');
            Break;
          end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
