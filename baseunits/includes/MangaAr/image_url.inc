  function GetMangaArImageURL: Boolean;
  var
    s: String;
    j, i: Cardinal;
    l, ts: TStringList;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(MANGAAR_ID, URL) + '/1';
    Result := GetPage(TObject(l),
      s,
      Task.Container.Manager.retryConnect);

    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    Task.Container.PageLinks.Clear;
    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('var pages =', parse[i]) > 0) then
        begin
          ts := TStringList.Create;
          s := GetString(parse[i], '[{', '}];');
          s := StringReplace(s, '":', '",', [rfReplaceAll]);
          s := StringReplace(s, '\', '', [rfReplaceAll]);
          ts.DelimitedText := s;
          for j := 0 to ts.Count - 1 do
          begin
            if (Pos('url', ts[j]) > 0) then
            begin
              s := ts[j + 1];
              s := 'http' + GetString(s, 'http', '?w='); //original resolution
              Task.Container.PageLinks.Add(s);
            end;
          end;
          ts.Free;
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
