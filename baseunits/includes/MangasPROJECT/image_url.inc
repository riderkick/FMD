  function GetMangasPROJECTImageURL: Boolean;
  var
    s: String;
    j, i: Cardinal;
    l, ts: TStringList;
  begin
    l := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MANGASPROJECT_ID, URL) + '/#1');
    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      manager.container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
      begin
        if Pos('{ path: ', parse[i]) > 0 then
        begin
          s := GetString(parse[i], 'new Array({ path: ', '});');
          s := StringReplace(s, sLineBreak, '', [rfReplaceAll]);
          ts := TStringList.Create;
          try
            ts.DelimitedText := s;
            for j := 0 to ts.Count - 1 do
            begin
              if Pos('http', ts[j]) > 0 then
                manager.container.PageLinks.Add(ts[j]);
            end;
          finally
            ts.Free;
          end;
          Break;
        end;
      end;
    end;

    parse.Free;
    l.Free;
  end;
