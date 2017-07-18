  function GetCentrumMangi_PLImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(CENTRUMMANGI_PL_ID, '/' +
      StringReplace(URL, '-1.html', '.html', []));
    Result := GetPage(TObject(l),
      s,
      Task.Container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
        if (Pos('<img alt=', parse[i]) > 0) then
        begin
          s := GetVal(parse[i], 'src');
          s := StringReplace(s, 'https://', 'http://', [rfReplaceAll]);
          s := StringReplace(s, 'mangas/', WebsiteRoots[CENTRUMMANGI_PL_ID, 1] +
            '/mangas/', [rfReplaceAll]);
          Task.Container.PageLinks.Add(EncodeURL(s));
        end;
    end;
    parse.Free;
    l.Free;
  end;
