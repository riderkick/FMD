  function GetMangaEstaImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    if Pos('http://', URL) = 0 then
      s:= WebsiteRoots[MANGAESTA_ID,1] + URL
    else
      s:= URL;
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
      begin
        if Pos('addpage(''', parse.Strings[i]) > 0 then
        begin
          s:= parse.Strings[i];
          s:= StringReplace(s, 'https://', 'http://', [rfReplaceAll]);
          repeat
            j:= Pos('addpage(''', s);
            if Pos('googleusercontent', s) > 0 then
              manager.container.pageLinks.Add(EncodeUrl(GetString(s, 'addpage(''', ''',')))
            else
              manager.container.pageLinks.Add(EncodeUrl(GetString(s, 'addpage(''', ');')));
            Delete(s, Pos('addpage(''', s), 16);
            j:= Pos('addpage(''', s);
          until j = 0;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
