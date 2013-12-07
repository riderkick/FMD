  function GetMangacanImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    if Pos('http://', URL) = 0 then
      s:= WebsiteRoots[MANGACAN_ID,1] + '/' + URL
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
        if (Pos('<img alt=', parse.Strings[i])>0) then
        begin
          s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
	  s:= StringReplace(s, 'https://', 'http://', [rfReplaceAll]);
	  s:= StringReplace(s, 'mangas/', WebsiteRoots[MANGACAN_ID,1] + '/mangas/', [rfReplaceAll]);
          manager.container.pageLinks.Add(EncodeURL(s));
        end;
    end;
    parse.Free;
    l.Free;
  end;
