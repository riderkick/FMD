  function GetMangaFoxImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(URL + '/' + IntToStr(workCounter+1) + '.html');
    if Pos(WebsiteRoots[MANGAFOX_ID,1], s) = 0 then
      s:= WebsiteRoots[MANGAFOX_ID,1] + s;
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
      for i:= 0 to parse.Count-1 do
        if (Pos('onclick="return enlarge()"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workCounter]:= GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
