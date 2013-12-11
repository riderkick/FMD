function GetMangaPandaPageNumber: Boolean;
  var
    s    : String;
    i, j : Cardinal;
    l    : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(WebsiteRoots[MANGAPANDA_ID,1] + URL);
    if (Pos('.html', URL) > 0) AND (Pos(SEPERATOR2, URL) > 0) then
      s:= StringReplace(s, SEPERATOR2, '-1/', []);
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= 1 to parse.Count-1 do
      begin
        if (Pos(' of ', parse.Strings[i])>0) AND
           (Pos('select', parse.Strings[i-1])>0) then
        begin
          s:= GetString(parse.Strings[i]+'~!@', ' of ', '~!@');
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
  