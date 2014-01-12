  function GetMangaGoPageNumber: Boolean;
  var
    s    : String;
    i, j : Cardinal;
    l    : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    if (Pos('http://', URL) > 0) then
      s:= DecodeUrl(URL + '1/')
    else
      s:= DecodeUrl(WebsiteRoots[MANGAGO_ID,1] + URL + '1/');
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
      for i:= parse.Count-1 downto 5 do
      begin
        if (Pos('class="clear gap"', parse.Strings[i])>0) then
        begin
          s:= TrimLeft(TrimRight(parse.Strings[i-5]));
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
