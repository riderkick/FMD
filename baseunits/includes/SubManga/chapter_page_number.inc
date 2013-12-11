function GetSubMangaPageNumber: Boolean;
  var
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[SUBMANGA_ID,1] + URL,
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= parse.Count-1 downto 3 do
      begin
        if Pos('</select>', parse.Strings[i]) > 0 then
        begin
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(parse.Strings[i-2])));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
  