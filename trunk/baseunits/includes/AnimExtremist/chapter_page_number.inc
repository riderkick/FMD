function GetAnimeExtremistPageNumber: Boolean;
  var
    i, j: Cardinal;
    l   : TStringList;
    s   : String;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= StringReplace(WebsiteRoots[ANIMEEXTREMIST_ID,1] + URL, '.html', '', []) + '-1.html';
    Result:= GetPage(TObject(l),
                     StringReplace(WebsiteRoots[ANIMEEXTREMIST_ID,1] + URL, '.html', '', []) + '-1.html',
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= 0 to parse.Count-1 do
      begin
        if Pos('</select>', parse.Strings[i]) > 0 then
        begin
          manager.container.pageNumber:= StrToInt(GetString(TrimLeft(TrimRight(parse.Strings[i-3]+'~!@')), 'Pagina ', '~!@'));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
  