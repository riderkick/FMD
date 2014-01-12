  function GetEatMangaPageNumber: Boolean;
  var
    s    : String;
    count: Cardinal = 0;
    i, j : Cardinal;
    l    : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(WebsiteRoots[EATMANGA_ID,1] + URL);
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
      for i:= 0 to parse.Count-1 do
      begin
        if (Pos('</select>', parse.Strings[i])>0) then
          if count > 0 then
          begin
            s:= parse.Strings[i-2];
            manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
            break;
          end
          else
            Inc(count);
      end;
    end;
    parse.Free;
    l.Free;
  end;
  