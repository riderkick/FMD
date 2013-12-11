  function GetMangaFramePageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(WebsiteRoots[MANGAFRAME_ID,1] + URL + '1');
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
        if (Pos('class="divider"', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i-8];
          manager.container.pageNumber:= StrToInt(GetString(s, '/page/', '"'));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
