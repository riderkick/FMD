function GetAnimeStoryPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    if Pos('http://', URL) = 0 then
      s:= DecodeUrl(WebsiteRoots[ANIMESTORY_ID,1] + URL + '1')
    else
      s:= DecodeUrl(URL + '1');
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
        if (Pos('data-page=', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i];
          manager.container.pageNumber:= StrToInt(GetAttributeValue(GetTagAttribute(s, 'data-page=')));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
  