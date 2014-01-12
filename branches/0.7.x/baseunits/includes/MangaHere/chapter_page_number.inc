function GetMangaHerePageNumber: Boolean;
  var
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[MANGAHERE_ID,1] + URL,
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
        if GetTagName(parse.Strings[i]) = 'option' then
        begin
          j:= i;
          while GetTagName(parse.Strings[j]) = 'option' do
          begin
            Inc(manager.container.pageNumber);
            Inc(j, 4);
          end;
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
  