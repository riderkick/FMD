function GetBatotoPageNumber: Boolean;
  var
    isGoOn: Boolean = FALSE;
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;

    parse.Clear;
    l.Clear;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[BATOTO_ID,1] + URL + '/1',
                     manager.container.manager.retryConnect);

    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.SlowExec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i:= 0 to parse.Count-1 do
      begin
        if (Pos('page_select', parse.Strings[i])<>0) then
        begin
          isGoOn:= TRUE;
          break;
        end;
      end;
    end;
    if NOT isGoOn then
    begin
      manager.container.pageNumber:= 1;
      parse.Free;
      l.Free;
      exit;
    end;

    // parse.Add(WebsiteRoots[BATOTO_ID,1] + URL + '/1');
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= 0 to parse.Count-1 do
      begin
        if Pos('page_select', parse.Strings[i]) <> 0 then
         // if Pos('Page', parse.Strings[i+2]) <> 0 then
        begin
          j:= i+2;
          s:= parse.Strings[j];
          while GetTagName(parse.Strings[j]) = 'option' do
          begin
            Inc(manager.container.pageNumber);
            Inc(j, 3);
          end;
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
