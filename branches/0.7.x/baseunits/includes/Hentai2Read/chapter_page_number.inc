function GetHentai2ReadPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     HENTAI2READ_ROOT + URL,
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
        if (GetTagName(parse.Strings[i]) = 'select') AND
           (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='cbo_wpm_pag') then
        begin
          j:= i+1;
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
  