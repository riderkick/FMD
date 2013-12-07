  function GetPururinPageNumber: Boolean;
  var
    s   : String;
    i,g,j: Cardinal;
    l   : TStringList;
    isStartGetPageNumber: Boolean = FALSE;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
        s:= StringReplace(URL, '_1.html', '.html', []);
		s:= StringReplace(s, '/view/', '/gallery/', []);
		s:= DecodeUrl(StringReplace(s, '/00/', '/', []));
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
        if (Pos('class="square"', parse.Strings[i])>0) then
          isStartGetPageNumber:= TRUE;

        if (isStartGetPageNumber) AND
           (Pos('class="square"', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i+1];
		  g:= length(s);
          Delete(s,g-10,g-3);
          Delete(s,1,9);
		  g:= StrToInt(s);
          manager.container.pageNumber:= g;
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
