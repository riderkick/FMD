function GetMangaArPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= WebsiteRoots[MANGAAR_ID,1] + URL + '/1';
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);

    // convert charset
    l.Text:= CP1256ToUTF8(l.Text);

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
        begin
          s:= TrimLeft(TrimRight(parse.Strings[i-3]));
          manager.container.pageNumber:= StrToInt(s);
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
  