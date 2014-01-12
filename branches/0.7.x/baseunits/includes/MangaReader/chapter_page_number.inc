function GetMangaReaderPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[MANGAREADER_ID,1] + URL,
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    s:= WebsiteRoots[MANGAREADER_ID,1] + URL;
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= 0 to parse.Count-1 do
      begin
        if (Pos('</select>', parse.Strings[i])>0) AND
           (Pos('</div>', parse.Strings[i+2])>0) then
        begin
          s:= parse.Strings[i+1];
          Delete(s, Pos(' of ', s), 4);
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
  