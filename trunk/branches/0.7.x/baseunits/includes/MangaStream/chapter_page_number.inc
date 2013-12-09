  function GetMangaStreamPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(MANGASTREAM_ROOT2 + URL + '/1');
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
        if (Pos('Last Page (', parse.Strings[i])>0) then
        begin
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(GetString(parse.Strings[i], 'Last Page (', ')'))));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
