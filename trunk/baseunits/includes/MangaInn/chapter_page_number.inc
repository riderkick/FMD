function GetMangaInnPageNumber: Boolean;
var
  s   : String;
  i, j: Cardinal;
  l   : TStringList;
begin
  l:= TStringList.Create;
  parse:= TStringList.Create;
  Result:= GetPage(TObject(l),
                   WebsiteRoots[MANGAINN_ID,1] + URL,
                   manager.container.manager.retryConnect);
  Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;
  Parser.Free;
  if parse.Count>0 then
    for i:= parse.Count-1 downto 5 do
    begin
      if Pos('</select>', parse.Strings[i]) <> 0 then
      begin
        manager.container.pageNumber:= StrToInt(Trim(parse.Strings[i-3]));
        break;
      end;
    end;
  parse.Free;
  l.Free;
end;
