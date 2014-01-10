function GetSenMangaPageNumber: Boolean;
var
  s   : String;
  i, j: Cardinal;
  l   : TStringList;
  isStartGetPageNumber: Boolean = FALSE;
begin
  l:= TStringList.Create;
  parse:= TStringList.Create;
  s:= DecodeUrl(WebsiteRoots[SENMANGA_ID,1] + URL + '1/');
  Result:= GetPage(TObject(l),
                   s,
                   manager.container.manager.retryConnect);
  Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.SlowExec;
  Parser.Free;
  if parse.Count>0 then
  begin
    manager.container.pageNumber:= 0;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('name="page"', parse.Strings[i])>0) then
        isStartGetPageNumber:= TRUE;

      if (isStartGetPageNumber) AND
         (Pos('</select>', parse.Strings[i])>0) then
      begin
        s:= parse.Strings[i-3];
        manager.container.pageNumber:= StrToInt(GetAttributeValue(GetTagAttribute(s, 'value=')));
        break;
      end;
    end;
  end;
  parse.Free;
  l.Free;
end;
