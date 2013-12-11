  function GetMangaTradersPageNumber: Boolean;
  var
    isStartGetPageNumber: Boolean = FALSE;
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[MANGATRADERS_ID,1] + URL,
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
        if (NOT isStartGetPageNumber) AND
           (Pos('option value="1"  selected="selected"', parse.Strings[i]) > 0) then
          isStartGetPageNumber:= TRUE;
        if (isStartGetPageNumber) AND
           (Pos('</option>', parse.Strings[i])>0) then
          Inc(manager.container.pageNumber);
        if (isStartGetPageNumber) AND
           (Pos('</select>', parse.Strings[i])>0) then
          break;
      end;
    end;
    parse.Free;
    l.Free;
  end;
