function   GetPururinDirectoryPageNumber: Byte;
  var
    i,g : Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[PURURIN_ID,1], 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= parse.Count-1 downto 2 do
    begin
      if (Pos('/browse/0/11/2.html', parse.Strings[i]) > 0) then
      begin
        {s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+3], 'href='));
		g:= length(s);
        Delete(s,g-8,g-8);
        Delete(s,1,10);
        Page:= StrToInt(s);}
        Page:= StrToInt('10');
	Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end; 