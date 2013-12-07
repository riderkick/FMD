function   GetBatotoDirectoryPageNumber: Byte;
  var
    myParser: THTMLParser;
    isFoundPage: Boolean = FALSE;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[BATOTO_ID,1] + '/comic/_/comics/?per_page=750&st=0', 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    myParser:= THTMLParser.Create(PChar(source.Text));
    myParser.OnFoundTag := OnTag;
    myParser.OnFoundText:= OnText;
    myParser.Exec;
    myParser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('Page 1 of ', parse.Strings[i]) > 0) then
      begin
        s:= GetString(parse.Strings[i]+'~!@', 'Page 1 of ', '~!@');
        Page:= StrToInt(TrimLeft(TrimRight(s)));
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;
  