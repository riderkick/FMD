function   BlogTruyenGetNamesAndLinks: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
    stream: TStringStream;
  begin
    Result:= INFORMATION_NOT_FOUND;
    stream:= TStringStream.Create('');
    s:= WebsiteRoots[BLOGTRUYEN_ID,1] + BLOGTRUYEN_JS_BROWSER;
    s:= BLOGTRUYEN_POST_FORM + IntToStr(StrToInt(URL)+1);
    while NOT HttpPostURL(WebsiteRoots[BLOGTRUYEN_ID,1] + BLOGTRUYEN_JS_BROWSER, BLOGTRUYEN_POST_FORM + IntToStr(StrToInt(URL)+1), stream) do ;
    source.Text:= stream.DataString;
    stream.Free;
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
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('onmouseover="TagToTip', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(GetString(parse.Strings[i], 'title="đọc truyện ', '" onmouseover'))));
        names.Add(HTMLEntitiesFilter(s));
        links.Add(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href="')));
      end;
    end;
    source.Free;
  end; 