  function GetMangaPandaImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;

    if (Pos('.html', URL) > 0) AND (Pos(SEPERATOR2, URL) > 0) then
    begin
      s:= DecodeUrl(WebsiteRoots[MANGAPANDA_ID,1] + URL);
      s:= StringReplace(s, SEPERATOR2, '-' + IntToStr(workCounter+1) + '/', [])
    end
    else
      s:= DecodeUrl(WebsiteRoots[MANGAPANDA_ID,1] + URL + '/' + IntToStr(workCounter+1));
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count>0 then
    begin
      for i:= 0 to parse.Count-1 do
        if (Pos('"imgholder"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workCounter]:= GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
