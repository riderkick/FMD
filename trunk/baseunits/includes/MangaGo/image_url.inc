  function GetMangaGoImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;

    if (Pos('http://', URL) > 0) then
      s:= DecodeUrl(URL + IntToStr(workCounter+1) + '/')
    else
      s:= DecodeUrl(WebsiteRoots[MANGAGO_ID,1] + URL + IntToStr(workCounter+1) + '/');
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
        if (Pos('imgReady(''', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workCounter]:= (GetString(parse.Strings[i], 'imgReady(''', ''','));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
