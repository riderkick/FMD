  function GetMangaParkImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[MANGAPARK_ID,1] + URL + 'all',//IntToStr(workCounter+1),
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count>0 then
    begin
      manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
       // if GetTagName(parse.Strings[i]) = 'img' then
        if (Pos('a target="_blank"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Add(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')));
      //    break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
