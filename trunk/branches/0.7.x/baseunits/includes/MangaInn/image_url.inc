  function GetMangaInnImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[MANGAINN_ID,1] + URL + '/page_'+IntToStr(workCounter+1),
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
        if GetTagName(parse.Strings[i]) = 'img' then
          if GetAttributeValue(GetTagAttribute(parse.Strings[i], 'id='))='imgPage' then
          begin
            manager.container.pageLinks.Strings[workCounter]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
            break;
          end;
    end;
    parse.Free;
    l.Free;
  end;
