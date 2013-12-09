  function GetMangaEdenImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    if manager.container.mangaSiteID = MANGAEDEN_ID then
      s:= WebsiteRoots[MANGAEDEN_ID,1] + URL + IntToStr(workCounter+1) + '/'
    else
      s:= WebsiteRoots[PERVEDEN_ID,1] + URL + IntToStr(workCounter+1) + '/';
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
      for i:= parse.Count-1 downto 0 do
        if (Pos('"mainImg"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workCounter]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
