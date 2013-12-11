  function GetBlogTruyenImageURL: Boolean;
  var
    isExtrackLink: Boolean = FALSE;
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[BLOGTRUYEN_ID,1] + URL,
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
      begin
        if NOT (isExtrackLink) AND (Pos('<div id="noidungchuong">', parse.Strings[i]) > 0) then
          isExtrackLink:= TRUE;
        if (isExtrackLink) AND (GetTagName(parse.Strings[i]) = 'img') then
          manager.container.pageLinks.Add(EncodeUrl(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='))))
        else
        if (isExtrackLink) AND (Pos('</div>', parse.Strings[i])>0) then
          break;
      end;
    end;
    parse.Free;
    l.Free;
  end;
