  function GetManga24hImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[MANGA24H_ID,1] + URL,
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
        if (GetTagName(parse.Strings[i]) = 'img') AND
           (Pos('style="border:3px', parse.Strings[i])<>0) then
          // (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class=')) = 'm_picture') then
        begin
          manager.container.pageLinks.Add(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
