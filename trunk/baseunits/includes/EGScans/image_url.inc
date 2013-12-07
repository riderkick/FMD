  function GetEGScansImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;

    s:= DecodeUrl(WebsiteRoots[EGSCANS_ID,1] + URL + '/' + IntToStr(workCounter+1));
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
      manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
        if (Pos('<img ondragstart', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Add(WebsiteRoots[EGSCANS_ID,1] + '/' + EncodeURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='))));
        end;
    end;
    parse.Free;
    l.Free;
  end;
