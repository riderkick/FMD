  function GetMangaTradersImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= WebsiteRoots[MANGATRADERS_ID,1] + URL + '/page/' + IntToStr(workCounter+1);
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
        if (Pos('"image_display"', parse.Strings[i])>0) then
        begin
          s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+4], 'src='));
          if s <> '' then
            manager.container.pageLinks.Strings[workCounter]:= s
          else
            manager.container.pageLinks.Strings[workCounter]:= GetAttributeValue(GetTagAttribute(parse.Strings[i+12], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
