  function GetMangaArImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= WebsiteRoots[MANGAAR_ID,1] + URL + '/' + IntToStr(workCounter+1);
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);

    // convert charset
    l.Text:= CP1256ToUTF8(l.Text);

    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count>0 then
    begin
      for i:= 0 to parse.Count-1 do
        if (Pos('id="PagePhoto"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workCounter]:= EncodeURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src=')));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
