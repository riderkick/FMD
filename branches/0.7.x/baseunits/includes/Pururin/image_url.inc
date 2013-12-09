  function GetPururinImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
	  s:= StringReplace(URL, '_1.html', '_', []);
      s:= DecodeUrl(StringReplace(s, '/00', '/0' + IntToStr(workCounter+0), []) + IntToStr(workCounter+1) + '.html');
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
      for i:= parse.Count-1 downto 4 do
        if (Pos('class="b"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workCounter]:= EncodeURL(WebsiteRoots[PURURIN_ID,1] + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
