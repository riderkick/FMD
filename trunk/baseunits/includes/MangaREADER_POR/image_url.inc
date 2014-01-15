  function GetMangaREADER_PORImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(WebsiteRoots[MANGAREADER_POR_ID,1] + URL + '/#1');
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
 
  if parse.Count > 0 then
  begin
   manager.container.pageLinks.Clear;
    for i:= 0 to parse.Count-1 do
    begin
      if Pos('function Proxima()', parse.Strings[i]) > 0 then
      begin
        s:= GetString(parse.Strings[i], 'new Array("",', 'function Proxima');
        repeat
          j:= Pos('http://', s);
          manager.container.pageLinks.Add(EncodeURL(GetString(s, '"', '"')));
          s:= StringReplace(s, '"', '', []);
          s:= StringReplace(s, '"', '', []);
          Delete(s, j, 7);
          j:= Pos('http://', s);
        until j = 0;
      end;
    end;
  end;
		
    parse.Free;
    l.Free;
  end;
