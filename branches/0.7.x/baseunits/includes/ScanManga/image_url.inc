  function GetScanMangaImageURL: Boolean;
  var
    s2,
    stub,
    tmp,
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    if Pos('http://', URL) = 0 then
      s:= DecodeUrl(WebsiteRoots[SCANMANGA_ID,1] + URL)
    else
      s:= DecodeUrl(URL);
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;

    manager.container.pageLinks.Clear;

    if parse.Count>0 then
    begin
      for i:= 0 to parse.Count-1 do
        if (Pos('''+u[id_page]', parse.Strings[i])>0) then
        begin
          stub:= 'http' + GetString(parse.Strings[i], '$(''#image_lel'').attr(''src'',''http', '''+u[id_page]');
          break;
        end;
    end;

    if parse.Count>0 then
    begin
      for i:= 0 to parse.Count-1 do
        if (Pos('var u = new Array', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i];
          repeat
            tmp:= GetString(s, ';u[', ']="');
            s:= StringReplace(s, ';u[' +tmp+ ']="', '~!@<>', []);
            tmp:= stub + GetString(s, '~!@<>', '";n[');
            //s2:= EncodeUrl(stub + GetString(s, '~!@<>', '";n'));
            manager.container.pageLinks.Add((stub + GetString(s, '~!@<>', '";n')));
            s:= StringReplace(s, '~!@<>', '', []);
            s:= StringReplace(s, '";n[', '', []);
            j:= Pos('";n[', s);
          until j = 0;
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
