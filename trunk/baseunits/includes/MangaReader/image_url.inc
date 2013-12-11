  function GetMangaReaderImageURL: Boolean;
  var
    realURL,
    s: String;
    j,
    i: Cardinal;
    l: TStringList;

    procedure  BreakURL;
    var
      isSlashed: Boolean = FALSE;
      i,
      oldI     : Cardinal;
    begin
      if Pos('.html', URL) = 0 then
      begin
        realURL:= URL + '/' + IntToStr(workCounter+1);
        exit;
      end;
      i:= 2;
      realURL:= '/';
      while i <= Length(URL) do
      begin
        if (NOT isSlashed) AND (URL[i] = '/') then
        begin
          isSlashed:= TRUE;
          oldI:= i;
          for i:= i-1 downto 1 do
          begin
            if URL[i] <> '-' then
            begin
              SetLength(realURL, Length(realURL)-1);
            end
            else
            begin
              realURL:= realURL + IntToStr(workCounter+1);
              break;
            end;
          end;
          i:= oldI;
         // realURL:= realURL + '/';
        end
        else
        begin
          realURL:= realURL + URL[i];
          Inc(i);
        end;
      end;
    end;

  begin
    l:= TStringList.Create;
    BreakURL;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[MANGAREADER_ID,1] + realURL,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
     // manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
      begin
        if GetTagName(parse.Strings[i]) = 'img' then
        begin
          if //(Pos(realURL, parse.Strings[i])>0) AND
             (Pos('alt=', parse.Strings[i])>0) then
          begin
            manager.container.pageLinks.Strings[workCounter]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
            break;
          end;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
