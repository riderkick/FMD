  function GetMangaReaderImageURL: Boolean;
  var
    realURL: String;
    i: Integer;
    l: TStringList;

    procedure BreakURL;
    var
      isSlashed: Boolean = False;
      i, oldI: Cardinal;
    begin
      if Pos('.html', URL) = 0 then
      begin
        realURL := URL + '/' + IntToStr(workCounter + 1);
        Exit;
      end;
      i := 2;
      realURL := '/';
      while i <= Length(URL) do
      begin
        if (not isSlashed) and (URL[i] = '/') then
        begin
          isSlashed := True;
          oldI := i;
          for i := i - 1 downto 1 do
          begin
            if URL[i] <> '-' then
            begin
              SetLength(realURL, Length(realURL) - 1);
            end
            else
            begin
              realURL := realURL + IntToStr(workCounter + 1);
              Break;
            end;
          end;
          i := oldI;
          // realURL:= realURL + '/';
        end
        else
        begin
          realURL := realURL + URL[i];
          Inc(i);
        end;
      end;
    end;

  begin
    l := TStringList.Create;
    BreakURL;
    Result := GetPage(TObject(l), FillMangaSiteHost(MANGAREADER_ID, realURL),
      manager.container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      // manager.container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
      begin
        if GetTagName(parse.Strings[i]) = 'img' then
        begin
          if //(Pos(realURL, parse.Strings[i])>0) AND
          (Pos('alt=', parse.Strings[i]) > 0) then
          begin
            manager.container.PageLinks.Strings[workCounter] :=
              GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
            Break;
          end;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
