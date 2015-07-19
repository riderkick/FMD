  function GetMangaReaderImageURL: Boolean;
  var
    realURL: String;
    i: Integer;
    l: TStringList;

    procedure BreakURL;
    var
      isSlashed: Boolean = False;
      j, oldI: Integer;
    begin
      if Pos('.html', URL) = 0 then
      begin
        realURL := URL + '/' + IntToStr(workCounter + 1);
        Exit;
      end;
      j := 2;
      realURL := '/';
      while j <= Length(URL) do
      begin
        if (not isSlashed) and (URL[j] = '/') then
        begin
          isSlashed := True;
          oldI := j;
          for j := j - 1 downto 1 do
          begin
            if URL[j] <> '-' then
            begin
              SetLength(realURL, Length(realURL) - 1);
            end
            else
            begin
              realURL := realURL + IntToStr(workCounter + 1);
              Break;
            end;
          end;
          j := oldI;
          // realURL:= realURL + '/';
        end
        else
        begin
          realURL := realURL + URL[j];
          Inc(j);
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
