  function GetEatMangaPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    isGetPageNumber: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    try
      Result := GetPage(TObject(l),
        DecodeUrl(FillMangaSiteHost(EATMANGA_ID, URL)),
        manager.container.manager.retryConnect);

      Parser := THTMLParser.Create(PChar(l.Text));
      try
        Parser.OnFoundTag := OnTag;
        Parser.OnFoundText := OnText;
        Parser.Exec;
      finally
        Parser.Free;
      end;

      if parse.Count > 0 then
      begin
        manager.container.PageNumber := 0;
        for i := 0 to parse.Count - 1 do
        begin
          if (GetTagName(parse[i]) = 'select') and (GetVal(parse[i], 'id') = 'pages') then
            isGetPageNumber := True;
          if isGetPageNumber then
          begin
            if GetTagName(parse[i]) = '/select' then
              Break
            else if GetTagName(parse[i]) = 'option' then
              Inc(manager.container.PageNumber);
          end;
        end;
      end;
    finally
      parse.Free;
      l.Free;
    end;
  end;
