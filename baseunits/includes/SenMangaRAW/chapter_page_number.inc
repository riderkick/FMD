  function GetSenMangaRAWPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    isExtractPageNumber: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    try
      FHTTP.UserAgent := RandomString(20+Random(100));
      if GetPage(TObject(l),
        DecodeUrl(FillMangaSiteHost(SENMANGARAW_ID, URL + '/1')),
        manager.container.manager.retryConnect) then
      begin
        Result := True;
        Parser := THTMLParser.Create(l.Text);
        try
          Parser.OnFoundTag := OnTag;
          Parser.OnFoundText := OnText;
          Parser.Exec;
        finally
          Parser.Free;
        end;

        if parse.Count > 0 then
        begin
          manager.container.PageLinks.Clear;
          for i := 0 to parse.Count - 1 do
          begin
            if (GetTagName(parse[i]) = 'select') and (GetVal(parse[i], 'name') = 'page') then
              isExtractPageNumber := True;
            if isExtractPageNumber then
            begin
              if GetTagName(parse[i]) = '/select' then
                Break
              else
              if GetTagName(parse[i]) = 'option' then
                manager.container.PageLinks.Add('G');
            end;
          end;
        end;
      end
      else
        Result := False;
    finally
      parse.Free;
      l.Free;
    end;
  end;
