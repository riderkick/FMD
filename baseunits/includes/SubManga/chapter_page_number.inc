  function GetSubMangaPageNumber: Boolean;
  var
    surl: String;
    i: Integer;
    l: TStringList;
    nextPage: Boolean = True;
    pageNum: Cardinal;
  begin
    pageNum := 1;
    surl := FillMangaSiteHost(SUBMANGA_ID, URL);
    l := TStringList.Create;
    parse := TStringList.Create;
    try
      manager.container.PageLinks.Clear;
      while nextPage do
      begin
        nextPage := False;
        l.Clear;
        parse.Clear;
        Result := GetPage(TObject(l), surl, manager.container.manager.retryConnect);
        Parser := THTMLParser.Create(l.Text);
        try
          Parser.OnFoundTag := OnTag;
          Parser.OnFoundText := OnText;
          Parser.Exec;
        finally
          Parser.Free;
        end;
        if parse.Count > 0 then
          for i := 0 to parse.Count - 1 do
          begin
            if (not nextPage) and
              ((GetTagName(parse[i]) = 'a') and
              (GetTagName(parse[i + 1]) = 'span') and
              (GetVal(parse[i + 1], 'class') = 'glyphicon glyphicon-chevron-right')) then
              if Pos('/c/', GetVal(parse[i], 'href')) <> 0 then
              begin
                nextPage := True;
                Inc(pageNum);
                surl := FillMangaSiteHost(SUBMANGA_ID, URL) + '/' + IntToStr(pageNum);
              end;
            if GetTagName(parse[i]) = 'img' then
            begin
              manager.container.PageLinks.Add(GetVal(parse[i], 'src'));
              Break;
            end;
          end;
      end;
    finally
      parse.Free;
      l.Free;
    end;
    manager.container.PageNumber := manager.container.PageLinks.Count;
  end;
