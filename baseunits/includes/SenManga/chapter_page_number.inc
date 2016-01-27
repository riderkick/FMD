  function GetSenMangaPageNumber: Boolean;
  var
    s: String;
    i: LongInt;
    l: TStringList;
    isStartGetPageNumber: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(URL);
    s := FillMangaSiteHost(SENMANGA_ID, s);
    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);
    if Result then
    begin
      Parser := THTMLParser.Create(l.Text);
      try
        Parser.OnFoundTag := OnTag;
        Parser.OnFoundText := OnText;
        parse.Clear;
        Parser.Exec;
      finally
        Parser.Free;
      end;

      manager.container.PageNumber := 0;
      if parse.Count > 0 then
        for i := 0 to parse.Count - 1 do
        begin
          if (GetTagName(parse[i]) = 'select') and (Pos('cbo_wpm_pag', parse[i]) > 0) then
            isStartGetPageNumber := True;
          if isStartGetPageNumber then
          begin
            if GetTagName(parse[i]) = '/select' then
              Break
            else
            if GetTagName(parse[i]) = 'option' then
              Inc(manager.container.PageNumber);
          end;
        end;
    end;
    parse.Free;
    l.Free;
  end;
