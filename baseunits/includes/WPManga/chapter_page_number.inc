  function GetWPMangaPageNumber: Boolean;
  var
    s: String;
    i, j: Integer;
    Source: TStringList;
  begin
    s := FillMangaSiteHost(manager.container.MangaSiteID, URL);
    if RightStr(s, 1) <> '/' then
      s += '/';
    s += '1/';

    Source := TStringList.Create;
    Result := GetPage(TObject(Source), s, manager.container.manager.retryConnect);

    if not Result then
    begin
      Source.Free;
      Exit;
    end;

    parse := TStringList.Create;
    ParseHTML(Source.Text, parse);
    Source.Free;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count-1 do
      begin
        if (GetTagName(parse[i]) = 'select') and
          (GetVal(parse[i], 'class') = 'cbo_wpm_pag') then
        begin
          for j := i+1 to parse.Count-1 do
          begin
            s := GetTagName(parse[j]);
            if s = '/select' then
              Break
            else
            if s = 'option' then
              Inc(manager.container.PageNumber);
          end;
          Break;
        end;
      end;
    end;
    parse.Free;
  end;
