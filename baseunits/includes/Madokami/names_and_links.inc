  function MadokamiGetNamesAndLinks: Byte;
  var
    i: Integer;
    s: string;
    j: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    i := StrToIntDef(URL, 0);
    if i >= Length(MADOKAMI_BROWSER) then
    begin
      Source.Free;
      Exit;
    end;

    s := WebsiteRoots[MADOKAMI_ID, 1] + MADOKAMI_BROWSER[i];
    if not GetPage(TObject(Source), s, 1) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    ParseHTML(Source.Text, parse);
    Source.Free;
    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        if (GetTagName(parse[i]) = 'tr') and
          (Pos('data-record=', parse[i]) <> 0)then
        begin
          for j := i+2 to parse.Count-1 do
            if GetTagName(parse[j]) = 'a' then
            begin
              s := Trim(parse[j+1]);
              if RightStr(s, 1) = '/' then
                Delete(s, Length(s), 1);
              if AnsiIndexText(s, ['', '</span>', '_Unsorted']) = -1 then
              begin
                names.Add(CommonStringFilter(s));
                links.Add(GetVal(parse[j], 'href'));
              end;
              Break;
            end;
        end;

    Result := NO_ERROR;
  end;
