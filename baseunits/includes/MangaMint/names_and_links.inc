  function MangaMintGetNamesAndLinks: Byte;
  var
    i: Integer;
    isExtractNames: Boolean = False;
    regx: TRegExpr;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAMINT_ID, 1] +
      '/directory?page=' + IntToStr(StrToInt(URL) + 1), 1) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    parse.Clear;
    Parser := THTMLParser.Create(Source.Text);
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
    finally
      Parser.Free;
    end;
    Source.Free;

    if parse.Count > 0 then
    begin
      regx := TRegExpr.Create;
      try
        regx.Expression := '\sManga$';
        for i := 0 to parse.Count - 1 do
        begin
          if (GetTagName(parse[i]) = 'table') and (GetVal(parse[i], 'class') = 'sticky-enabled') then
            isExtractNames := True;
          if isExtractNames then
          begin
            if GetTagName(parse[i]) = '/table' then
              Break
            else
            if (GetTagName(parse[i]) = 'a') and (GetVal(parse[i], 'class') = 'cover') then
              if GetTagName(parse[i + 1]) = 'img' then
              begin
                links.Add(GetVal(parse[i], 'href'));
                names.Add(CommonStringFilter(
                  regx.Replace(Trim(GetVal(parse[i + 1], 'title')), '', False)));
              end;
          end;
        end;
      finally
        regx.Free;
      end;
    end;

    Result := NO_ERROR;
  end;
