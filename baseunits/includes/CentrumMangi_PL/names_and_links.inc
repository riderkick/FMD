  function CentrumMangi_PLGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    //isExtractItem :boolean = True;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[CENTRUMMANGI_PL_ID, 1] +
      CENTRUMMANGI_PL_BROWSER, 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        //if (Pos('<div ', parse[i]) > 0) and
        //   (Pos('class="box-max"', parse[i]) > 0) then
        //   isExtractItem := True;
        //if isExtractItem and
        //   (Pos('</table', parse[i]) > 0) then
        //   isExtractItem := False;

        if (Pos('<td style="width:45%;text-align: left;">', parse[i]) > 0) then
        begin
          Result := NO_ERROR;
          ALinks.Add(StringReplace(GetVal(parse[i + 1], 'href'),
            WebsiteRoots[CENTRUMMANGI_PL_ID, 1], '', [rfIgnoreCase]));
          ANames.Add(Trim(HTMLEntitiesFilter(StringFilter(parse[i + 2]))));
        end;

        //if isExtractItem then
        //begin
        //  if (Pos('<a ', parse[i]) > 0) and
        //     (Pos('/spis/', parse[i]) = 0) then
        //  begin
        //    Result := NO_ERROR;
        //    ALinks.Add(StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[CENTRUMMANGI_PL_ID, 1], '', [rfIgnoreCase]));
        //    ANames.Add(Trim(HTMLEntitiesFilter(StringFilter(parse[i + 1]))));
        //  end;
        //end;
      end;
    end;
    Source.Free;
  end;
