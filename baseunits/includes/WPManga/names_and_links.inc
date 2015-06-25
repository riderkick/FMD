  function GetWPMangaNamesAndLinks: Byte;
  var
    i, j, k: Integer;
    s: String;
  const
    dirURL = '/manga-list/all/any/last-added/';
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[WebsiteID, 1] + dirURL +
      IntToStr(StrToIntDef(URL, -1) + 1) + '/', 1) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    ParseHTML(Source.Text, parse);
    Source.Free;

    if WebsiteID = EYEONMANGA_ID then
    begin
      for i := 0 to parse.Count-1 do
        if GetVal(parse[i], 'class') = 'cvr' then try
          if GetTagName(parse[i-4]) = 'a' then
          begin
            names.Add(CommonStringFilter(parse[i-3]));
            links.Add(GetVal(parse[i-4], 'href'));
          end;
        except
        end;
    end
    else
      for i := 0 to parse.Count-1 do
      begin
        //thumbnail mode
        if GetVal(parse[i], 'id') = 'sct_content' then
        begin
          for j := i+1 to parse.Count-1 do
          begin
            s := GetTagName(parse[j]);
            if (s = 'sct_sidebar') or (s = 'sct_wid_bot') then
              Break
            else
            if GetVal(parse[j], 'class') = 'det' then
            begin
              for k := j+1 to parse.Count-1 do
              begin
                if GetTagName(parse[k]) = 'a' then
                begin
                  links.Add(GetVal(parse[k], 'href'));
                  names.Add(CommonStringFilter(parse[k+1]));
                  Break;
                end;
              end;
            end;
          end;
          Break;
        end;
      end;
  end;
