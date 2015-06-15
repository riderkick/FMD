  function GetWPMangaImageURL: Boolean;
  var
    s: String;
    i, j: Integer;
    Source: TStringList;
  begin
    s := FillMangaSiteHost(manager.container.MangaSiteID, URL);
    if RightStr(s, 1) <> '/' then
      s += '/';
    s += IntToStr(QWord(workCounter)+1) + '/';

    Source := TStringList.Create;
    Result := GetPage(TObject(Source), s, manager.container.Manager.retryConnect);

    if not Result then
    begin
      Source.Free;
      Exit;
    end;

    parse := TStringList.Create;
    ParseHTML(Source.Text, parse);
    Source.Free;

    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        if (GetVal(parse[i], 'class') = 'wpm_pag mng_rdr') then
        begin
          for j := i+1 to parse.Count-1 do
            if GetTagName(parse[j]) = 'img' then
            begin
              manager.container.PageLinks.Strings[workCounter] := GetVal(parse[j], 'src');
              Break;
            end;
          Break;
        end;
    parse.Free;
  end;
