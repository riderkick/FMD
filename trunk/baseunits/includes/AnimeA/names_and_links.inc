  function   AnimeAGetNamesAndLinks: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[ANIMEA_ID,1] + ANIMEA_BROWSER + URL, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    for i:= 0 to source.Count-1 do
    begin
      if Pos('manga_img', source.Strings[i]) <> 0 then
      begin
        Result:= NO_ERROR;
        links.Add(GetString(source.Strings[i], '"', '"'));
        names.Add(GetString(source.Strings[i], 'title="', ' Manga"'));
      end;
    end;
    source.Free;
  end;