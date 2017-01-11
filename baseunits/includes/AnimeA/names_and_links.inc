  function AnimeAGetNamesAndLinks: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[ANIMEA_ID, 1] +
      ANIMEA_BROWSER + AURL, 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;
    for i := 0 to Source.Count - 1 do
    begin
      if Pos('manga_img', Source[i]) <> 0 then
      begin
        Result := NO_ERROR;
        ALinks.Add(GetString(Source[i], '"', '"'));
        ANames.Add(GetString(Source[i], 'title="', ' Manga"'));
      end;
    end;
    Source.Free;
  end;