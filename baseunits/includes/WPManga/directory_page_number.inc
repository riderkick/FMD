  function GetWPMangaDirectoryPageNumber: Byte;
  var
    i: Integer;
  const
    dirURL = '/manga-list/all/any/last-added/';
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[WebsiteID, 1] +
      dirURL, 1) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    ParseHTML(Source.Text, parse);
    Source.Free;

    if parse.Count > 0 then
      for i := parse.Count-1 downto 0 do
        if Pos(dirURL, parse[i]) <> 0 then
        begin
          Page := StrToIntDef(ReplaceRegExpr('^.*'+dirURL+'(\d+)/$',
            GetVal(parse[i], 'href'), '$1', True), 1);
          Break;
        end;
  end;
