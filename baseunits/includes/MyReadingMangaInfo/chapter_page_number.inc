  function GetMyReadingMangaInfoPageNumber: Boolean;
  var
    i: Integer;
    //j: Integer;
    l: TStringList;
    s: String;
    isExtractImg: Boolean = False;
  begin
    Task.Container.PageNumber := 0;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(MYREADINGMANGAINFO_ID, URL);
    Result := GetPage(TObject(l), s, Task.Container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    l.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
      begin
        //if (Pos('class="separator"', parse[i]) > 0) and
        //  (Pos('style="clear: both; text-align: center;"', parse[i]) > 0) then
        //  for j := i + 1 to parse.Count - 1 do
        //    if (Pos('<img', parse[j]) > 0) then
        //    begin
        //      Task.Container.PageLinks.Add(GetVal(parse[j], 'src'));
        //      Break;
        //    end;
        if isExtractImg and (Pos('<footer', parse[i]) > 0) and
          (Pos('class="entry-footer"', parse[i]) > 0) then
        begin
          isExtractImg := False;
          Break;
        end;
        if (Pos('<div', parse[i]) > 0) and
          (Pos('class="entry-content"', parse[i]) > 0) then
          isExtractImg := True;
        if isExtractImg and (Pos('<img', parse[i]) > 0) then
          Task.Container.PageLinks.Add(GetVal(parse[i], 'src'));
      end;
      Task.Container.PageNumber := Task.Container.PageLinks.Count;
    end;
    parse.Free;
  end;
