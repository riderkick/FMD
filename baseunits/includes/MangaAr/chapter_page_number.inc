  function GetMangaArPageNumber: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(MANGAAR_ID, URL) + '/1';
    Result := GetPage(TObject(l),
      s,
      manager.container.manager.retryConnect);

    if l.Count > 0 then
     try
       for i := 0 to l.Count - 1 do
         if Pos('var pages = [', l[i]) > 0 then
         begin
           s := l[i];
           s := StringReplace(s, 'var pages =', '', [rfIgnoreCase]);
           s := Trim(TrimChar(s, [';', ' ']));
           Break;
         end;
       if s <> '' then
       begin
         manager.container.PageLinks.Clear;
         ParseJSONArray(s, 'url', manager.container.PageLinks);
         manager.container.PageNumber := manager.container.PageLinks.Count;
       end;
     finally
       parse.Free;
       l.Free;
     end;
  end;
