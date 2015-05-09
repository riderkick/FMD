  function GetS2scanPageNumber: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    try
      Result := GetPage(TObject(l),
        FillMangaSiteHost(S2SCAN_ID, URL) + 'page/1',
        manager.container.Manager.retryConnect);

      if l.Count > 0 then
      begin
        manager.container.PageLinks.Clear;
        for i := 0 to l.Count - 1 do
        begin
          if Pos('var pages = [', l[i]) > 0 then
          begin
            s := StringReplace(l[i], 'var pages = [', '[', []);
            s := Trim(TrimChar(s, [';']));
            ParseJSONArray(s, 'url', manager.container.PageLinks);
            Break;
          end;
        end;
      end;
    finally
      l.Free;
    end;
  end;
