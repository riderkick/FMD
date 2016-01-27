  function GetMangaEdenImageURL: Boolean;
  var
    s, imgURL: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    if manager.container.MangaSiteID = MANGAEDEN_ID then
      s := FillMangaSiteHost(MANGAEDEN_ID, URL) + IntToStr(workCounter + 1) + '/'
    else
      s := FillMangaSiteHost(PERVEDEN_ID, URL) + IntToStr(workCounter + 1) + '/';
    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i := parse.Count - 1 downto 0 do
        if (Pos('"mainImg"', parse[i]) > 0) then
        begin
          imgURL := GetVal(parse[i], 'src');
          if Copy(imgURL, 1, 2) = '//' then
            imgURL := 'http:' + imgURL;
          manager.container.PageLinks[workCounter] := imgURL;
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
