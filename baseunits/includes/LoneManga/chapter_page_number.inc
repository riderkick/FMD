  function GetLoneMangaPageNumber: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;

    s := 'showAll=true&design=Show+in+Long+Strip+Mode';
    FHTTP.Sock.Tag := 100; //POST
    FHTTP.Document.Clear;
    FHTTP.Document.Write(PChar(s)^, Length(s));

    Result := GetPage(TObject(l),
      DecodeUrl(FillMangaSiteHost(LONEMANGA_ID, URL)),
      manager.container.manager.retryConnect);

    ParseHTML(l.Text, parse);
    if parse.Count > 0 then
    begin
      manager.container.PageLinks.Clear;
      for i := 0 to parse.Count-1 do
      begin
        if GetTagName(parse[i]) = 'img' then
          WriteLog('Scan: '+parse[i]);
        if GetVal(parse[i], 'class') = 'imageWrapper' then
          if GetTagName(parse[i+2]) = 'img' then
            manager.container.PageLinks.Add(GetVal(parse[i+2], 'src'));
      end;
      manager.container.PageNumber := manager.container.PageLinks.Count;
    end;
    parse.Free;
    l.Free;
  end;
