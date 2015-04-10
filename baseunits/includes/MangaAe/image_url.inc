  function GetMangaAeImageURL: Boolean;
  var
    s: String;
    i: LongInt;
    l: TStringList;
    isImageURL: Boolean = False;
  begin
    l := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MANGAAE_ID, URL) + '/' + IntToStr(workCounter + 1));
    Result := GetPage(TObject(l),
      s,
      manager.container.manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('id="showchaptercontainer', parse[i]) <> 0) then
          isImageURL := True;
        if isImageURL and (Pos('<img', parse[i]) <> 0) then
        begin
          manager.container.PageLinks[workCounter] := GetVal(parse[i], 'src');
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
