  function GetAnimeStoryImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(ANIMESTORY_ID, URL) + IntToStr(workCounter + 1));
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
      for i := 0 to parse.Count - 1 do
        if (Pos('id="chpimg"', parse[i]) > 0) then
        begin
          manager.container.PageLinks[workCounter] :=
            DecodeURL(GetVal(parse[i], 'src'));
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
