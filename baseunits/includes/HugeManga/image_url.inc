  function GetHugeMangaImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(HUGEMANGA_ID, URL) + '/' + IntToStr(Task.Container.WorkCounter + 1));
    Result := GetPage(TObject(l),
      s,
      Task.Container.manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if (Pos('class="picture"', parse[i]) > 0) then
        begin
          Task.Container.PageLinks[Task.Container.WorkCounter] :=
            EncodeURL(WebsiteRoots[HUGEMANGA_ID, 1] + HUGEMANGA_BROWSER +
            GetVal(parse[i], 'src'));
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
