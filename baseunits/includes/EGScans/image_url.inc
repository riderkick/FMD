  function GetEGScansImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(EGSCANS_ID, URL) + '/' + IntToStr(WorkId + 1));
    Result := GetPage(TObject(l),
      s,
      Task.Container.manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := @OnTag;
    Parser.OnFoundText := @OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      Task.Container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
        if (Pos('<img ondragstart', parse[i]) > 0) then
        begin
          Task.Container.PageLinks.Add(WebsiteRoots[EGSCANS_ID, 1] +
            '/' + EncodeURL(GetVal(parse[i], 'src')));
        end;
    end;
    parse.Free;
    l.Free;
  end;
