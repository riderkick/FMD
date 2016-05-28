  function GetKomikidImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
    regx: TRegExpr;
  begin
    l := TStringList.Create;
    s := EncodeURL(FillMangaSiteHost(KOMIKID_ID, URL) + '/' +
      IntToStr(WorkId + 1) + '/');
    Result := GetPage(TObject(l),
      s,
      Task.Container.Manager.retryConnect);
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
        if (Pos('<img', LowerCase(parse[i])) > 0) and
          (Pos('class="picture"', LowerCase(parse[i])) > 0) then
        begin
          s := GetVal(parse[i], 'src');
          regx := TRegExpr.Create;
          try
            regx.Expression := 'https?://';
            if not regx.Exec(s) then
              s := WebsiteRoots[KOMIKID_ID, 1] + '/' + s;
            s := EncodeURL(s);
            Task.container.PageLinks[WorkId] := s;
          finally
            regx.Free;
          end;
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
