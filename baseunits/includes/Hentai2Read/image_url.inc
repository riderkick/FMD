  function GetHentai2ReadImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(HENTAI2READ_ID, URL) + IntToStr(workCounter + 1) + '/',
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
        if (GetTagName(parse[i]) = 'img') and
          (GetVal(parse[i], 'id') =
          'img_mng_enl') then
        begin
          manager.container.PageLinks[workCounter] :=
            GetVal(parse[i], 'src');
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
