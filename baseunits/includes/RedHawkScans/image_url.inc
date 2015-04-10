  function GetRedHawkScansImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(REDHAWKSCANS_ID, URL) + 'page/' +
      IntToStr(workCounter + 1));
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
        if (Pos('class="open"', parse.Strings[i]) > 0) then
        begin
          manager.container.PageLinks.Strings[workCounter] :=
            GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
