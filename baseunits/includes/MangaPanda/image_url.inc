  function GetMangaPandaImageURL: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;

    if (Pos('.html', URL) > 0) and (Pos(SEPERATOR2, URL) > 0) then
    begin
      s := DecodeUrl(FillMangaSiteHost(MANGAPANDA_ID, URL));
      s := StringReplace(s, SEPERATOR2, '-' + IntToStr(workCounter + 1) + '/', []);
    end
    else
      s := DecodeUrl(FillMangaSiteHost(MANGAPANDA_ID, URL) + '/' + IntToStr(workCounter + 1));
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
        if (Pos('<img ', parse[i]) > 0) and
          (Pos('id="img"', parse[i]) > 0) and
          (Pos('name="img"', parse[i]) > 0) then
        begin
          manager.container.PageLinks[workCounter] := GetVal(parse[i], 'src');
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
