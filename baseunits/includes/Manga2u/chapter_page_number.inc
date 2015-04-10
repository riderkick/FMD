  function GetManga2uPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
    isStartGetPageNumber: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MANGA2U_ID, URL) + '1/');
    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('class="cbo_wpm_pag"', parse.Strings[i]) > 0) then
          isStartGetPageNumber := True;

        if (isStartGetPageNumber) and
          (Pos('</select>', parse.Strings[i]) > 0) then
        begin
          s := parse.Strings[i - 3];
          manager.container.PageNumber :=
            StrToInt(GetAttributeValue(GetTagAttribute(s, 'value=')));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
