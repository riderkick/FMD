  function GetS2scanPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(S2SCAN_ID, URL) + 'page/1');
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
      for i := 1 to parse.Count - 1 do
      begin
        if (Pos('class="topbar_right"', parse.Strings[i]) > 0) then
        begin
          s := parse.Strings[i + 4];
          manager.container.pageNumber :=
            StrToInt(TrimLeft(TrimRight(GetString('~!@' + s, '~!@', ' '))));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
