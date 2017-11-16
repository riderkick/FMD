  function GetEGScansPageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(EGSCANS_ID, URL) + '/1');
    Result := GetPage(TObject(l),
      s,
      Task.Container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := @OnTag;
    Parser.OnFoundText := @OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageNumber := 0;
      for i := parse.Count - 1 downto 2 do
      begin
        if (Pos('</span>', parse[i]) > 0) then
        begin
          s := parse[i - 4];
          Task.Container.PageNumber :=
            StrToInt(TrimLeft(TrimRight(GetString(s + ' ', 'of ', ' '))));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
