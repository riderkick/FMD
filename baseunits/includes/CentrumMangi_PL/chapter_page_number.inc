  function GetCentrumMangi_PLPageNumber: Boolean;
  var
    s: String;
    Count: Cardinal = 0;
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(CENTRUMMANGI_PL_ID, URL));
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
        if (Pos('</select>', parse.Strings[i]) > 0) then
          if Count > 0 then
          begin
            s := parse.Strings[i - 2];
            manager.container.PageNumber := StrToInt(TrimLeft(TrimRight(s)));
            Break;
          end
          else
            Inc(Count);
      end;
    end;
    parse.Free;
    l.Free;
  end;
