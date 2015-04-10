  function GetJapanShinPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(JAPANSHIN_ID, URL),
      manager.container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      //class="tbtitle dropdown_parent dropdown_right
      for i := 0 to parse.Count - 1 do
        if (Pos('onClick="changePage', parse.Strings[i]) > 0) and
          (Pos('return false', parse.Strings[i]) > 0) then
          Inc(manager.container.PageNumber);
    end;
    parse.Free;
    l.Free;
  end;
