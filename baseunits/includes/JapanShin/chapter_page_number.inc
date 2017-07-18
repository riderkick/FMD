  function GetJapanShinPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(JAPANSHIN_ID, URL),
      Task.Container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageNumber := 0;
      //class="tbtitle dropdown_parent dropdown_right
      for i := 0 to parse.Count - 1 do
        if (Pos('onClick="changePage', parse[i]) > 0) and
          (Pos('return false', parse[i]) > 0) then
          Inc(Task.Container.PageNumber);
    end;
    parse.Free;
    l.Free;
  end;
