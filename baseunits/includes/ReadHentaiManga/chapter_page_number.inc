  function GetReadHentaiMangaPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
    s: String;
    isExtractPage: Boolean = False;
    isGetDirect: Boolean = True;
  begin
    manager.container.PageNumber := 0;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(READHENTAIMANGA_ID, URL);
    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);

    if isGetDirect then
    begin
      if l.Count > 0 then
      begin
        for i := 0 to l.Count - 1 do
        begin
          if Pos('var wpm_mng_rdr_img_lst', l[i]) > 0 then
          begin
            s := Trim(l[i]);
            s := GetString(s, '(''', ''')');
            s := Trim(URLDecode(s));
            s := TrimChar(s, ['[', ']', '"']);
            s := StringReplace(s, '\', '' , [rfReplaceAll]);
            s := StringReplace(s, '","', '\', [rfReplaceAll]);
            manager.container.PageLinks.Clear;
            manager.container.PageLinks.Delimiter := '\';
            manager.container.PageLinks.DelimitedText := s;
            manager.container.PageNumber := manager.container.PageLinks.Count;
            Break;
          end;
        end;
      end;
    end
    else
    begin
      Parser := THTMLParser.Create(PChar(l.Text));
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
      Parser.Free;
      if parse.Count > 0 then
      begin
        for i := 0 to parse.Count - 1 do
        begin
          if (Pos('<select', parse[i]) > 0) and
            (Pos('class="cbo_wpm_pag"', parse[i]) > 0) then
            isExtractPage := True;
          if isExtractPage and
            (Pos('</select', parse[i]) > 0) then
          begin
            isExtractPage := False;
            Break;
          end;
          if isExtractPage and
            (Pos('<option', parse[i]) > 0) then
          begin
            Inc(manager.container.PageNumber);
          end;
        end;
      end;
    end;
    l.Free;
    parse.Free;
  end;
