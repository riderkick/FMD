  function GetReadHentaiMangaImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
    s: String;
  begin
    l := TStringList.Create;
    s := FillMangaSiteHost(READHENTAIMANGA_ID, URL);
    if Length(s) > 0 then
      if s[Length(s)] <> '/' then
         s := s + '/';
    s := s + IntToStr(workCounter + 1) + '/';
    Result := GetPage(TObject(l), s , manager.container.Manager.retryConnect);

    if Self.Terminated then
    begin
      l.Free;
      parse.Free;
      Exit;
    end;

    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    l.Free;
    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<img', parse[i]) > 0) and (Pos('id="main_img"', parse[i]) > 0) then
        begin
          s := GetVal(parse[i], 'src');
          if (s <> '') and (Pos('&#38;#x', parse[i]) > 0) then
          begin
            s := StringReplace(s, '&#38;#x', '%', [rfIgnoreCase, rfReplaceAll]);
            s := StringReplace(s, ';', '', [rfReplaceAll]);
          end;
          s := URLDecode(s);
          manager.container.PageLinks[workCounter] := s;
          Break;
        end;
      end;
    end;
    parse.Free;
  end;
