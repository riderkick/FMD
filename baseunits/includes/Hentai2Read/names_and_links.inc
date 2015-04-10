  function Hentai2ReadGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
    regx: TRegExpr;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), HENTAI2READ_ROOT + HENTAI2READ_BROWSER +
      IntToStr(StrToInt(URL) + 1) + '/?text-ver', 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count = 0 then
    begin
      Source.Free;
      Exit;
    end;
    regx := TRegExpr.Create;
    regx.ModifierI := True;
    regx.Expression := '^.+\sdj\s[-_]\s(.+)$';
    for i := 0 to parse.Count - 1 do
    begin
      //if Pos('class="lst-anm-ifo"', parse.Strings[i])>0 then
      if Pos('class="mng_det_pop"', parse[i]) > 0 then
      begin
        begin
          Result := NO_ERROR;
          s := Trim(StringFilter(GetAttributeValue(
            GetTagAttribute(parse[i], 'title='))));
          s := regx.Replace(s, '$1', True);
          names.Add(s);
          links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse[i], 'href=')),
            HENTAI2READ_ROOT, '', []));
        end;
      end;
    end;
    regx.Free;
    Source.Free;
  end;