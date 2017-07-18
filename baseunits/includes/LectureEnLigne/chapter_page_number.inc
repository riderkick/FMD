  function GetLectureEnLignePageNumber: Boolean;
  var
    s: string;
    i: Integer;
    l: TStringList;
    isGetPageNumber: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(LECTUREENLIGNE_ID, URL));
    if Pos('.htm', s) > 0 then
      s := ReplaceRegExpr('/\d+\.html?$', s, '', False);
    s := s + '/1.html';
    Result := GetPage(TObject(l),
      s,
      Task.Container.Manager.retryConnect);

    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
      begin
        if (GetTagName(parse[i]) = 'select') and
          (GetVal(parse[i], 'class') = 'pages') then
          isGetPageNumber := True;
        if isGetPageNumber and (GetTagName(parse[i]) = '/select') then
          Break;
        if isGetPageNumber and (GetTagName(parse[i]) = 'option') then
          Inc(Task.Container.PageNumber);
      end;
    parse.Free;
    l.Free;
  end;
