  function GeLectureEnligneImageURL: Boolean;
  var
    s: string;
    i: Integer;
    l: TStringList;
  begin
    s := DecodeUrl(FillMangaSiteHost(LECTUREENLIGNE_ID, URL));
    if Pos('.htm', s) > 0 then
      s := ReplaceRegExpr('/\d+\.html?$', s, '', False);
    s := s + '/' + IntToStr(Task.Container.WorkCounter + 1) + '.html';
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      s,
      Task.Container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        if (GetTagName(parse[i]) = 'img') and
          (GetVal(parse[i], 'id') = 'image') then
        begin
          Task.Container.PageLinks[Task.Container.WorkCounter] := GetVal(parse[i], 'src');
          Break;
        end;
    parse.Free;
    l.Free;
  end;
