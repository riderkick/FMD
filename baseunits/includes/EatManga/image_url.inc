  function GetEatMangaImageURL: Boolean;
  var
    i: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      DecodeUrl(FillMangaSiteHost(EATMANGA_ID, URL) + 'page-' + IntToStr(workCounter + 1)),
      Task.Container.Manager.retryConnect);

    parse := TStringList.Create;
    try
      Parser := THTMLParser.Create(PChar(l.Text));
      try
        Parser.OnFoundTag := OnTag;
        Parser.OnFoundText := OnText;
        Parser.Exec;
      finally
        Parser.Free;
      end;

      if parse.Count > 0 then
      begin
        for i := 0 to parse.Count - 1 do
          if (GetTagName(parse[i]) = 'img') and
            (GetVal(parse[i], 'id') = 'eatmanga_image') then
          begin
            Task.Container.PageLinks[workCounter] := GetVal(parse[i], 'src');
            Break;
          end;
      end;
    finally
      parse.Free;
      l.Free;
    end;
  end;
