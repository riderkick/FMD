  function GetSenMangaImageURL: Boolean;
  var
    i: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    try
      Result := GetPage(TObject(l),
        FillMangaSiteHost(SENMANGA_ID, URL + '/' + IntToStr(workCounter + 1) + '/'),
        manager.container.Manager.retryConnect);
      if Result then
      begin
        parse := TStringList.Create;
        try
          Parser := THTMLParser.Create(l.Text);
          try
            Parser.OnFoundTag := OnTag;
            Parser.OnFoundText := OnText;
            Parser.Exec;
          finally
            Parser.Free;
          end;
          if parse.Count > 0 then
            for i := 0 to parse.Count - 1 do
              if (GetTagName(parse[i]) = 'img') and
                (GetVal(parse[i], 'id') = 'img_mng_enl') then
              begin
                manager.container.PageLinks[workCounter] := GetVal(parse[i], 'src');
                Break;
              end;
        finally
          parse.Free;
        end;
      end;
    finally
      l.Free;
    end;
  end;
