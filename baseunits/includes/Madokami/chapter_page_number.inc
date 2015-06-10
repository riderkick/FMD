  function GetMadokamiPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    datapath_, datafiles_: string;
  begin
    l := TStringList.Create;

    Result := GetPage(TObject(l),
      FillMangaSiteHost(MADOKAMI_ID, URL),
      manager.container.manager.retryConnect);

    parse := TStringList.Create;
    ParseHTML(l.Text, parse);
    l.Free;
    datapath_ := '';
    datafiles_ := '';
    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count-1 do
        if GetVal(parse[i], 'id') = 'reader' then
        begin
          datapath_ := GetVal(parse[i], 'data-path');
          datafiles_ := GetVal(parse[i], 'data-files');
          Break;
        end;
    end;
    parse.Free;

    if (datapath_ <> '') and (datafiles_ <> '') then
    begin
      datafiles_ := Trim(TrimChar(datafiles_, ['[', ']']));
      datafiles_ := StringReplace(datafiles_, '&quot;', '"', [rfIgnoreCase, rfReplaceAll]);
      datafiles_ := StringReplace(datafiles_, '\', '', [rfReplaceAll]);
      with manager.container do
      begin
        PageLinks.Delimiter := ',';
        PageLinks.DelimitedText := datafiles_;
        if PageLinks.Count > 0 then
          for i := 0 to PageLinks.Count-1 do
            PageLinks[i] := EncodeURL(WebsiteRoots[MADOKAMI_ID, 1] + '/reader/image?path=' +
              datapath_ + '&file=' + PageLinks[i]);
      end;
    end;
  end;
