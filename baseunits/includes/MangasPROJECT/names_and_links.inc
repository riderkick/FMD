  function MangasPROJECTGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    url: String;
    sstream: TStringStream;
    parser: TJSONParser;
    Data: TJSONData;
  begin
    Result := INFORMATION_NOT_FOUND;
    url := 'http://www.mangasproject.net/AJAX/listaMangas/all';
    if not GetPage(TObject(Source), url, 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;
    sstream := TStringStream.Create(Source.Text);
    {$IF (FPC_FULLVERSION >= 30101)}
    parser := TJSONParser.Create(sstream, [joUTF8]);
    {$ELSE}
    parser := TJSONParser.Create(sstream, True);
    {$ENDIF}
    try
      Data := Parser.Parse;
      if Data <> nil then
      begin
        if Data.Count > 0 then
        begin
          Result := NO_ERROR;
          for i := 0 to Data.Count - 1 do
          begin
            ANames.Add(Data.Items[i].Items[0].AsString);
            ALinks.Add(StringReplace(Data.Items[i].Items[2].AsString,
              WebsiteRoots[MangasPROJECT_ID, 1], '', []));
          end;
        end;
        Data.Free;
      end;
    except
      on E: Exception do
        MessageDlg('Exception occured: ', E.Message, mtConfirmation, [mbYes], 0);
    end;
    sstream.Free;
    parser.Free;
    Source.Free;
  end;
