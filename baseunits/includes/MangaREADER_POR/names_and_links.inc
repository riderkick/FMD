  function MangaREADER_PORGetNamesAndLinks: Byte;
  var
    i: LongInt;
    s: String;
    parser: TJSONParser;
    Data: TJSONData;
    jobject: TJSONObject;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(source),
      WebsiteRoots[MANGAREADER_POR_ID, 1] + '/AJAX/listaMangas/all', 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    //UTF8BOM:string=#$EF#$BB#$BF
    //Remove BOM header, jsonparser can't deal with BOM
    s := Source.Text;
    if Length(s) > 3 then
    begin
      if Copy(s, 1, 3) = UTF8BOM then
        Delete(s, 1, 3);
    end;
    {$IF (FPC_FULLVERSION >= 30101)}
    parser := TJSONParser.Create(s, [joUTF8]);
    {$ELSE}
    parser := TJSONParser.Create(s, True);
    {$ENDIF}
    try
      Data := parser.Parse;
      try
        if Assigned(Data) then
        begin
          if Data.JSONType = jtArray then
          begin
            for i := 0 to Data.Count - 1 do
            begin
              jobject := TJSONObject(Data.Items[i]);
              ANames.Add(jobject.Strings['title']);
              ALinks.Add(StringReplace(jobject.Strings['serie_url'],
                WebsiteRoots[MANGAREADER_POR_ID, 1], '', []));
            end;
          end;
        end;
      finally
        Data.Free;
      end;
    finally
      parser.Free;
    end;
    Result := NO_ERROR;
    Source.Free;
  end;
