function   MangaREADER_PORGetNamesAndLinks: Byte;
var
  tmp: Integer;
  i  : Cardinal;
  url: String;
  sstream: TStringStream;
  parser : TJSONParser;
  data   : TJSONData;
begin
  Result:= INFORMATION_NOT_FOUND;
  url:= 'http://www.mangareader.com.br/AJAX/listaMangas/all';
  if NOT GetPage(TObject(source), url, 0) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;
  sstream:= TStringStream.Create(source.Text);
  parser:= TJSONParser.Create(sstream);
  try
    data:= Parser.Parse;
    if data <> nil then
    begin
      if data.Count > 0 then
      begin
        Result:= NO_ERROR;
        for i:= 0 to data.Count-1 do
        begin
          names.Add(Data.Items[i].Items[2].AsString);
          links.Add(StringReplace(Data.Items[i].Items[8].AsString, WebsiteRoots[MangaREADER_POR_ID, 1], '', []));
        end;
      end;
      data.Free;
    end;
  except
    on E: Exception do
      MessageDlg('Exception occured: ', E.Message, mtConfirmation, [mbYes], 0);
  end;
  sstream.Free;
  parser.Free;
  source.Free;
end;
