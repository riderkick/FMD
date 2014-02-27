  function GetKissMangaImageURL: Boolean;
  var
    s, imageURL: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[KISSMANGA_ID,1] + URL,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
      begin
        if Pos('lstImages.push("', parse.Strings[i]) > 0 then
        begin
          s:= parse.Strings[i];
          repeat
            j:= Pos('lstImages.push("', s);
            imageURL:= DecodeUrl(GetString(s, 'lstImages.push("', '");'));
            if (Pos('googleusercontent', imageURL) > 0) AND
               (Pos('proxy?', imageURL) > 0) then
            begin
              imageURL:= GetString(imageURL + '<~>', '*&url=', '<~>');
            end;
            manager.container.pageLinks.Add(EncodeUrl(imageURL));
            Delete(s, Pos('lstImages.push("', s), 16);
            j:= Pos('lstImages.push("', s);
          until j = 0;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
