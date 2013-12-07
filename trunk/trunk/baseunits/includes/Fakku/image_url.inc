  function GetFakkuImageURL: Boolean;
  var
    i, j  : Cardinal;
    l     : TStringList;
    imgURL: String;
  begin
    l:= TStringList.Create;
    // get number of pages
    Result:= GetPage(TObject(l),
                     WebsiteRoots[FAKKU_ID,1] + StringReplace(URL, '/read', '', []){ + '#page' + IntToStr(workCounter+1)},
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    j:= 0;
    if parse.Count>0 then
    begin
      i:= 0;
      manager.container.pageLinks.Clear;
      while i < parse.Count-1 do
      begin
        if (Pos('favorites', parse.Strings[i])>0) AND
           (Pos('pages', parse.Strings[i+4])>0) then
        begin
          j:= StrToInt(TrimRight(TrimLeft(parse.Strings[i+2])));
          break;
        end;
        Inc(i);
      end;
    end;
    // get link pages
    l.Clear;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[FAKKU_ID,1] + URL + '#page' + IntToStr(workCounter+1),
                     manager.container.manager.retryConnect);
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      i:= 0;
      manager.container.pageLinks.Clear;
      while i < parse.Count-1 do
      begin
        if (Pos('return ''http://c.fakku.net/', parse.Strings[i])>0) then
        begin
        //  manager.container.pageLinks.Strings[workCounter]:=
          imgURL:= 'http://c.fakku.net/' + GetString(parse.Strings[i], '''http://c.fakku.net/', '''');
          break;
        end
        else
        if (Pos('return ''http://t.fakku.net/', parse.Strings[i])>0) then
        begin
        //  manager.container.pageLinks.Strings[workCounter]:=
          imgURL:= 'http://t.fakku.net/' + GetString(parse.Strings[i], '''http://t.fakku.net/', '''');
          break;
        end
        else
        if (Pos('return ''http://cdn.fakku.net/', parse.Strings[i])>0) then
        begin
        //  manager.container.pageLinks.Strings[workCounter]:=
          imgURL:= 'http://cdn.fakku.net/' + GetString(parse.Strings[i], '''http://cdn.fakku.net/', '''');
          break;
        end;
        Inc(i);
      end;
    end;
    // build page files
    for i:= 1 to j do
    begin
     // s:= imgURL + Format('%3.3d.jpg', [i]);
      manager.container.pageLinks.Add(imgURL + Format('%3.3d.jpg', [i]));
    end;
    parse.Free;
    l.Free;
  end;
