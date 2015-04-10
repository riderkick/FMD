  function GetLectureEnLignePageNumber: Boolean;
  var
    s: String;
    i: Cardinal;
    l: TStringList;
    server_name, slug, chapters, ext, bonus, number: String;
    nb_images: Cardinal;
    regx: TRegExpr;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(LECTUREENLIGNE_ID, URL));
    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);

    if Self.Terminated then
    begin
      l.Free;
      parse.Free;
      Exit;
    end;

    // getting javascript
    if l.Count > 0 then
    begin
      for i := 0 to l.Count - 1 do
      begin
        if Pos('server_name=', l.Strings[i]) > 0 then
        begin
          s := l.Strings[i];
        end;
      end;

      l.Clear;
      ExtractStrings([';'], [], PChar(s), l);
      if l.Count > 0 then
      begin
        regx := TRegExpr.Create;
        for i := 0 to l.Count - 1 do
        begin
          //get server_name
          if Pos('server_name=', l.Strings[i]) > 0 then
          begin
            regx.Expression := '^.*server_name=''([^'']+)''.*$';
            server_name := regx.Replace(l.Strings[i], '$1', True);
          end;
          //get slug/manga title
          if Pos('slug=', l.Strings[i]) > 0 then
          begin
            regx.Expression := '^.*slug=''([^'']+)''.*$';
            slug := regx.Replace(l.Strings[i], '$1', True);
          end;
          //get current chapters, nb_images, ext, bonus
          if Pos('chapters["current"]', l.Strings[i]) > 0 then
          begin
            //chapters
            regx.Expression := '^.*number:(\d+).*$';
            chapters := regx.Replace(l.Strings[i], '$1', True);
            //nb_images
            regx.Expression := '^.*nb_images:(\d+).*$';
            nb_images := StrToIntDef(regx.Replace(l.Strings[i], '$1', True), 0);
            //ext
            regx.Expression := '^.*ext:"(\w+)".*$';
            ext := regx.Replace(l.Strings[i], '$1', True);
            //bonus
            regx.Expression := '^.*isBonus:(\d+).*$';
            if (regx.Replace(l.Strings[i], '$1', True) = '1') then
              bonus := 'bonus/'
            else
              bonus := '';
          end;
        end;
        regx.Free;

        manager.container.PageNumber := nb_images;
        //building images url
        for i := 1 to nb_images do
        begin
          // http://"+server_name+"/images/mangas/"+slug+"/"+n+chapters[e].number+"/"+i+t+"."+chapters[e].ext
          // http://www.lecture-en-ligne.com:80/images/mangas//5/26.jpg
          number := IntToStr(i);
          while Length(number) < Length(IntToStr(nb_images)) do
            number := '0' + number;
          manager.container.PageLinks.Add(
            'http://' + server_name + '/images/mangas/' + slug + '/' + bonus +
            chapters + '/' + number + '.' + ext
            );
        end;
      end;
    end;

    l.Free;
  end;
