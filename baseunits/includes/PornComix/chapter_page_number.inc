  function GetPornComixPageNumber(MangaID: Cardinal): Boolean;
  var
    i: LongInt;
    s: String;
    l: TStringList;
    regx: TRegExpr;
    isGetImageURL: Boolean = False;
    RepeatGet: Boolean = True;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MangaID, URL));
    while RepeatGet do
    begin
      RepeatGet := False;
      l.Clear;
      parse.Clear;
      Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);
      Parser := THTMLParser.Create(PChar(l.Text));
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
      Parser.Free;

      manager.container.PageNumber := 0;
      manager.container.PageLinks.Clear;
      manager.container.PageContainerLinks.Clear;
      if parse.Count > 0 then
      begin
        regx := TRegExpr.Create;
        try
          isGetImageURL := False;
          regx.Expression := '^^(.*)/img-(\w+)\..*$';
          for i := 0 to  parse.Count - 1 do
          begin
            if (Pos('<div', parse[i]) <> 0) and (Pos('class="single-post', parse[i]) <> 0) then
              isGetImageURL := True;
            if isGetImageURL and (Pos('</div', parse[i]) <> 0) then
              Break;
            if isGetImageURL and
              (Pos('<a', parse[i]) <> 0) and
              ((Pos('target="_blank', parse[i]) <> 0) or
               (Pos('rel="nofollow', parse[i]) <> 0)) then
            begin
              Inc(manager.container.PageNumber);
              s := GetVal(parse[i], 'href');
              if (Pos('<img', parse[i + 1]) <> 0) and
                (Pos('/upload/small/', parse[i + 1]) <> 0) then
              begin
                s := GetVal(parse[i + 1], 'src');
                s := StringReplace(s, '/small/', '/big/', [rfIgnoreCase]);
                manager.container.PageLinks.Add(s);
                manager.container.PageContainerLinks.Add('W');
              end
              else if (Pos('imagetwist.', s) <> 0) or
                (Pos('imgmega.', s) <> 0) or
                (Pos('imgchili.', s) <> 0) then
              begin
                manager.container.PageContainerLinks.Add(s);
                manager.container.PageLinks.Add('W');
              end
              else if regx.Exec(s) then
              begin
                s := regx.Replace(s, '$1/dlimg.php?id=$2', True);
                manager.container.PageLinks.Add(s);
                manager.container.PageContainerLinks.Add('W');
              end
              else if (Pos('/category/', s) = 0) and
                ((Pos(WebsiteRoots[PORNCOMIX_ID, 1], s) <> 0) or
                (Pos(WebsiteRoots[XXCOMICS_ID, 1], s) <> 0) or
                (Pos(WebsiteRoots[XXCOMICSMT_ID, 1], s) <> 0) or
                (Pos(WebsiteRoots[XXCOMICS3D_ID, 1], s) <> 0) or
                (Pos(WebsiteRoots[PORNCOMIXRE_ID, 1], s) <> 0) or
                (Pos(WebsiteRoots[PORNCOMIXIC_ID, 1], s) <> 0) or
                (Pos(WebsiteRoots[PORNXXXCOMICS_ID, 1], s) <> 0) or
                (Pos('.xxcomics.', s) <> 0) or
                (Pos('.porncomix.', s) <> 0)) then
              begin
                RepeatGet := True;
                if Assigned(FHTTP) then
                  FHTTP.Cookies.Clear;
                Break;
              end
              else
                Dec(manager.container.PageNumber);
            end;
          end;
        finally
          regx.Free;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
