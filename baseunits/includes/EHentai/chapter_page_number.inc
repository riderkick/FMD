  function GetEHentaiPageNumber: Boolean;
  var
    s: String;
    i, j, g, p: Cardinal;
    l: TStringList;
    regx: TRegExpr;
    cw: Boolean;
  begin
    regx := TRegExpr.Create;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(EHENTAI_ID, URL);
    regx.Expression := '^(.*)/\?p=\d.*$';
    s := regx.Replace(s, '$1', True);
    s := ReplaceRegExpr('\?nw=.+$', s, '', False);
    if not ExecRegExpr('/$', s) then
      s := s + '/';
    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);

    // check if content warning
    cw := False;
    if l.Count > 0 then
    begin
      for i := 0 to l.Count - 1 do
      begin
        if Pos('>Content Warning</', l[i]) > 0 then
        begin
          cw := True;
          Break;
        end;
      end;
      if cw then
      begin
        s := s + '?nw=session';
        //s := s + '?nw=always';
        Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);
      end;
    end;

    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      g := 0;
      p := 0;
      //get images page count  method 1
      regx.Expression := '^.*onclick="sp\((\d+)\).*$';
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('onclick="sp(', parse.Strings[i]) > 0) then
        begin
          g := StrToIntDef(regx.Replace(parse.Strings[i], '$1', True), 0);
          if g > p then
            p := g;
        end;
      end;

      {//get images page count  method 2
      for i := 0 to parse.Count - 1 do
      begin
          if (Pos('Images:', parse.Strings[i])>0) then
            if (Pos('class="gdt1"', parse.Strings[i-1])>0) AND
               (Pos('</td>', parse.Strings[i+1])>0) AND
               (Pos('class="gdt2"', parse.Strings[i+2])>0) then  //to make sure not getting false line
          begin
            g := Pos('@', parse.Strings[i+3]);
            if (g>0) then
            begin
              try
                s := Trim(Copy(parse.Strings[i+3], 1, g-1));
                g := StrToInt(s);
                p := g div 40;        // per pages contain max 40 images
                if g mod 40 > 0 then
                p := p+1;
              except
                p := 2;
              end;
            end
            else
              p := 2;

            Dec(p); //1 is page 2
            Break;
          end;
      end; }

      //manager.container.PageNumber := 0;
      //manager.container.PageContainerLinks.Clear;

      //get images page in first page container
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('class="gdtm"', parse.Strings[i]) > 0) then
        begin
          s := GetAttributeValue(GetTagAttribute(parse.Strings[i + 2], 'href='));
          s := StringReplace(s, WebsiteRoots[EHENTAI_ID, 1] + '/s/', '', []);
          manager.container.PageContainerLinks.Add(s);
        end;
      end;

      //get all images page for the rest of page container
      if p > 0 then
      begin
        for i := 1 to p do
        begin
          parse.Clear;
          l.Clear;
          s := StringReplace(URL, WebsiteRoots[EHENTAI_ID, 1], '', []);
          s := StringReplace(s, '?nw=session', '', [rfReplaceAll]);
          s := StringReplace(s, '?nw=always', '', [rfReplaceAll]);
          s := WebsiteRoots[EHENTAI_ID, 1] + s;
          //if s[Length(s)]='/' then
          //SetLength(s, Length(s)-1);
          if s[Length((s))] <> '/' then
            s := s + '/';
          s := s + '?p=' + IntToStr(i);
          //s:= s+'?nw=session';

          Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);
          Parser := THTMLParser.Create(PChar(l.Text));
          Parser.OnFoundTag := OnTag;
          Parser.OnFoundText := OnText;
          Parser.Exec;
          Parser.Free;

          if parse.Count > 0 then
            for j := 0 to parse.Count - 1 do
              if (Pos('class="gdtm"', parse.Strings[j]) > 0) then
              begin
                s := GetAttributeValue(GetTagAttribute(parse.Strings[j + 2], 'href='));
                s := StringReplace(s, WebsiteRoots[EHENTAI_ID, 1] + '/s/', '', []);
                manager.container.PageContainerLinks.Add(s);
              end;
        end;
      end;
      manager.container.PageNumber := manager.container.PageContainerLinks.Count;
      manager.container.PageLinks.Clear;
      while manager.container.PageLinks.Count < manager.container.PageContainerLinks.Count do
        manager.container.PageLinks.Add('G')
    end;
    parse.Free;
    l.Free;
    regx.Free;
  end;
