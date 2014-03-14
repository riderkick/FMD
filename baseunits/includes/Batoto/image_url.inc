  function GetBatotoImageURL: Boolean;
  var
    isNormalReaderStyle: Boolean = FALSE;
    i: Cardinal;
    l: TStringList;
    s: String;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[BATOTO_ID,1] + URL + '/'+IntToStr(workCounter+1),
                     manager.container.manager.retryConnect);

    if l.Count > 0 then
    begin
      Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText:= OnText;
      Parser.SlowExec;
      Parser.Free;
    end;

    if parse.Count > 0 then
    begin
      for i:= 0 to parse.Count-1 do
      begin
        if (Pos('page_select', parse.Strings[i])<>0) then
        begin
          isNormalReaderStyle:= TRUE;
          break;
        end;
      end;

      case isNormalReaderStyle of
        TRUE:
          begin
            for i:= 0 to parse.Count-1 do
              if (GetTagName(parse.Strings[i]) = 'img') AND
                 (Pos('batoto.net/comics', parse.Strings[i])>0) AND
                 (Pos('z-index: 1003', parse.Strings[i])>0) then
              begin
                manager.container.pageLinks.Strings[workCounter]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
                break;
              end;
          end;
        FALSE:
          begin
            manager.container.pageLinks.Clear;
            for i:= 0 to parse.Count-1 do
              if (i+1 < parse.Count) AND
                 (GetTagName(parse.Strings[i]) = 'img') AND
                 (Pos('<br/>', parse.Strings[i+1])>0) then
              begin
                manager.container.pageLinks.Add(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
              end;
          end;
      end;
    end;

    parse.Free;
    l.Free;
  end;
