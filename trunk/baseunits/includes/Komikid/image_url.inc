  function GetKomikidImageURL: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(WebsiteRoots[KOMIKID_ID,1] + URL + '/' + IntToStr(workCounter+1));
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count>0 then
    begin
      for i:= 0 to parse.Count-1 do
        if (Pos('class="lazyload_ad"', parse.Strings[i])>0) then
        begin
          s:= '';
          s:= GetAttributeValue(GetTagAttribute(parse.Strings[i-8], 'src='));
          if s = '' then
            s:= GetAttributeValue(GetTagAttribute(parse.Strings[i-7], 'src='));
          if Pos('http://', s) = 0 then
            s:= WebsiteRoots[KOMIKID_ID,1] + KOMIKID_BROWSER + s;
          manager.container.pageLinks.Strings[workCounter]:= EncodeURL(s);
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
