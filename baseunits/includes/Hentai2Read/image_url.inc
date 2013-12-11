  function GetHentai2ReadImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     HENTAI2READ_ROOT + URL + IntToStr(workCounter+1)+'/',
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
        if (GetTagName(parse.Strings[i]) = 'img') AND
           (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'id='))='img_mng_enl') then
        begin
          manager.container.pageLinks.Strings[workCounter]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
