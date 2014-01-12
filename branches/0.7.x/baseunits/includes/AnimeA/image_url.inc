  function GetAnimeAImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     WebsiteRoots[ANIMEA_ID,1] +
                     StringReplace(URL, '.html', '', []) +
                     '-page-'+IntToStr(workCounter+1)+'.html',
                     manager.container.manager.retryConnect);
    for i:= 0 to l.Count-1 do
      if (Pos('class="mangaimg', l.Strings[i])<>0) then
      begin
        manager.container.pageLinks.Strings[workCounter]:= GetString(l.Strings[i], '<img src="', '"');
        break;
      end;
    l.Free;
  end;
