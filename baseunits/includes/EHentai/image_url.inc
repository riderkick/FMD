  function getEHentaiImageURL: Boolean;
  var
    i, RetryCount: Cardinal;
    prs: TStringList;
    hparser: THTMLParser;
    {s, }purl, iurl, base_url, startkey, gid, startpage, nl: String;
  begin
    Result := False;

    //Result := 'W';
    //s := manager.container.DownloadInfo.SaveTo +
    //  '/' + manager.container.ChapterName.Strings[
    //  manager.container.CurrentDownloadChapterPtr] + '/' +
    //  Format('%.3d', [workCounter + 1]);
    // Check to see if a file with similar name was already exist. If so then we
    // skip the download process.
    //if (FileExists(s + '.jpg')) or
    //  (FileExists(s + '.png')) or
    //  (FileExists(s + '.gif')) then
    //begin
    //  Result := 'D';
    //  Exit;
    //end;

    purl := manager.container.PageContainerLinks[workCounter];
    purl := FillMangaSiteHost(EHENTAI_ID, '/s/' + purl);
    RetryCount := 0;
    while not Result do
    begin
      iurl := '';
      prs := TStringList.Create;
      GetPage(TObject(prs), purl,
        manager.container.Manager.retryConnect);
      if Self.Terminated then
      begin
        prs.Free;
        Exit;
      end;

      parse := TStringList.Create;
      hparser := THTMLParser.Create(PChar(prs.Text));
      hparser.OnFoundTag := OnTag;
      hparser.OnFoundText := OnText;
      hparser.Exec;
      hparser.Free;

      if parse.Count > 0 then
        for i := 0 to parse.Count - 1 do
        begin
          if (Pos('<img ', parse[i]) > 0) and
            ((Pos(' style=', parse[i]) > 0) or
            (Pos(' id="img"', parse[i]) > 0) or
            (Pos('/keystamp=', parse.Strings[i]) > 0)) then
          begin
            iurl := GetAttributeValue(GetTagAttribute(parse[i], 'src='));
            iurl := StringReplace(iurl, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
          end;
          if Pos('onclick="return nl(', parse[i]) > 0 then
            nl := ReplaceRegExpr('^.*nl\((\d+)\).*$', parse[i], '$1', True);
        end;

      //new reload link
      if prs.Count > 0 then
      begin
        for i := 0 to prs.Count - 1 do
        begin
          if Pos('var base_url=', prs[i]) > 0 then
            base_url := GetValuesFromString(prs[i], '=');
          if Pos('var startkey=', prs[i]) > 0 then
            startkey := GetValuesFromString(prs[i], '=');
          if Pos('var gid=', prs[i]) > 0 then
            gid := GetValuesFromString(prs[i], '=');
          if Pos('var startpage=', prs[i]) > 0 then
            startpage := GetValuesFromString(prs[i], '=');
        end;
        if nl <> '' then
          purl := base_url + '/s/' + startkey + '/' + gid + '-' + startpage + '?nl=' + nl;
      end;
      parse.Free;
      prs.Free;

      //save image
      Result := SaveImage(Self,
        AHTTP,
        manager.container.MangaSiteID,
        iurl,
        lpath,
        Format('%.3d', [workCounter + 1]),
        prefix,
        1);

      if (manager.container.Manager.retryConnect <> 0) and
        (manager.container.Manager.retryConnect <= RetryCount) then
        Break;

      if not Result then
        Inc(RetryCount);
    end;
  end;
