function GetSenMangaRAWInfoFromURL: Byte;
var
  i, j: Integer;
  isExtractChapters: Boolean = False;
begin
  mangaInfo.website := WebsiteRoots[SENMANGARAW_ID, 0];
  mangaInfo.url := FillMangaSiteHost(SENMANGARAW_ID, URL);
  if GetPage(TObject(Source), mangaInfo.url, Reconnect) then
  begin
    Parser := THTMLParser.Create(Source.Text);
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      parse.Clear;
      Parser.Exec;
    finally
      Parser.Free;
    end;
    Source.Free;

    if parse.Count > 0 then
    begin
      mangaInfo.genres := '';
      mangaInfo.summary := '';
      for i := 0 to parse.Count - 1 do
      begin
        //title
        if mangaInfo.title = '' then
          if (GetTagName(parse[i]) = 'h1') then
            if (GetTagName(parse[i + 1]) = 'a') then
            mangaInfo.title := parse[i + 2];
        //cover
        if (GetTagName(parse[i]) = 'img') and (Pos('/no_preview.jpg', parse[i]) > 0) then
        begin
          mangaInfo.coverLink := FixURL(GetVal(parse[i], 'src'));
          if Pos('http', mangaInfo.coverLink) <> 1 then
          begin
            if LeftStr(mangaInfo.coverLink, 1) <> '/' then
              mangaInfo.coverLink := '/' + mangaInfo.coverLink;
            mangaInfo.coverLink := WebsiteRoots[SENMANGARAW_ID, 1] + mangaInfo.coverLink;
          end;
        end;

        if (GetTagName(parse[i]) = 'strong') and (GetVal(parse[i], 'class') = 'data') then
        begin
          //author
          if Trim(parse[i + 1]) = 'Author:' then
            mangaInfo.authors := parse[i + 5];
          //artist
          if Trim(parse[i + 1]) = 'Artist:' then
            mangaInfo.artists := parse[i + 5];
          //status
          if Trim(parse[i + 1]) = 'Status:' then
          begin
            mangaInfo.status := Trim(parse[i + 5]);
            if mangaInfo.status = 'Complete' then
              mangaInfo.status := '0'
            else
              mangaInfo.status := '1';
          end;
          //genres
          if Trim(parse[i + 1]) = 'Categorize in:' then
            for j := i + 3 to parse.Count - 1 do
            begin
              if GetTagName(parse[j]) = 'strong' then
                Break
              else
              if GetTagName(parse[j]) = 'a' then
              begin
                if mangaInfo.genres = '' then
                  mangaInfo.genres := Trim(parse[j + 1])
                else
                  mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[j + 1]);
              end;
            end;
          //summary
          if Trim(parse[i + 1]) = 'Summary:' then
            for j := i + 3 to parse.Count - 1 do
            begin
              if GetTagName(parse[j]) = 'div' then
                Break
              else
              if LeftStr(Trim(parse[j]), 1) <> '<' then
                mangaInfo.summary := mangaInfo.summary + LineEnding + Trim(parse[j]);
            end;
        end;

        //chapters
        if (GetTagName(parse[i]) = 'table') and (GetVal(parse[i], 'width') = '100%') then
          isExtractChapters := True;
        if isExtractChapters then
        begin
          if GetTagName(parse[i]) = '/table' then
            isExtractChapters := False
          else
          if GetTagName(parse[i]) = 'a' then
          begin
            s := GetVal(parse[i], 'href');
            if RightStr(s, 2) = '/1' then
              SetLength(s, Length(s) - 2);
            mangaInfo.chapterLinks.Add(s);
            mangaInfo.chapterName.Add(parse[i + 1]);
          end;
        end;
      end;
      InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
      Result := NO_ERROR;
    end
    else
      Result := INFORMATION_NOT_FOUND;
  end
  else
    Result := NET_PROBLEM;
end;
