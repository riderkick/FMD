  function GetNineMangaDirectoryPageNumber: Byte;
  var
    SiteID: Cardinal;
    p: Integer;
  begin
    Source.Free;
    Result := 0;
    BROWSER_INVERT := True;
    //I can't get manga directory total pages. Its not available on any page
    //The only option to get total pages is just checking manually with browser :(
    p := advancedfile.ReadInteger('UpdateListDirectoryPageNumber', website, -1);
    if p > 0 then
      Page := p
    else
    begin
      SiteID := GetMangaSiteID(website);
      case SiteID of
        NINEMANGA_ID: Page := 520;  //latest check = 513 (01-11-2014)
        NINEMANGA_ES_ID: Page := 596;
        NINEMANGA_CN_ID: Page := 778;
        NINEMANGA_RU_ID: Page := 205;
        NINEMANGA_BR_ID: Page := 56;
        NINEMANGA_IT_ID: Page := 50;
        NINEMANGA_DE_ID: Page := 30;
        else
          Page := 500;                 //not checked yet = 500 default
      end;
    end;
  end;
