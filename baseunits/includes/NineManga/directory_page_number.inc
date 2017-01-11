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
    p := advancedfile.ReadInteger('UpdateListDirectoryPageNumber', AWebsite, -1);
    if p > 0 then
      APage := p
    else
    begin
      SiteID := GetMangaSiteID(AWebsite);
      case SiteID of
        NINEMANGA_ID: APage := 520;  //latest check = 513 (01-11-2014)
        NINEMANGA_ES_ID: APage := 596;
        NINEMANGA_CN_ID: APage := 778;
        NINEMANGA_RU_ID: APage := 205;
        NINEMANGA_BR_ID: APage := 56;
        NINEMANGA_IT_ID: APage := 50;
        NINEMANGA_DE_ID: APage := 30;
        else
          APage := 500;                 //not checked yet = 500 default
      end;
    end;
  end;
