  function GetDynastyScansDirectoryPageNumber: Byte;
  begin
    Source.Free;
    APage := Length(DYNASTYSCANS_BROWSER);
    Result := NO_ERROR;
  end;
