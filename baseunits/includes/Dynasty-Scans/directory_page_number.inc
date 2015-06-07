  function GetDynastyScansDirectoryPageNumber: Byte;
  begin
    Source.Free;
    Page := Length(DYNASTYSCANS_BROWSER);
    Result := NO_ERROR;
  end;
