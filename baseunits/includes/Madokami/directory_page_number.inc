  function GetMadokamiDirectoryPageNumber: Byte;
  begin
    Source.Free;
    Page := Length(MADOKAMI_BROWSER);
    Result := NO_ERROR;
  end;
