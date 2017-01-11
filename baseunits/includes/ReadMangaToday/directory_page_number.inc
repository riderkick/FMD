  function GetReadMangaTodayDirectoryPageNumber: Byte;
  begin
    Source.Free;
    APage := Length(ALPHA_LIST);
    Result := NO_ERROR;
  end;
