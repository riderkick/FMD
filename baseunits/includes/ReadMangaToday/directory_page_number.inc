  function GetReadMangaTodayDirectoryPageNumber: Byte;
  begin
    Source.Free;
    Page := Length(ALPHA_LIST);
    Result := NO_ERROR;
  end;
