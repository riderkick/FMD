function   GetPururinDirectoryPageNumber: Byte;
var
  i,g : Cardinal;
begin
  Result:= INFORMATION_NOT_FOUND;
  if NOT GetPage(TObject(source), WebsiteRoots[PURURIN_ID,1], 0) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;
  Parser.Free;
  if parse.Count=0 then
  begin
    source.Free;
    exit;
  end;
  for i:= parse.Count-1 downto 2 do
  begin
    if (Pos('/browse/0/11/2.html', parse.Strings[i]) > 0) then
    begin
      Page:= StrToInt(Trim(parse.Strings[i-2])) div 3;
      break;
    end;
  end;
  source.Free;
end;
