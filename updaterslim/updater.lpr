program updater;

uses
  {$ifdef windows}
  windows,
  {$else}
  fileutil,
  {$endif}
  sysutils, process;

var
  exefile, zipexefile, updatepackagefile, maindir, ozipexefile, oexefile: String;
  counter: Integer;

{$R *.res}

begin
  if ParamCount<4 then
  begin
    writeln('Parameters insuficient, ',ParamCount,' of 4');
    readln;
    exit;
  end;
  exefile:=ParamStr(1);
  zipexefile:=ParamStr(2);
  updatepackagefile:=ParamStr(3);
  maindir:=ParamStr(4);
  if not (FileExists(zipexefile) or FileExists(updatepackagefile)) then Exit;
  oexefile:=ExtractFilePath(exefile)+'old_'+ExtractFileName(exefile);
  if FileExists(oexefile) then DeleteFile(oexefile);
  counter:=0;
  {$ifdef windows}
  windows.CopyFile(pchar(exefile),pchar(oexefile),false);
  {$else}
  CopyFile(exefile,oexefile);
  {$endif}
  while FileExists(exefile) do begin
    if DeleteFile(exefile) then break;
    Inc(counter);
    writeln('Waiting to close ',counter,'/',10,' ',exefile);
    Sleep(1000);
    if counter=10 then break;
  end;
  if not FileExists(exefile) then
  begin
    with TProcess.Create(nil) do try
      ozipexefile:=ExtractFilePath(zipexefile)+'old_'+ExtractFileName(zipexefile);
      if FileExists(ozipexefile) then
        DeleteFile(ozipexefile);
      RenameFile(zipexefile,ozipexefile);
      Executable:=ozipexefile;
      CurrentDirectory:=maindir;
      Parameters.Add('x');
      Parameters.Add(updatepackagefile);
      Parameters.Add('-o'+AnsiQuotedStr(maindir,'"'));
      Parameters.Add('-aoa');
      Options:=Options+[poWaitOnExit];
      Execute;
      if ExitStatus=0 then
      begin
        DeleteFile(oexefile);
        DeleteFile(ozipexefile);
        DeleteFile(updatepackagefile);
        Executable:=exefile;
        Parameters.Clear;
        Options:=Options-[poWaitOnExit];
        Execute;
      end
      else
      begin
        readln;
        if FileExists(oexefile) then
        begin
          if FileExists(exefile) then
            DeleteFile(oexefile)
          else
            RenameFile(oexefile,exefile);
        end;
        if FileExists(ozipexefile) then
        begin
          if FileExists(zipexefile) then
            DeleteFile(ozipexefile)
          else
            RenameFile(ozipexefile,zipexefile);
        end;
      end;
    finally
      Free;
    end;
  end
  else
  begin
    writeln('Can''t delete file ',exefile);
    readln;
  end;
end.

