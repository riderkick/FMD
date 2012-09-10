{
        File: updatelist.pas
        License: GPLv3
        This unit is part of Free Manga Downloader
}

unit updatelist;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, data, baseunit;

type
  TUpdateMangaManagerThread = class;

  TUpdateMangaThread = class(TThread)
  protected
    names,
    links             : TStringList;
    isTerminated,
    isSuspended,
    isCheckNameAndLink: Boolean;
    workPtr           : Cardinal;
    manager           : TUpdateMangaManagerThread;
    Info              : TMangaInformation;
    procedure   Execute; override;
    procedure   DecThreadCount;
    procedure   UpdateNamesAndLinks;
  public
    constructor Create;
    destructor  Destroy; override;
  end;

  TUpdateMangaManagerThread = class(TThread)
  protected
    procedure   Execute; override;
    procedure   ShowResult;
  public
    isTerminated,
    isSuspended,
    isDoneUpdateInformation,
    isDoneUpdateNecessary,
    isDoneCheckNameAndLink: Boolean;
    dataProcess           : TDataProcess;
    names,
    links,
    websites              : TStringList;
    website               : String;
    threadCount,
    numberOfThreads       : Cardinal;
    threads               : array of TUpdateMangaThread;
    constructor Create;
    destructor  Destroy;
  end;

implementation

uses mainunit;

// ----- TUpdateMangaThread -----

constructor TUpdateMangaThread.Create;
begin
  names:= TStringList.Create;
  links:= TStringList.Create;
  isSuspended:= TRUE;
  Info:= TMangaInformation.Create;
  FreeOnTerminate:= TRUE;
  isTerminated   := FALSE;
  inherited Create(FALSE);
end;

destructor  TUpdateMangaThread.Destroy;
begin
  names.Free;
  links.Free;
  Info.Free;
  isTerminated:= TRUE;
  inherited Destroy;
end;

procedure   TUpdateMangaThread.DecThreadCount;
begin
  Dec(manager.threadCount);
end;

procedure   TUpdateMangaThread.UpdateNamesAndLinks;
var
  i: Cardinal;
begin
  if names.Count = 0 then exit;
    for i:= 0 to names.Count - 1 do
    begin
      manager.names.Add(names.Strings[i]);
      manager.links.Add(links.Strings[i]);
    end;
end;

procedure   TUpdateMangaThread.Execute;
begin
  while NOT Terminated do
  begin
    while isSuspended do Sleep(16);
    if isCheckNameAndLink then
      if Info.GetNameAndLink(names, links,
                             manager.website, IntToStr(workPtr)) = INFORMATION_NOT_FOUND then
        manager.isDoneCheckNameAndLink:= TRUE
      else
        Synchronize(UpdateNamesAndLinks);
    Synchronize(DecThreadCount);
    Terminate;
  end;
end;

// ----- TUpdateMangaManagerThread -----

constructor TUpdateMangaManagerThread.Create;
begin
  websites   := TStringList.Create;
  isSuspended:= TRUE;
  dataProcess:= TDataProcess.Create;
  names  := TStringList.Create;
  links  := TStringList.Create;
  isTerminated:= FALSE;
  FreeOnTerminate:= TRUE;
  isDoneCheckNameAndLink:= FALSE;
  isDoneUpdateNecessary := FALSE;
  inherited Create(FALSE);
end;

destructor  TUpdateMangaManagerThread.Destroy;
begin
  websites.Free;
  names.Free;
  links.Free;
  SetLength(threads, 0);
  isTerminated:= TRUE;
  inherited Destroy;
end;

procedure   TUpdateMangaManagerThread.ShowResult;
begin
  if names.Count > MainForm.dataProcess.Data.Count then
    MessageDlg('', IntToStr(names.Count - MainForm.dataProcess.Data.Count) + ' new manga',
                  mtInformation, [mbYes], 0)
  else
    MessageDlg('', 'Nothing new.',
                  mtInformation, [mbYes], 0);
  Terminate;
end;

procedure   TUpdateMangaManagerThread.Execute;
var
  i, j, workPtr: Cardinal;
begin
  while NOT Terminated do
  begin
    while isSuspended do Sleep(16);
    if websites.Count = 0 then
    begin
     // Synchronize(ShowResult);
      Terminate;
    end;
    SetLength(threads, numberOfThreads);

    for i:= 0 to websites.Count-1 do
    begin
      website:= websites.Strings[i];
      dataProcess.LoadFromFile(website);
      isDoneCheckNameAndLink := FALSE;
      isDoneUpdateInformation:= FALSE;
      names.Clear;
      links.Clear;

      workPtr:= 0;
      while NOT isDoneCheckNameAndLink do
      begin
        if threadCount < numberOfThreads then
        begin
          for j:= 0 to numberOfThreads-1 do
            if (NOT Assigned(threads[j])) OR (threads[j].isTerminated) then
            begin
              Inc(threadCount);
              threads[j]:= TUpdateMangaThread.Create;
              threads[j].manager:= self;
              threads[j].isCheckNameAndLink:= TRUE;
              threads[j].workPtr:= workPtr;
              threads[j].isSuspended:= FALSE;
              Inc(workPtr);
              MainForm.sbMain.Panels[0].Text:= 'Updating list... ('+website+'.P.'+IntToStr(workPtr)+')';
            end;
        end;
        Sleep(100);
      end;
    end;

    while threadCount > 0 do Sleep(100);

    if (isDoneCheckNameAndLink) then
    begin
      MainForm.sbMain.Panels[0].Text:= '';
      Synchronize(ShowResult);
    end;

    if (isDoneUpdateNecessary) then
    begin
      Terminate;
    end;
  end;
  Terminate;
end;

end.

