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
    checkStyle        : Cardinal;
    names,
    links             : TStringList;
    isTerminated,
    isSuspended       : Boolean;
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
    procedure   DlgReport;
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
    case CheckStyle of
      CS_PAGE:
        begin
          if Info.GetNameAndLink(names, links,
                                 manager.website, IntToStr(workPtr)) = INFORMATION_NOT_FOUND then
            manager.isDoneCheckNameAndLink:= TRUE
          else
            Synchronize(UpdateNamesAndLinks);
        end;
      CS_INFO:
        begin
          Info.GetInfoFromURL(manager.website, manager.links[workPtr]);
          Info.AddInfoToData(manager.names[workPtr], manager.links[workPtr], MainForm.dataProcess);
        end;
    end;
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

procedure   TUpdateMangaManagerThread.DlgReport;
begin
  MessageDlg('', Format(stDlgNewManga, [links.Count]),
                 mtInformation, [mbYes], 0)
end;

procedure   TUpdateMangaManagerThread.Execute;
var
  i, j, k, workPtr: Cardinal;
begin
  while NOT Terminated do
  begin
    while isSuspended do Sleep(16);
    if websites.Count = 0 then
      Terminate;
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
              threads[j].checkStyle:= CS_PAGE;
              threads[j].manager:= self;
              threads[j].workPtr:= workPtr;
              threads[j].isSuspended:= FALSE;
              Inc(workPtr);
              MainForm.sbMain.Panels[0].Text:= 'Updating list... ('+website+'.P.'+IntToStr(workPtr)+')';
            end;
        end;
        Sleep(100);
      end;
      while threadCount > 0 do Sleep(100);

      j:= 0;
      repeat
        if Find(links.Strings[j], MainForm.dataProcess.Link, Integer(workPtr)) then
        begin
          links.Delete(j);
          names.Delete(j);
        end
        else
          Inc(j);
      until j = links.Count;
      if links.Count = 0 then
      begin
        Synchronize(DlgReport);
        Terminate;
      end;
      workPtr:= 0;
      while workPtr < links.Count do
      begin
        if (threadCount < numberOfThreads) then
        begin
          for j:= 0 to numberOfThreads-1 do
            if (NOT Assigned(threads[j])) OR (threads[j].isTerminated) then
            begin
              Inc(threadCount);
              threads[j]:= TUpdateMangaThread.Create;
              threads[j].checkStyle:= CS_INFO;
              threads[j].manager:= self;
              threads[j].workPtr:= workPtr;
              threads[j].isSuspended:= FALSE;
              Inc(workPtr);
              MainForm.sbMain.Panels[0].Text:= 'Updating list... ('+website+'.I.'+IntToStr(workPtr)+')';
            end;
        end;
        Sleep(100);
      end;
      while threadCount > 0 do Sleep(100);
    end;
    Synchronize(DlgReport);
    MainForm.dataProcess.RemoveFilter;
    MainForm.vtMangaList.Clear;
    MainForm.vtMangaList.RootNodeCount:= MainForm.dataProcess.filterPos.Count;
    MainForm.lbMode.Caption:= Format(stModeAll, [MainForm.dataProcess.filterPos.Count]);
    MainForm.sbMain.Panels[0].Text:= '';
    Terminate;
  end;
  Terminate;
end;

end.

