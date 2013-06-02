{
        File: updatelist.pas
        License: GPLv2
        This unit is part of Free Manga Downloader
}

unit updatelist;

{$mode delphi}
{$DEFINE DOWNLOADER}
interface

uses
  Classes, SysUtils, data, baseunit;

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
    {$IFNDEF DOWNLOADER}
    procedure   ConsoleReport;
    procedure   SaveCurrentDatabase;
    {$ENDIF}
    procedure   RefreshList;
    procedure   DlgReport;
    procedure   getInfo(const limit, cs: Cardinal);
  public
    isTerminated,
    isSuspended,
    isDoneUpdateNecessary : Boolean;
    mainDataProcess,
    dataProcess           : TDataProcess;
    names,
    links,
    websites              : TStringList;
    S,
    website               : String;
    workPtr,
    directoryCount,
    // for fakku's doujinshi only
    directoryCount2,
    threadCount,
    numberOfThreads       : Cardinal;
    threads               : array of TUpdateMangaThread;
    constructor Create;
    destructor  Destroy; override;
  end;

implementation

uses
  mainunit, Dialogs;

// ----- TUpdateMangaThread -----

constructor TUpdateMangaThread.Create;
begin
  inherited Create(FALSE);
  names:= TStringList.Create;
  links:= TStringList.Create;
  isSuspended:= TRUE;
  Info:= TMangaInformation.Create;
  Info.isGetByUpdater:= TRUE;
  isTerminated   := FALSE;
  FreeOnTerminate:= TRUE;
end;

destructor  TUpdateMangaThread.Destroy;
begin
  Info.Free;
  links.Free;
  names.Free;
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
  if Terminated then exit;
  while isSuspended do Sleep(16);
  case CheckStyle of
    CS_DIRECTORY_COUNT:
      begin
        {$IFDEF DOWNLOADER}
        if manager.website = BATOTO_NAME then
          manager.directoryCount:= batotoLastDirectoryPage;
        {$ENDIF}
        if manager.website = FAKKU_NAME then
        begin
          FAKKU_BROWSER:= FAKKU_MANGA_BROWSER;
          info.GetDirectoryPage(manager.directoryCount , manager.website);
          FAKKU_BROWSER:= FAKKU_DOUJINSHI_BROWSER;
          info.GetDirectoryPage(manager.directoryCount2, manager.website);
        end
        else
          info.GetDirectoryPage(manager.directoryCount , manager.website);
        {$IFDEF DOWNLOADER}
        if manager.website = BATOTO_NAME then
          {MainForm.}batotoLastDirectoryPage:= manager.directoryCount;
        {$ENDIF}
      end;
    CS_DIRECTORY_PAGE:
      begin
        if manager.website = FAKKU_NAME then
        begin
          if Integer(workPtr-manager.directoryCount) >= 0 then
          begin
            FAKKU_BROWSER:= FAKKU_DOUJINSHI_BROWSER;
            Info.GetNameAndLink(names, links, manager.website, IntToStr(workPtr-manager.directoryCount));
          end
          else
          begin
            FAKKU_BROWSER:= FAKKU_MANGA_BROWSER;
            Info.GetNameAndLink(names, links, manager.website, IntToStr(workPtr));
          end;
        end
        else
        begin
          Info.GetNameAndLink(names, links, manager.website, IntToStr(workPtr));
        end;
        Synchronize(UpdateNamesAndLinks);
      end;
    CS_INFO:
      begin
        Info.GetInfoFromURL(manager.website, manager.links[workPtr], {$IFDEF DOWNLOADER}5{$ELSE}0{$ENDIF});
     // {$IFNDEF DOWNLOADER}
        Info.AddInfoToDataWithoutBreak(manager.names[workPtr], manager.links[workPtr], manager.mainDataProcess);
     // {$ELSE}
     //   Info.AddInfoToData(manager.names[workPtr], manager.links[workPtr], manager.mainDataProcess);
     // {$ENDIF}
      end;
  end;
  Synchronize(DecThreadCount);
end;

// ----- TUpdateMangaManagerThread -----

constructor TUpdateMangaManagerThread.Create;
begin
  inherited Create(FALSE);
  websites   := TStringList.Create;
  isSuspended:= TRUE;
  dataProcess:= TDataProcess.Create;
  names  := TStringList.Create;
  links  := TStringList.Create;
  isTerminated:= FALSE;
  FreeOnTerminate:= TRUE;
end;

destructor  TUpdateMangaManagerThread.Destroy;
begin
  websites.Free;
  names.Free;
  links.Free;
  SetLength(threads, 0);
  {$IFDEF DOWNLOADER}
  MainForm.isUpdating:= FALSE;
  {$ENDIF}
  isTerminated:= TRUE;
  inherited Destroy;
end;

{$IFNDEF DOWNLOADER}
procedure   TUpdateMangaManagerThread.ConsoleReport;
begin
  MainForm.Memo1.Lines.Add(S);
end;

procedure   TUpdateMangaManagerThread.SaveCurrentDatabase;
begin
  mainDataProcess.SaveToFile(website);
  MainForm.Memo1.Lines.Clear;
end;
{$ENDIF}

procedure   TUpdateMangaManagerThread.RefreshList;
begin
  {$IFDEF DOWNLOADER}
  if MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex] = website then
  begin
    MainForm.dataProcess.RemoveFilter;
    MainForm.dataProcess.Free;
    MainForm.dataProcess:= TDataProcess.Create;
    MainForm.dataProcess.LoadFromFile(website);
    MainForm.vtMangaList.Clear;
    MainForm.vtMangaList.RootNodeCount:= MainForm.dataProcess.filterPos.Count;
    MainForm.lbMode.Caption:= Format(stModeAll, [MainForm.dataProcess.filterPos.Count]);
    MainForm.sbMain.Panels[0].Text:= '';
  end;
  {$ENDIF}
end;

procedure   TUpdateMangaManagerThread.DlgReport;
begin
  MessageDlg('', Format(stDlgNewManga, [website, links.Count]),
                 mtInformation, [mbYes], 0);
end;

procedure   TUpdateMangaManagerThread.getInfo(const limit, cs: Cardinal);
var
  j: Cardinal;
begin
  while (workPtr < limit) do
  begin
    if (threadCount < numberOfThreads) then
      for j:= 0 to numberOfThreads-1 do
        if (NOT Assigned(threads[j])) OR (threads[j].isTerminated) then
        begin
          Inc(threadCount);
          threads[j]:= TUpdateMangaThread.Create;
          threads[j].checkStyle:= cs;
          threads[j].manager:= self;
          threads[j].workPtr:= workPtr;
          threads[j].isSuspended:= FALSE;
          Inc(workPtr);
          S:= 'Updating list: '+website+'[T.'+IntToStr(j)+'; CS.'+IntToStr(cs)+'] '+IntToStr(workPtr)+' / '+IntToStr(limit);
          if cs = CS_INFO then
            S:= S+' "'+links.Strings[workPtr-1]+'"';
        {$IFNDEF DOWNLOADER}
          Synchronize(ConsoleReport);
          if (workPtr mod 100 = 0) AND (workPtr > 50) AND (cs = CS_INFO) AND (mainDataProcess.Data.Count > 5) then
            Synchronize(SaveCurrentDatabase);
        {$ELSE}
          MainForm.sbMain.Panels[0].Text:= S;
        {$ENDIF}
          break;
        end;
    Sleep(100);
  end;
end;

procedure   TUpdateMangaManagerThread.Execute;
var
  i, j, k: Cardinal;
begin
 // while NOT Terminated do
  begin
    while isSuspended do Sleep(16);
    if websites.Count = 0 then
      Terminate;
    SetLength(threads, numberOfThreads);

    for i:= 0 to websites.Count-1 do
    begin
      website:= websites.Strings[i];
      if website = GEHENTAI_NAME then
        numberOfThreads:= 1;

      dataProcess.LoadFromFile(website);
      names.Clear;
      links.Clear;

      workPtr:= 0;
      getInfo(1, CS_DIRECTORY_COUNT);
      while threadCount > 0 do Sleep(100);

      workPtr:= 0;
      if website = FAKKU_NAME then
        getInfo(directoryCount+directoryCount2, CS_DIRECTORY_PAGE)
      else
        getInfo(directoryCount, CS_DIRECTORY_PAGE);
      while threadCount > 0 do Sleep(100);

      {$IFNDEF DOWNLOADER}
    {  names.SaveToFile(website+'_names.txt');
      links.SaveToFile(website+'_links.txt');
                }
      names.Clear;
      links.Clear;

      names.LoadFromFile(website+'_names.txt');
      links.LoadFromFile(website+'_links.txt');
      {$ENDIF}
      mainDataProcess:= TDataProcess.Create;
      mainDataProcess.LoadFromFile(website);

      //
      j:= 0;
      repeat
        if Find(links.Strings[j], mainDataProcess.Link, Integer(workPtr)) then
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
        continue;
      end;  //
      workPtr:= 0;//mainDataProcess.Data.Count;

      getInfo(links.Count, CS_INFO);
      Sleep(100);
      while threadCount > 0 do Sleep(100);
    end;
    mainDataProcess.SaveToFile(website);
  {$IFNDEF DOWNLOADER}
    S:= 'Saving to '+website+'.dat ...';
    Synchronize(ConsoleReport);
    S:= 'Done.';
    Synchronize(ConsoleReport);
  {$ELSE}
    Synchronize(RefreshList);
   // Synchronize(DlgReport);
    MainForm.sbMain.Panels[0].Text:= '';
  {$ENDIF}
    mainDataProcess.Free;
   // Synchronize(DlgReport);
  end;
end;

end.

