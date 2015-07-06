{
        File: uUpdateThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uUpdateThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, typinfo, syncobjs, uData, LazFileUtils,
  uBaseUnit, uFMDThread, uTranslation;

type
  TUpdateMangaManagerThread = class;

  { TUpdateMangaThread }

  TUpdateMangaThread = class(TFMDThread)
  protected
    checkStyle: TCheckStyleType;
    names, links: TStringList;
    workPtr: Cardinal;
    manager: TUpdateMangaManagerThread;
    Info: TMangaInformation;

    procedure MainThreadUpdateNamesAndLinks;
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TUpdateMangaManagerThread }

  TUpdateMangaManagerThread = class(TFMDThread)
  private
    FStatus: String;
  protected
    procedure Execute; override;
    {$IFNDEF DOWNLOADER}
    procedure ConsoleReport;
    procedure SaveCurrentDatabase;
    {$ENDIF}
    procedure MainThreadShowGetting;
    procedure MainThreadEndGetting;
    procedure RefreshList;
    procedure DlgReport;
    procedure GetInfo(const limit: Cardinal; const cs: TCheckStyleType);
    procedure DoTerminate; override;
  public
    CS_AddInfoToData, CS_AddNamesAndLinks: TCriticalSection;
    isFinishSearchingForNewManga, isDownloadFromServer, isDoneUpdateNecessary: Boolean;
    mainDataProcess: TDBDataProcess;
    names, links, websites: TStringList;
    website, twebsite: String;
    workPtr, directoryCount,
    // for fakku's doujinshi only
    directoryCount2, numberOfThreads, websitePtr: Cardinal;
    threads: TFPList;
    CS_threads: TCriticalSection;
    constructor Create;
    destructor Destroy; override;
  end;
  
resourcestring
  RS_UpdatingList = 'Updating list';
  RS_GettingDirectory = 'Getting directory';
  RS_LookingForNewTitle = 'Looking for new title(s)';
  RS_LookingForNewTitleFromAnotherDirectory = 'Looking for new title(s) from another directory';
  RS_GettingInfo = 'Getting info';
  RS_GettingListFor = 'Getting list for';
  RS_Preparing = 'Preparing';
  RS_IndexingNewTitle = 'Indexing new title(s)';
  RS_RemovingDuplicateFromNewTitle = 'Removing duplicate from new title(s)';
  RS_RemovingDuplicateFromCurrentData = 'Removing duplicate from current data';
  RS_RemovingDuplicateFromLocalData = 'Removing duplicate from local data';
  RS_SynchronizingData = 'Synchronizing data';
  RS_SavingData = 'Saving data';
  RS_DlgHasNewManga = '%s has %d new manga(s)';

implementation

uses
  frmMain, Dialogs, ComCtrls, Forms, Controls, USimpleLogger;

// ----- TUpdateMangaThread -----

constructor TUpdateMangaThread.Create;
begin
  inherited Create(True);
  names := TStringList.Create;
  links := TStringList.Create;
  Info := TMangaInformation.Create(Self);
  info.isGetByUpdater := True;
end;

destructor TUpdateMangaThread.Destroy;
begin
  links.Free;
  names.Free;
  Info.Free;
  inherited Destroy;
end;

procedure TUpdateMangaThread.MainThreadUpdateNamesAndLinks;
begin
  if names.Count = 0 then
    Exit;
  manager.links.AddStrings(links);
  manager.names.AddStrings(names);
end;

procedure TUpdateMangaThread.Execute;
var
  iPos: Integer;
begin
  try
    case CheckStyle of
      CS_DIRECTORY_COUNT:
      begin
        if SitesMemberOf(manager.website,
          [BATOTO_ID, FAKKU_ID, MANGAEDEN_ID, PERVEDEN_ID]) then
        begin
          BATOTO_BROWSER := BATOTO_BROWSER_1;
          FAKKU_BROWSER := FAKKU_BROWSER_1;
          MANGAEDEN_BROWSER := MANGAEDEN_BROWSER_1;
          PERVEDEN_BROWSER := PERVEDEN_BROWSER_1;
          info.GetDirectoryPage(manager.directoryCount, manager.website);

          BATOTO_BROWSER := BATOTO_BROWSER_2;
          FAKKU_BROWSER := FAKKU_BROWSER_2;
          MANGAEDEN_BROWSER := MANGAEDEN_BROWSER_2;
          PERVEDEN_BROWSER := PERVEDEN_BROWSER_2;
          info.GetDirectoryPage(manager.directoryCount2, manager.website);
        end
        else
          info.GetDirectoryPage(manager.directoryCount, manager.website);
      end;

      //get names and links
      CS_DIRECTORY_PAGE, CS_DIRECTORY_PAGE_2:
      begin
        if BROWSER_INVERT then
        begin
          if checkStyle = CS_DIRECTORY_PAGE then
            workPtr := manager.directoryCount - workPtr - 1
          else
          if checkStyle = CS_DIRECTORY_PAGE_2 then
            workPtr := manager.directoryCount2 - workPtr - 1;
        end;
        if SitesMemberOf(manager.website,
          [BATOTO_ID, FAKKU_ID, MANGAEDEN_ID, PERVEDEN_ID]) then
        begin
          if checkStyle = CS_DIRECTORY_PAGE then
          begin
            BATOTO_BROWSER := BATOTO_BROWSER_1;
            FAKKU_BROWSER := FAKKU_BROWSER_1;
            MANGAEDEN_BROWSER := MANGAEDEN_BROWSER_1;
            PERVEDEN_BROWSER := PERVEDEN_BROWSER_1;
            Info.GetNameAndLink(names, links, manager.website, IntToStr(workPtr));
          end
          else
          if checkStyle = CS_DIRECTORY_PAGE_2 then
          begin
            BATOTO_BROWSER := BATOTO_BROWSER_2;
            FAKKU_BROWSER := FAKKU_BROWSER_2;
            MANGAEDEN_BROWSER := MANGAEDEN_BROWSER_2;
            PERVEDEN_BROWSER := PERVEDEN_BROWSER_2;
            Info.GetNameAndLink(names, links, manager.website, IntToStr(workPtr));
          end;
        end
        else
        begin
          Info.GetNameAndLink(names, links, manager.website, IntToStr(workPtr));
        end;
        //Synchronize(MainThreadUpdateNamesAndLinks);
        // For Fakku and Pururin only, reduce the number of page we have to visit
        // in order to search for new series.
        if SitesWithSortedList(manager.website) then
        begin
          if links.Count > 0 then
            if manager.mainDataProcess.Locate(DATA_PARAM_LINK, links.Strings[0]) then
              manager.isFinishSearchingForNewManga := True;
        end;

        //**removing hostname in links
        RemoveHostFromURLs(links);

        if links.Count > 0 then
        begin
          manager.CS_AddNamesAndLinks.Acquire;
          try
            manager.links.AddStrings(links);
            manager.names.AddStrings(names);
          finally
            manager.CS_AddNamesAndLinks.Release;
          end;
        end;
      end;

      CS_INFO:
      begin
        Info.mangaInfo.title := manager.names[workPtr];
        Info.GetInfoFromURL(manager.website, manager.links[workPtr],
          MainForm.DLManager.retryConnect);
        if not Terminated then
        begin
          manager.CS_AddInfoToData.Acquire;
          try
            Info.AddInfoToData(manager.names[workPtr], manager.links[workPtr],
              manager.mainDataProcess);
          finally
            manager.CS_AddInfoToData.Release;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      E.Message := E.Message + LineEnding + LineEnding +
        '  Website : ' + manager.website + LineEnding +
        '  CS      : ' + GetEnumName(TypeInfo(TCheckStyleType), Integer(checkStyle)) + LineEnding;
      if checkStyle = CS_INFO then
      begin
        E.Message := E.Message +
        '  Title   : ' + manager.names[workPtr] + LineEnding +
        '  URL     : ' + manager.links[workPtr] + LineEnding;
      end;
      MainForm.ExceptionHandler(Self, E);
    end;
  end;
end;

procedure TUpdateMangaThread.DoTerminate;
begin
  manager.CS_threads.Acquire;
  try
    manager.threads.Remove(Self);
  finally
    manager.CS_threads.Release;
  end;
  inherited DoTerminate;
end;

{ TUpdateMangaManagerThread }

procedure TUpdateMangaManagerThread.MainThreadShowGetting;
begin
  if MainForm.sbUpdateList.Visible = False then
  begin
    //statusbar reordering based on who's show up first?
    MainForm.sbUpdateList.Height := 30;
    MainForm.sbMain.Hide;
    MainForm.sbUpdateList.Show;
    MainForm.sbUpdateList.Panels[0].Style := psOwnerDraw;
    MainForm.btAbortUpdateList.Show;
    MainForm.sbMain.Show;
  end;
  MainForm.sbMain.SizeGrip := not MainForm.sbUpdateList.Visible;
  MainForm.sbUpdateList.Panels[0].Text := FStatus;
end;

procedure TUpdateMangaManagerThread.MainThreadEndGetting;
begin
  MainForm.sbUpdateList.Panels[0].Text := '';
  mainForm.sbUpdateList.Panels[0].Style := psText;
  MainForm.sbUpdateList.Hide;
  MainForm.sbMain.SizeGrip := not MainForm.sbUpdateList.Visible;
end;

constructor TUpdateMangaManagerThread.Create;
begin
  inherited Create(True);
  CS_threads := TCriticalSection.Create;
  CS_AddInfoToData := TCriticalSection.Create;
  CS_AddNamesAndLinks := TCriticalSection.Create;
  FreeOnTerminate := True;

  websites := TStringList.Create;
  names := TStringList.Create;
  links := TStringList.Create;

  mainDataProcess := TDBDataProcess.Create;

  threads := TFPList.Create;
end;

destructor TUpdateMangaManagerThread.Destroy;
begin
  websites.Free;
  names.Free;
  links.Free;
  mainDataProcess.Free;
  threads.Free;
  MainForm.isUpdating := False;
  CS_AddInfoToData.Free;
  CS_AddNamesAndLinks.Free;
  CS_threads.Free;
  inherited Destroy;
end;

procedure TUpdateMangaManagerThread.RefreshList;
begin
  try
    with MainForm do
    begin
      if cbSelectManga.Items[cbSelectManga.ItemIndex] = website then
      begin
        Screen.Cursor := crHourGlass;
        edSearch.Clear;
        vtMangaList.Clear;
        if dataProcess = nil then
          dataProcess := TDBDataProcess.Create
        else
          dataProcess.Close;
        OverwriteDBDataProcess(website, twebsite);
        dataProcess.Open(website);
        vtMangaList.RootNodeCount := dataProcess.DataCount;
        lbMode.Caption := Format(RS_ModeAll, [dataProcess.DataCount]);
        Screen.Cursor := crDefault;
      end
      else
        OverwriteDBDataProcess(website, twebsite);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TUpdateMangaManagerThread.DlgReport;
begin
  MessageDlg('', Format(RS_DlgHasNewManga, [website, links.Count]),
    mtInformation, [mbYes], 0);
end;

procedure TUpdateMangaManagerThread.GetInfo(const limit: Cardinal;
  const cs: TCheckStyleType);

  procedure WaitForThreads;
  begin
    while threads.Count > 0 do
      Sleep(200);
  end;

  procedure TerminateThreads;
  var
    i: Cardinal;
  begin
    if threads.Count > 0 then
      for i := threads.Count - 1 downto 0 do
        TUpdateMangaThread(threads[i]).Terminate;
    WaitForThreads;
  end;

var
  mt: Integer;
  s: String;
begin
  MainForm.ulTotalPtr := limit;
  try
    while workPtr < limit do
    begin
      MainForm.ulWorkPtr := workPtr + 1;
      if Terminated then
      begin
        TerminateThreads;
        Break;
      end;

      mt := INIAdvanced.ReadInteger('UpdateListNumberOfThreads', website, -1);
      if mt > 0 then
      begin
        if mt > 32 then //32 is max | be carefull, there's still memory leak problems
          mt := 32;
        numberOfThreads := mt;
      end
      else
      begin
        mt := MainForm.options.ReadInteger('connections', 'NumberOfThreadsPerTask', 1);
        case GetMangaSiteID(website) of
          EATMANGA_ID   : numberOfThreads := 1;
          SCANMANGA_ID  : numberOfThreads := 1;
          EHENTAI_ID    : numberOfThreads := 3;
        else
          numberOfThreads := mt;
        end;
        if numberOfThreads > mt then
          numberOfThreads := mt;
      end;
      if numberOfThreads < 1 then
        numberOfThreads := 1;  //default

      // Finish searching for new series
      if ((cs = CS_DIRECTORY_PAGE) or (cs = CS_DIRECTORY_PAGE_2)) and
        (isFinishSearchingForNewManga) then
      begin
        WaitForThreads;
        workPtr := limit;
        Break;
      end;

      while threads.Count >= numberOfThreads do
      begin
        if Terminated then
        begin
          TerminateThreads;
          Break;
        end;
        Sleep(200);   //waiting for empty slot / slowing down the circle
      end;

      if Terminated then
      begin
        TerminateThreads;
        Break;
      end;
      if threads.Count < numberOfThreads then
      begin
        CS_threads.Acquire;
        try
          threads.Add(TUpdateMangaThread.Create);
          TUpdateMangaThread(threads.Last).checkStyle := cs;
          TUpdateMangaThread(threads.Last).manager := Self;
          TUpdateMangaThread(threads.Last).workPtr := workPtr;
          TUpdateMangaThread(threads.Last).Start;
          Inc(workPtr);
          s := RS_UpdatingList + Format(' [%d/%d] %s | [T:%d] [%d/%d]',
            [websitePtr, websites.Count, website, threads.Count, workPtr, limit]);
          if cs = CS_DIRECTORY_COUNT then
            if limit = 1 then
              s := RS_UpdatingList + Format(' [%d/%d] ', [websitePtr, websites.Count]) +
                website + ' | ' + RS_GettingDirectory + '...'
            else
              s := s + ' | ' + RS_GettingDirectory + '...';
          if cs = CS_DIRECTORY_PAGE then
            s := s + ' | ' + RS_LookingForNewTitle + '...';
          if cs = CS_DIRECTORY_PAGE_2 then
            s := s + ' | ' + RS_LookingForNewTitleFromAnotherDirectory + '...';
          if cs = CS_INFO then
            s := s + ' | ' + RS_GettingInfo + ' "' + names.Strings[workPtr - 1] +
              '" "' + WebsiteRoots[GetMangaSiteID(website), 1] +
              links.Strings[workPtr - 1] + '"';
          FStatus := s;
          Synchronize(MainThreadShowGetting);
        finally
          CS_threads.Release;
        end;
      end;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TUpdateMangaManagerThread.DoTerminate;
begin
  Synchronize(MainThreadEndGetting);
  while threads.Count > 0 do
    Sleep(200);
  inherited DoTerminate;
end;

procedure TUpdateMangaManagerThread.Execute;

  procedure WaitForThreads;
  var
    i: Cardinal;
  begin
    while threads.Count > 0 do
    begin
      if Terminated then
        for i := threads.Count - 1 downto 0 do
          TUpdateMangaThread(threads[i]).Terminate;
      Sleep(200);
    end;
  end;

var
  s: String;
  j, k, iPos: Integer;
  del, purg: Boolean;
begin
  if websites.Count = 0 then
    Exit;
  try
    websitePtr := 0;
    if isDownloadFromServer then
    begin
      while websitePtr < websites.Count do
      begin
        website := websites.Strings[websitePtr];
        Inc(websitePtr);
        FStatus := RS_GettingListFor + ' ' + website + ' ...';
        Synchronize(MainThreadShowGetting);
        RunExternalProcess(fmdDirectory + 'updater.exe', ['-x', '-r' , '3', '-d',
          GetMangaDatabaseURL(website), '--lang', uTranslation.LastSelected]);
        Synchronize(RefreshList);
      end;
    end
    else
      while websitePtr < websites.Count do
      begin
        website := websites.Strings[websitePtr];
        Inc(websitePtr);
        FStatus := RS_UpdatingList + Format(' [%d/%d] %s',
          [websitePtr, websites.Count, website]) + ' | ' + RS_Preparing + '...';
        Synchronize(MainThreadShowGetting);

        if MainForm.cbSelectManga.Text = website then
        begin
          MainForm.vtMangaList.Clear;
          MainForm.dataProcess.Close;
        end;

        twebsite := '__' + website;
        CopyDBDataProcess(website, twebsite);

        if not mainDataProcess.Open(twebsite) then
        begin
          mainDataProcess.CreateDatabase(twebsite);
          mainDataProcess.OpenTable;
        end;

        //if MainForm.cbSelectManga.Text = website then
        //begin
        //  MainForm.dataProcess.Open;
        //  MainForm.vtMangaList.RootNodeCount := MainForm.dataProcess.DataCount;
        //end;
        //
        names.Clear;
        links.Clear;

        //get directory page count
        INIAdvanced.Reload;
        directoryCount := 0;
        directoryCount2 := 0;
        workPtr := 0;
        GetInfo(1, CS_DIRECTORY_COUNT);
        WaitForThreads;
        if Terminated then
          Break;

        //get names and links
        INIAdvanced.Reload;
        workPtr := 0;
        isFinishSearchingForNewManga := False;
        if SitesMemberOf(website, [BATOTO_ID, FAKKU_ID, MANGAEDEN_ID,
          PERVEDEN_ID]) then
        begin
          if directoryCount = 0 then
            directoryCount := 1;
          GetInfo(directoryCount, CS_DIRECTORY_PAGE);
          workPtr := 0;
          isFinishSearchingForNewManga := False;
          if directoryCount2 = 0 then
            directoryCount2 := 1;
          GetInfo(directoryCount2, CS_DIRECTORY_PAGE_2);
        end
        else
          GetInfo(directoryCount, CS_DIRECTORY_PAGE);
        WaitForThreads;
        if Terminated then
          Break;

        FStatus := RS_UpdatingList + Format(' [%d/%d] %s',
          [websitePtr, websites.Count, website]) + ' | ' + RS_IndexingNewTitle + '...';
        Synchronize(MainThreadShowGetting);

        // remove duplicate
        if links.Count > 0 then
        begin
          FStatus := RS_UpdatingList + Format(' [%d/%d] %s',
            [websitePtr, websites.Count, website]) + ' | ' + RS_RemovingDuplicateFromNewTitle + '...';
          Synchronize(MainThreadShowGetting);
          j := 0;
          while j < (links.Count - 1) do
          begin
            if Terminated then
              Break;
            del := False;
            if (j + 1) < links.Count then
              for k := j + 1 to links.Count - 1 do
              begin
                if Terminated then
                  Break;
                if SameText(links[j], links[k]) then
                begin
                  links.Delete(j);
                  names.Delete(j);
                  del := True;
                  Break;
                end;
              end;
            if not del then
              Inc(j);
          end;
        end;

        // remove duplicate found<>current database
        if links.Count > 0 then
        begin
          FStatus := RS_UpdatingList + Format(' [%d/%d] %s',
            [websitePtr, websites.Count, website]) + ' | ' + RS_RemovingDuplicateFromCurrentData + '...';
          Synchronize(MainThreadShowGetting);
          j := 0;
          while j < links.Count do
          begin
            if Terminated then
              Break;
            if mainDataProcess.Locate(DATA_PARAM_LINK, links[j]) then
            begin
              links.Delete(j);
              names.Delete(j);
            end
            else
              Inc(j);
          end;
        end;

        //get manga info
        if links.Count > 0 then
        begin
          if (SitesWithoutInformation(website)) or
            OptionUpdateListNoMangaInfo then
          begin
            for k := 0 to links.Count - 1 do
            begin
              mainDataProcess.AddData(
                names[k],
                links[k],
                '',
                '',
                '',
                '',
                '',
                0,
                Now
                );
            end;
          end
          else
          begin
            workPtr := 0;
            GetInfo(links.Count, CS_INFO);
          end;
          WaitForThreads;
          names.Clear;
          links.Clear;
        end;

        if (not Terminated) or (not SitesWithSortedList(website)) then
        begin
          FStatus := RS_UpdatingList + Format(' [%d/%d] %s',
            [websitePtr, websites.Count, website]) + ' | ' + RS_SavingData + '...';
          Synchronize(MainThreadShowGetting);
          { TODO -ocholif : Sort after update }
          mainDataProcess.Sort;
          mainDataProcess.Close;
        end;
        Synchronize(RefreshList);
        if Terminated then
          Break;
        websites[websitePtr - 1] :=
          UTF8Encode(#$2714 + WideString(websites[websitePtr - 1]));
      end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

end.
