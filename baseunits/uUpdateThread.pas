{
        File: uUpdateThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uUpdateThread;

{$mode delphi}
{$DEFINE DOWNLOADER}

interface

uses
  Classes, SysUtils, fgl, typinfo, FileUtil, syncobjs, uData, uBaseUnit,
  uFMDThread, blcksock;

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

    procedure SockOnHeartBeat(Sender: TObject);
    procedure MainThreadUpdateNamesAndLinks;
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TUpdateMangaThreadList = TFPGList<TUpdateMangaThread>;

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
    mainDataProcess, syncProcess: TDataProcess;
    names, links, websites, dataLinks: TStringList;
    S, website: String;
    workPtr, directoryCount,
    // for fakku's doujinshi only
    directoryCount2, numberOfThreads, websitePtr: Cardinal;
    threads: TUpdateMangaThreadList;
    CS_threads: TCriticalSection;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF DOWNLOADER}
  frmMain,
  {$ELSE}
  mainunit,
  {$ENDIF}
  Dialogs, ComCtrls, Forms, Controls, RegExpr;

// ----- TUpdateMangaThread -----

constructor TUpdateMangaThread.Create;
begin
  inherited Create(True);
  names := TStringList.Create;
  links := TStringList.Create;
  Info := TMangaInformation.Create;
  info.isGetByUpdater := True;
  info.FOwner := Self;
  info.FHTTP.Sock.OnHeartbeat := SockOnHeartBeat;
  info.FHTTP.Sock.HeartbeatRate := SOCKHEARTBEATRATE;
end;

destructor TUpdateMangaThread.Destroy;
begin
  links.Free;
  names.Free;
  Info.Free;
  inherited Destroy;
end;

procedure TUpdateMangaThread.SockOnHeartBeat(Sender: TObject);
begin
  if Terminated then
  begin
    TBlockSocket(Sender).Tag := 1;
    TBlockSocket(Sender).StopFlag := True;
    TBlockSocket(Sender).AbortSocket;
  end;
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
  regx: TRegExpr;
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
        {$IFDEF DOWNLOADER}
        if SitesWithSortedList(manager.website) then
        begin
          if links.Count > 0 then
            if manager.dataLinks.Find(links.Strings[0], iPos) then
              manager.isFinishSearchingForNewManga := True;
        end;
        {$ENDIF}

        //**removing hostname in links
        if links.Count > 0 then
        begin
          regx := TRegExpr.Create;
          try
            regx.Expression := '(https?\://)?[^/]*\w+\.\w+(\:\d+)?/';
            for iPos := 0 to links.count - 1 do
              if regx.Exec(links[iPos]) then
                links[iPos] := regx.Replace(links[iPos], '/', False);
          finally
            regx.Free;
          end;
        end;

        manager.CS_AddNamesAndLinks.Acquire;
        try
          if links.Count > 0 then
          begin
            manager.links.AddStrings(links);
            manager.names.AddStrings(names);
          end;
        finally
          manager.CS_AddNamesAndLinks.Release;
        end;
      end;

      CS_INFO:
      begin
        {$IFDEF DOWNLOADER}
        Info.GetInfoFromURL(manager.website, manager.links[workPtr], 5);
        {$ELSE}
        Info.GetInfoFromURL(manager.website, manager.links[workPtr], 0);
        {$ENDIF}
        if not Terminated then
        begin
          manager.CS_AddInfoToData.Acquire;
          try
            // {$IFNDEF DOWNLOADER}
            Info.AddInfoToDataWithoutBreak(manager.names[workPtr],
              manager.links[workPtr], manager.mainDataProcess);
            // {$ELSE}
            //   Info.AddInfoToData(manager.names[workPtr], manager.links[workPtr], manager.mainDataProcess);
            // {$ENDIF}
          finally
            manager.CS_AddInfoToData.Release;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      E.Message := E.Message + LineEnding +
        '  CS      : ' + GetEnumName(TypeInfo(TCheckStyleType), Integer(checkStyle)) + LineEnding +
        '  Title   : ' + manager.names[workPtr] + LineEnding +
        '  URL     : ' + manager.links[workPtr] + LineEnding +
        '  Website : ' + manager.website;
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

// ----- TUpdateMangaManagerThread -----

procedure TUpdateMangaManagerThread.MainThreadShowGetting;
begin
  {$IFDEF DOWNLOADER}
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
  {$ENDIF}
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
  dataLinks := TStringList.Create;

  mainDataProcess := TDataProcess.Create;
  syncProcess := TDataProcess.Create;

  threads := TUpdateMangaThreadList.Create;
end;

destructor TUpdateMangaManagerThread.Destroy;
begin
  websites.Free;
  names.Free;
  links.Free;
  dataLinks.Free;
  mainDataProcess.Free;
  syncProcess.Free;
  threads.Free;
  {$IFDEF DOWNLOADER}
  MainForm.isUpdating := False;
  {$ENDIF}
  CS_AddInfoToData.Free;
  CS_AddNamesAndLinks.Free;
  CS_threads.Free;
  inherited Destroy;
end;

{$IFNDEF DOWNLOADER}
procedure TUpdateMangaManagerThread.ConsoleReport;
begin
  MainForm.Memo1.Lines.Add(S);
end;

procedure TUpdateMangaManagerThread.SaveCurrentDatabase;
begin
  mainDataProcess.SaveToFile(website);
  MainForm.Memo1.Lines.Clear;
end;

{$ENDIF}

procedure TUpdateMangaManagerThread.RefreshList;
begin
  {$IFDEF DOWNLOADER}
  try
    if MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex] = website then
    begin
      Screen.Cursor := crHourGlass;
      MainForm.edSearch.Clear;
      MainForm.dataProcess.RemoveFilter;
      MainForm.dataProcess.Free;
      MainForm.dataProcess := TDataProcess.Create;
      MainForm.dataProcess.LoadFromFile(website);
      MainForm.vtMangaList.Clear;
      MainForm.vtMangaList.RootNodeCount := MainForm.dataProcess.filterPos.Count;
      MainForm.lbMode.Caption :=
        Format(stModeAll, [MainForm.dataProcess.filterPos.Count]);
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
  {$ENDIF}
end;

procedure TUpdateMangaManagerThread.DlgReport;
begin
  MessageDlg('', Format(stDlgNewManga, [website, links.Count]),
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
        threads[i].Terminate;
    WaitForThreads;
  end;

var
  mt: Integer;

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

      INIAdvanced.Reload;
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
          //FAKKU_ID      : numberOfThreads := 3;
          //PURURIN_ID    : numberOfThreads := 3;
          else
            numberOfThreads := mt;
        end;
        if numberOfThreads > mt then
          numberOfThreads := mt;
      end;
      if numberOfThreads < 1 then
        numberOfThreads := 1;  //default

      // Finish searching for new series
      {$IFDEF DOWNLOADER}
      if ((cs = CS_DIRECTORY_PAGE) or (cs = CS_DIRECTORY_PAGE_2)) and
        (isFinishSearchingForNewManga) then
      begin
        WaitForThreads;
        workPtr := limit;
        Break;
      end;
      {$ENDIF}

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
          threads.Last.checkStyle := cs;
          threads.Last.manager := Self;
          threads.Last.workPtr := workPtr;
          threads.Last.Start;
          Inc(workPtr);
          {S := 'Updating list: ' + website + ' [T.' + IntToStr(threads.Count) +
            '|' + GetEnumName(TypeInfo(TCheckStyleType), integer(cs)) +
            '] ' + Format('[%d/%d]', [workPtr, limit]);}
          S := 'Updating list ' + Format('[%d/%d] %s | [T:%d] [%d/%d]',
            [websitePtr, websites.Count, website, threads.Count, workPtr, limit]);
          if cs = CS_DIRECTORY_COUNT then
            if limit = 1 then
              S := 'Updating list' + Format(' [%d/%d] ', [websitePtr, websites.Count]) +
                website + ' | Getting directory...'
            else
              S := S + ' | Getting directory...';
          if cs = CS_DIRECTORY_PAGE then
            S := S + ' | Looking for new title(s)...';
          if cs = CS_DIRECTORY_PAGE_2 then
            S := S + ' | Looking for new title(s) from another directory...';
          if cs = CS_INFO then
            S := S + ' | Getting info "' + names.Strings[workPtr - 1] +
              '" "' + WebsiteRoots[GetMangaSiteID(website), 1] +
              links.Strings[workPtr - 1] + '"';
          {$IFNDEF DOWNLOADER}
          Synchronize(ConsoleReport);
          if (workPtr mod 100 = 0) and (workPtr > 50) and (cs = CS_INFO) and
            (mainDataProcess.Data.Count > 5) then
            Synchronize(SaveCurrentDatabase);
          {$ELSE}
          FStatus := s;
          Synchronize(MainThreadShowGetting);
          {$ENDIF}
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
          threads[i].Terminate;
      Sleep(200);
    end;
  end;

var
  s: String;
  j, k: Cardinal;
  del: Boolean;
  iPos: Integer;
  purg: Boolean;
begin
  if websites.Count = 0 then
    Exit;
  try
    websitePtr := 0;
    {$IFDEF DOWNLOADER}
    if isDownloadFromServer then
    begin
      while websitePtr < websites.Count do
      begin
        website := websites.Strings[websitePtr];
        Inc(websitePtr);
        FStatus := 'Getting list for ' + website + ' ...';
        Synchronize(MainThreadShowGetting);
        fmdRunAsAdmin(fmdDirectory + 'updater.exe', '-d ' + GetMangaDatabaseURL(website) +
          ' -x -r 5 -q', True);
        Synchronize(RefreshList);
      end;
    end
    else
    {$ENDIF}
      while websitePtr < websites.Count do
      begin
        website := websites.Strings[websitePtr];
        Inc(websitePtr);
      {$IFDEF DOWNLOADER}
        //while not FileExists(DATA_FOLDER + website + DATA_EXT) do
        //begin
        //  FStatus := 'Getting list for ' + website + ' ...';
        //  Synchronize(MainThreadShowGetting);
        //  fmdRunAsAdmin('updater.exe', 'data ' + GetMangaDatabaseURL(website) + ' 5', True);
        //end;
      {$ENDIF}
        FStatus := 'Updating list ' + Format('[%d/%d] %s',
          [websitePtr, websites.Count, website]) + ' | Preparing...';
        Synchronize(MainThreadShowGetting);

        mainDataProcess.Clear;
        mainDataProcess.LoadFromFile(website);

        //Sort first for faster searching
        dataLinks.Assign(mainDataProcess.Link);
        dataLinks.Sort;

        // convert old data
        if (mainDataProcess.Link.Count > 0) and
          (website = WebsiteRoots[MANGAFOX_ID, 0]) then
        begin
          purg := False;
          s := WebsiteRoots[GetMangaSiteID(website), 1];
          if dataLinks.Find(s, iPos) then
            purg := True;
          if purg then
          begin
            for k := 0 to mainDataProcess.Link.Count - 1 do
            begin
              if Pos(s, mainDataProcess.Link[k]) > 0 then
              begin
                mainDataProcess.Link[k] :=
                  StringReplace(mainDataProcess.Link[k], s, '', [rfIgnoreCase]);

                mainDataProcess.Data[k] := RemoveStringBreaks(
                  mainDataProcess.Param[k, DATA_PARAM_NAME] + SEPERATOR +
                  mainDataProcess.Link[k] + SEPERATOR +
                  mainDataProcess.Param[k, DATA_PARAM_AUTHORS] + SEPERATOR +
                  mainDataProcess.Param[k, DATA_PARAM_ARTISTS] + SEPERATOR +
                  mainDataProcess.Param[k, DATA_PARAM_GENRES] + SEPERATOR +
                  mainDataProcess.Param[k, DATA_PARAM_STATUS] + SEPERATOR +
                  mainDataProcess.Param[k, DATA_PARAM_SUMMARY] + SEPERATOR +
                  mainDataProcess.Param[k, DATA_PARAM_NUMCHAPTER] + SEPERATOR +
                  mainDataProcess.Param[k, DATA_PARAM_JDN] + SEPERATOR +
                  mainDataProcess.Param[k, DATA_PARAM_READ] + SEPERATOR);
              end;
            end;
            mainDataProcess.SaveToFile(website);
            dataLinks.Assign(mainDataProcess.Link);
            dataLinks.Sort;
          end;
        end;

        names.Clear;
        links.Clear;

        //get directory page count
        directoryCount := 0;
        directoryCount2 := 0;
        workPtr := 0;
        GetInfo(1, CS_DIRECTORY_COUNT);
        WaitForThreads;
        if Terminated then
          Break;

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

        {$IFNDEF DOWNLOADER}
        names.SaveToFile(website + '_names.txt');
        links.SaveToFile(website + '_links.txt');

        names.Clear;
        links.Clear;

        names.LoadFromFile(website + '_names.txt');
        links.LoadFromFile(website + '_links.txt');
        {$ENDIF}

        FStatus := 'Updating list ' + Format('[%d/%d] %s',
          [websitePtr, websites.Count, website]) + ' | Indexing new title(s)...';
        Synchronize(MainThreadShowGetting);

        // remove duplicate
        if links.Count > 0 then
        begin
          FStatus := 'Updating list ' + Format('[%d/%d] %s',
            [websitePtr, websites.Count, website]) + ' | Removing duplicate from new title(s)...';
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
                if SameText(links.Strings[j], links.Strings[k]) then
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
        if (mainDataProcess.Link.Count > 0) and (links.Count > 0) then
        begin
          FStatus := 'Updating list ' + Format('[%d/%d] %s',
            [websitePtr, websites.Count, website]) + ' | Removing duplicate from current data...';
          Synchronize(MainThreadShowGetting);
          j := 0;
          while j < links.Count do
          begin
            if Terminated then
              Break;
            if dataLinks.Find(links[j], integer(workPtr)) then
            begin
              links.Delete(j);
              names.Delete(j);
            end
            else
              Inc(j);
          end;
        end;
        dataLinks.Clear; //not used anymore

        if OptionUpdateListRemoveDuplicateLocalData then
        begin
          FStatus := 'Updating list ' + Format('[%d/%d] %s',
            [websitePtr, websites.Count, website]) + ' | Removing duplicate local data...';
          Synchronize(MainThreadShowGetting);
          if mainDataProcess.Link.Count > 0 then
          begin
            j := 0;
            while j < (mainDataProcess.Link.Count - 1) do
            begin
              if Terminated then
                Break;
              del := False;
              if (j + 1) < mainDataProcess.Link.Count then
                for k := j + 1 to mainDataProcess.Link.Count - 1 do
                begin
                  if Terminated then
                    Break;

                  if SameText(mainDataProcess.Link.Strings[j],
                    mainDataProcess.Link.Strings[k]) then
                  begin
                    mainDataProcess.Link.Delete(j);
                    mainDataProcess.Title.Delete(j);
                    mainDataProcess.Data.Delete(j);
                    del := True;
                    Break;
                  end;
                end;
              if not del then
                Inc(j);
            end;
          end;
        end;

        if links.Count > 0 then
        begin
          if (SitesWithoutInformation(website)) or
            OptionUpdateListNoMangaInfo then
          begin
            mainDataProcess.Title.AddStrings(names);
            mainDataProcess.Link.AddStrings(links);
            for k := 0 to links.Count - 1 do
            begin
            {$IFDEF DOWNLOADER}
              mainDataProcess.Data.Add(
                RemoveStringBreaks(
                SetParams(
                [names.Strings[k],
                links.Strings[k],
                '',
                '',
                '',
                '',
                '',
                '0',
                IntToStr(GetCurrentJDN),
                '0'])));
            {$ELSE}
              mainDataProcess.Data.Add(
                RemoveStringBreaks(
                SetParams(
                [names.Strings[k],
                links.Strings[k],
                '',
                '',
                '',
                '',
                '',
                '0',
                '0',
                '0'])));
            {$ENDIF}
            end;
          end
          else
          begin
            workPtr := 0;
            GetInfo(links.Count, CS_INFO);
          end;
          WaitForThreads;

          // sync data based on existing sites
          if (mainDataProcess.Data.Count > 0) and
            (SitesWithoutInformation(website)) and
            (FileExistsUTF8(DATA_FOLDER + WebsiteRoots[BATOTO_ID, 0] + DATA_EXT) or
             FileExistsUTF8(DATA_FOLDER + WebsiteRoots[ANIMEA_ID, 0] + DATA_EXT) or
             FileExistsUTF8(DATA_FOLDER + WebsiteRoots[MANGAGO_ID, 0] + DATA_EXT) or
             FileExistsUTF8(DATA_FOLDER + WebsiteRoots[MANGAPARK_ID, 0] + DATA_EXT))
            then
          begin
            FStatus := 'Updating list ' + Format('[%d/%d] %s',
              [websitePtr, websites.Count, website]) + ' | Synchronizing data...';
            Synchronize(MainThreadShowGetting);

            syncProcess.Clear;
            if FileExistsUTF8(DATA_FOLDER + WebsiteRoots[GetMangaSiteID(website), 0] + DATA_EXT) then
              syncProcess.LoadFromFile(website);

            //remove existed data
            if syncProcess.Data.Count > 0 then
            begin
              j := 0;
              while j < mainDataProcess.Link.Count do
              begin
                if Terminated then
                  Break;
                del := False;
                for k := 0 to syncProcess.Link.Count - 1 do
                begin
                  if Terminated then
                    Break;
                  if SameText(mainDataProcess.Link[j], syncProcess.Link[k]) then
                  begin
                    mainDataProcess.Title.Delete(j);
                    mainDataProcess.Link.Delete(j);
                    mainDataProcess.Data.Delete(j);
                    del := True;
                    Break;
                  end;
                end;
                if not del then
                  Inc(j);
              end;
            end;

            syncProcess.Clear;
            if mainDataProcess.Link.Count > 0 then
            begin
              if FileExistsUTF8(DATA_FOLDER + WebsiteRoots[BATOTO_ID, 0] + DATA_EXT) then
                syncProcess.LoadFromFile(WebsiteRoots[BATOTO_ID, 0])
              else
              if FileExistsUTF8(DATA_FOLDER + WebsiteRoots[ANIMEA_ID, 0] + DATA_EXT) then
                syncProcess.LoadFromFile(WebsiteRoots[ANIMEA_ID, 0])
              else
              if FileExistsUTF8(DATA_FOLDER + WebsiteRoots[MANGAGO_ID, 0] + DATA_EXT) then
                syncProcess.LoadFromFile(WebsiteRoots[MANGAGO_ID, 0])
              else
              if FileExistsUTF8(DATA_FOLDER + WebsiteRoots[MANGAPARK_ID, 0] + DATA_EXT) then
                syncProcess.LoadFromFile(WebsiteRoots[MANGAPARK_ID, 0]);

              // brute force ...
              if syncProcess.Link.Count > 0 then
              begin
                for k := 0 to mainDataProcess.Data.Count - 1 do
                begin
                  if Terminated then
                    Break;
                  {
                  if odd(k) then
                  begin
                    FStatus := 'Updating list ' + Format('[%d/%d] %s | [%d/%d] | %s',
                      [websitePtr, websites.Count, website,
                      k, mainDataProcess.Data.Count
                      , 'Synchronizing data...']);
                    Synchronize(MainThreadShowGetting);
                  end;
                  }
                  for j := 0 to syncProcess.Link.Count - 1 do
                  begin
                    if Terminated then
                      Break;
                    //if SameText(mainDataProcess.Link[k], syncProcess.Link[j]) then
                    if SameText(mainDataProcess.Title[k], syncProcess.Title[j]) then
                    begin
                      //if (website = WebsiteRoots[MANGASTREAM_ID, 0]) or
                      //  (website = WebsiteRoots[S2SCAN_ID, 0]) then
                        s := syncProcess.Param[j, DATA_PARAM_SUMMARY];
                      //else
                      //  s := mainDataProcess.Param[k, DATA_PARAM_SUMMARY];
                      mainDataProcess.Data.Strings[k] := RemoveBreaks(
                        mainDataProcess.Param[k, DATA_PARAM_NAME] + SEPERATOR +
                        mainDataProcess.Param[k, DATA_PARAM_LINK] + SEPERATOR +
                        syncProcess.Param[j, DATA_PARAM_AUTHORS] + SEPERATOR +
                        syncProcess.Param[j, DATA_PARAM_ARTISTS] + SEPERATOR +
                        syncProcess.Param[j, DATA_PARAM_GENRES] + SEPERATOR +
                        mainDataProcess.Param[k, DATA_PARAM_STATUS] + SEPERATOR +
                        s + SEPERATOR +
                        mainDataProcess.Param[k, DATA_PARAM_NUMCHAPTER] + SEPERATOR +
                        mainDataProcess.Param[k, DATA_PARAM_JDN] + SEPERATOR +
                        mainDataProcess.Param[k, DATA_PARAM_READ] + SEPERATOR);
                      Break;
                    end;
                  end;
                end;
              end;
              syncProcess.Clear;
              // add back existing data
              if FileExistsUTF8(DATA_FOLDER + WebsiteRoots[GetMangaSiteID(website), 0] + DATA_EXT) then
              begin
                syncProcess.LoadFromFile(website);
                if syncProcess.Data.Count > 0 then
                  mainDataProcess.Data.AddStrings(syncProcess.Data);
                syncProcess.Clear;
              end;
            end;
          end;
        end;

        if (not Terminated) or (not SitesWithSortedList(website)) then
        begin
          FStatus := 'Updating list ' + Format('[%d/%d] %s',
            [websitePtr, websites.Count, website]) + ' | Saving data...';
          Synchronize(MainThreadShowGetting);
          mainDataProcess.SaveToFile(website);
        end;
        {$IFDEF DOWNLOADER}
        Synchronize(RefreshList);
        {$ENDIF}
        if Terminated then
          Break;
        websites[websitePtr - 1] :=
          UTF8Encode(#$2714 + WideString(websites[websitePtr - 1]));
      end;
    {$IFNDEF DOWNLOADER}
    S := 'Saving to ' + website + '.dat ...';
    Synchronize(ConsoleReport);
    S := 'Done.';
    Synchronize(ConsoleReport);
    {$ELSE}
    // Synchronize(DlgReport);
    //Synchronize(MainThreadEndGetting);
    {$ENDIF}
    // Synchronize(DlgReport);
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

end.
