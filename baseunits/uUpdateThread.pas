{
        File: uUpdateThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uUpdateThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, typinfo, uData, LazFileUtils, uBaseUnit, uMisc,
  WebsiteModules, DBDataProcess, SimpleTranslator, FMDOptions, httpsendthread,
  BaseThread, MultiLog, ExtCtrls, Forms, Controls, Buttons, Graphics;

type
  TUpdateListManagerThread = class;

  { TUpdateListThread }

  TUpdateListThread = class(TBaseThread)
  protected
    Info: TMangaInformation;
    procedure Execute; override;
  public
    checkStyle: TCheckStyleType;
    manager: TUpdateListManagerThread;
    workPtr: Integer;
    title, link: String;
    constructor Create;
    destructor Destroy; override;
  end;

  { TUpdateListManagerThread }

  TUpdateListManagerThread = class(TBaseThread)
  private
    FCommitCount: Integer;
    FThreadAborted,
    FThreadEndNormally,
    FIsPreListAvailable: Boolean;
    FCurrentGetInfoLimit: Integer;
    FCS_CurrentGetInfoLimit: TRTLCriticalSection;

    FStatusBar: TPanel;
    FButtonCancel: TSpeedButton;

    FControlMargin,
    FProgressBarHeight: Integer;
    FResized: Boolean;
    FProgressBarRect,
    FProgressBarPercentsRect,
    FStatusTextRect: TRect;
    FStatusText: String;

    FWorkPtr,
    FTotalPtr: Integer;

    FTimerRepaint: TTimer;
    FNeedRepaint: Boolean;
  protected
    procedure SyncCreate;
    procedure SyncDestroy;
    procedure TimerRepaintTimer(Sender: TObject);
    procedure StatusBarPaint(Sender: TObject);
    procedure StatusBarRezise(Sender: TObject);
    procedure StatusBarShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure ButtonCancelClick(Sender: TObject);
    procedure UpdateStatusText(AStatusText: String);
    procedure SetCurrentDirectoryPageNumber(AValue: Integer);
    procedure MainThreadEndGetting;
    procedure MainThreadRemoveFilter;
    procedure ExtractFile;
    procedure RefreshList;
    procedure DlgReport;
    procedure GetInfo(const limit: Integer; const cs: TCheckStyleType);
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    CS_Threads,
    CS_AddInfoToData,
    CS_AddNamesAndLinks: TRTLCriticalSection;
    isFinishSearchingForNewManga, isDoneUpdateNecessary: Boolean;
    mainDataProcess: TDBDataProcess;
    tempDataProcess: TDBDataProcess;
    websites: TStringList;
    website, twebsite, twebsitetemp: String;
    Module: TModuleContainer;
    directoryCount,
    workPtr,
    websitePtr,
    numberOfThreads: Integer;
    Threads: TFPList;
    constructor Create;
    destructor Destroy; override;
    procedure CheckCommit(const CommitCount: Integer = 32);
    property CurrentDirectoryPageNumber: Integer read FCurrentGetInfoLimit write SetCurrentDirectoryPageNumber;
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
  frmMain, FMDVars, Dialogs;

const
  CL_ProgressBarBaseLine = $bcbcbc;
  CL_ProgressBarBase     = $e6e6e6;
  CL_ProgressBarLine     = $25b006;
  CL_ProgressBar         = $42d932;

{ TUpdateListThread }

constructor TUpdateListThread.Create;
begin
  inherited Create(True);
end;

destructor TUpdateListThread.Destroy;
begin
  manager.Module.DecActiveConnectionCount;
  EnterCriticalsection(manager.CS_Threads);
  try
    manager.Threads.Remove(Self);
  finally
    LeaveCriticalsection(manager.CS_Threads);
  end;
  if Assigned(Info) then
    Info.Free;
  inherited Destroy;
end;

procedure TUpdateListThread.Execute;
var
  names, links: TStringList;
  i: Integer;
begin
  try
    if checkStyle = CS_INFO then
      Info := TMangaInformation.Create(Self, True)
    else
      Info := TMangaInformation.Create(Self, False);
    Info.isGetByUpdater := True;
    info.ModuleId := manager.Module.ID;

    case CheckStyle of
      CS_DIRECTORY_COUNT:
          info.GetDirectoryPage(manager.Module.TotalDirectoryPage[workPtr], manager.website);

      //get names and links
      CS_DIRECTORY_PAGE:
      begin
        names := TStringList.Create;
        links := TStringList.Create;
        try
          if BROWSER_INVERT then
            workPtr := manager.Module.TotalDirectoryPage[manager.Module.CurrentDirectoryIndex] - workPtr -1;
          Info.GetNameAndLink(names, links, manager.website, IntToStr(workPtr));

          //if website has sorted list by latest added
          //we will stop at first found against current db
          if links.Count > 0 then
          begin
            EnterCriticalSection(manager.CS_AddNamesAndLinks);
            try
              if manager.FIsPreListAvailable then begin
                for i:=0 to links.Count-1 do begin
                  if manager.mainDataProcess.AddData(names[i],links[i],'','','','','',0,0) then
                    manager.tempDataProcess.AddData(names[i],links[i],'','','','','',0,0)
                  else if (manager.isFinishSearchingForNewManga=False) and manager.Module.SortedList and (not BROWSER_INVERT) then
                    manager.isFinishSearchingForNewManga:=True;
                end;
                manager.mainDataProcess.Rollback;
              end
              else
                for i:=0 to links.Count-1 do
                  manager.tempDataProcess.AddData(names[i],links[i],'','','','','',0,0);
              manager.tempDataProcess.Commit;
            finally
              LeaveCriticalSection(manager.CS_AddNamesAndLinks);
            end;
          end;
        finally
          names.Free;
          links.Free;
        end;
      end;

      CS_INFO:
      begin
        Info.mangaInfo.title:=title;
        Info.mangaInfo.link:=link;
        if link<>'' then begin
          Info.GetInfoFromURL(manager.website,link);
          // status = '-1' mean it's not exist and shouldn't be saved to database
          if (not Terminated) and (Info.mangaInfo.status <> '-1') then
          begin
            EnterCriticalSection(manager.CS_AddInfoToData);
            try
              Info.AddInfoToData(title,link,manager.mainDataProcess);
              manager.CheckCommit(manager.numberOfThreads);
            finally
              LeaveCriticalSection(manager.CS_AddInfoToData);
            end;
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
        '  Title   : ' + title + LineEnding +
        '  URL     : ' + link + LineEnding;
      end;
      MainForm.ExceptionHandler(Self, E);
    end;
  end;
end;

{ TUpdateListManagerThread }

procedure TUpdateListManagerThread.MainThreadEndGetting;
begin
  isUpdating:=False;
  if isPendingExitCounter then
    MainForm.DoExitWaitCounter;
end;

procedure TUpdateListManagerThread.MainThreadRemoveFilter;
begin
  MainForm.btRemoveFilterClick(MainForm.btRemoveFilter);
end;

procedure TUpdateListManagerThread.ExtractFile;
var
  Sza, datapath, filepath: String;
begin
  Sza := FMD_DIRECTORY + ZIP_EXE;
  if not FileExistsUTF8(Sza) then Exit;

  datapath := DATA_FOLDER;
  filepath := datapath + website;
  if FileExistsUTF8(filepath + '.7z') then
     filepath += '.7z'
  else
  if FileExistsUTF8(filepath + '.zip') then
    filepath += '.zip';

  if FileExistsUTF8(filepath) then
  begin
    if FileExistsUTF8(datapath + website + DBDATA_EXT) then
      DeleteFileUTF8(datapath + website + DBDATA_EXT);
    if FileExistsUTF8(datapath + website + DATA_EXT) then
      DeleteFileUTF8(datapath + website + DATA_EXT);
    RunExternalProcess(Sza, ['x', filepath, '-o' +
      AnsiQuotedStr(datapath, '"'), '-aoa'], False, True);
    DeleteFileUTF8(filepath);
  end
end;

constructor TUpdateListManagerThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;

  InitCriticalSection(CS_Threads);
  InitCriticalSection(CS_AddInfoToData);
  InitCriticalSection(CS_AddNamesAndLinks);
  InitCriticalSection(FCS_CurrentGetInfoLimit);

  websites := TStringList.Create;
  mainDataProcess := TDBDataProcess.Create;
  tempDataProcess := TDBDataProcess.Create;
  Threads := TFPList.Create;
  FThreadEndNormally:=False;
  FThreadAborted:=False;
  FIsPreListAvailable:=False;
  FCurrentGetInfoLimit := 1;

  Synchronize(SyncCreate);
end;

destructor TUpdateListManagerThread.Destroy;
begin
  Synchronize(SyncDestroy);
  if FThreadAborted then Logger.SendWarning(Self.ClassName+', thread aborted by user?');
  if not FThreadEndNormally then Logger.SendWarning(Self.ClassName+', thread doesn''t end normally, ended by user?');
  websites.Free;
  mainDataProcess.Close;
  tempDataProcess.Close;
  DeleteDBDataProcess(twebsite);
  DeleteDBDataProcess(twebsitetemp);
  mainDataProcess.Free;
  tempDataProcess.Free;
  Threads.Free;
  isUpdating := False;
  DoneCriticalsection(FCS_CurrentGetInfoLimit);
  DoneCriticalsection(CS_AddInfoToData);
  DoneCriticalsection(CS_AddNamesAndLinks);
  DoneCriticalsection(CS_Threads);
  inherited Destroy;
end;

procedure TUpdateListManagerThread.CheckCommit(const CommitCount: Integer);
begin
  Inc(FCommitCount);
  if FCommitCount >= CommitCount then
  begin
    FCommitCount := 0;
    if Assigned(mainDataProcess) then
      mainDataProcess.Commit;
  end;
end;

procedure TUpdateListManagerThread.RefreshList;
begin
  try
    with MainForm do
    begin
      if cbSelectManga.Text = website then
      begin
        vtMangaList.Clear;
        if dataProcess = nil then
          dataProcess := TDBDataProcess.Create
        else
          dataProcess.Close;
        OverwriteDBDataProcess(website, twebsite);
        OpenDataDB(website);
      end
      else
      begin
        if dataProcess.WebsiteLoaded(website) then
          dataProcess.RemoveFilter;
        OverwriteDBDataProcess(website, twebsite);
      end;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TUpdateListManagerThread.DlgReport;
begin
  MessageDlg('', Format(RS_DlgHasNewManga, [website, tempDataProcess.RecordCount]),
    mtInformation, [mbYes], 0);
end;

procedure TUpdateListManagerThread.GetInfo(const limit: Integer;
  const cs: TCheckStyleType);

  procedure WaitForThreads;
  begin
    while (not Terminated) and (Threads.Count > 0) do
      Sleep(SOCKHEARTBEATRATE);
  end;

var
  plimit, conlimit: Integer;
  s: String;
  t: TUpdateListThread;
begin
  try
    FCurrentGetInfoLimit := limit;
    while workPtr < FCurrentGetInfoLimit do begin
      if Terminated then Break;
      if FTotalPtr <> FCurrentGetInfoLimit then
        FTotalPtr := FCurrentGetInfoLimit;
      if Module.Settings.Enabled and (Module.Settings.UpdateListNumberOfThread > 0) then
        numberOfThreads := Module.Settings.UpdateListNumberOfThread
      else
        numberOfThreads := OptionMaxThreads;
      if numberOfThreads < 1 then
        numberOfThreads := 1;  //default

      conlimit := Module.GetMaxConnectionLimit;
      if conlimit < 1 then
        conlimit := numberOfThreads;

      // Finish searching for new series
      if (cs = CS_DIRECTORY_PAGE) and
        (isFinishSearchingForNewManga) then
      begin
        WaitForThreads;
        workPtr := FCurrentGetInfoLimit;
        Exit;
      end;

      if Module.GetMaxConnectionLimit > 0 then
        while (not Terminated) and (Module.ActiveConnectionCount >= conlimit) do
          Sleep(SOCKHEARTBEATRATE)
      else
        while (not Terminated) and (Threads.Count >= numberOfThreads) do
          Sleep(SOCKHEARTBEATRATE);

      if Terminated then Break;
      if Threads.Count < numberOfThreads then
      begin
        EnterCriticalsection(CS_Threads);
        try
          if Module.ActiveConnectionCount >= conlimit then Exit;
          Module.IncActiveConnectionCount;
          t := TUpdateListThread.Create;
          Threads.Add(t);
          if cs=CS_INFO then begin
            t.title:=tempDataProcess.Value[workPtr,DATA_PARAM_TITLE];
            t.link:=tempDataProcess.Value[workPtr,DATA_PARAM_LINK];
          end;
          t.checkStyle:=cs;
          t.manager:=Self;
          t.workPtr:=Self.workPtr;
          t.Start;
          Inc(workPtr);
          s := RS_UpdatingList + Format(' [%d/%d] %s | [T:%d] [%d/%d]',
            [websitePtr, websites.Count, website, Threads.Count, workPtr, FCurrentGetInfoLimit]);

          case cs of
            CS_DIRECTORY_COUNT:
              begin
                if FCurrentGetInfoLimit = 1 then
                  s := RS_UpdatingList + Format(' [%d/%d] ', [websitePtr, websites.Count]) +
                    website + ' | ' + RS_GettingDirectory + '...'
                else
                  s := s + ' | ' + RS_GettingDirectory + '...';
              end;
            CS_DIRECTORY_PAGE:
              begin
                s += ' | ' + RS_LookingForNewTitle +
                  Format(' %d/%d', [Module.CurrentDirectoryIndex + 1, Module.TotalDirectory]) +
                  '...';
              end;
            CS_INFO:
              s := Format('%s | %s "%s"', [s, RS_GettingInfo, tempDataProcess.Value[workPtr-1,DATA_PARAM_TITLE]]);
          end;
          UpdateStatusText(s);
          FWorkPtr := workPtr + 1;
        finally
          LeaveCriticalsection(CS_Threads);
        end;
      end;
      // wait for threads and new data
      if workPtr >= FCurrentGetInfoLimit then
      begin
        plimit := FCurrentGetInfoLimit;
        while Threads.Count > 0 do
        begin
          if Terminated then Break;
          // if limit changed, break and continue the loop with new limit
          if FCurrentGetInfoLimit <> plimit then Break;
          Sleep(SOCKHEARTBEATRATE);
        end;
      end;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
  WaitForThreads;
end;

procedure TUpdateListManagerThread.DoTerminate;
var
  i: Integer;
begin
  EnterCriticalsection(CS_Threads);
  try
    if Threads.Count > 0 then
      for i := 0 to Threads.Count - 1 do
        TUpdateListThread(Threads[i]).Terminate;
  finally
    LeaveCriticalsection(CS_Threads);
  end;
    while Threads.Count > 0 do
      Sleep(SOCKHEARTBEATRATE);
  inherited DoTerminate;
end;

procedure TUpdateListManagerThread.SetCurrentDirectoryPageNumber(AValue: Integer);
begin
  if AValue < FCurrentGetInfoLimit then Exit;
  try
    EnterCriticalsection(FCS_CurrentGetInfoLimit);
    FCurrentGetInfoLimit := AValue;
  finally
    LeaveCriticalsection(FCS_CurrentGetInfoLimit);
  end;
end;

procedure TUpdateListManagerThread.SyncCreate;
var
  txtHeight: Integer;
begin
  FControlMargin := FormMain.ScaleFontTo96(2);
  FProgressBarHeight := FormMain.ScaleFontTo96(7);
  FStatusBar := TPanel.Create(nil);
  with FStatusBar do begin
    Parent := MainForm;
    DoubleBuffered := True;
    Align := alBottom;
    AutoSize := False;
    txtHeight := Canvas.GetTextHeight('A');
    Height := txtHeight + FProgressBarHeight + (FControlMargin * 6);
    Caption := '';
    Color := clBtnFace;
    BevelOuter := bvNone;
    BevelInner := bvNone;
    BorderStyle := bsNone;
    BorderSpacing.Top := FControlMargin;
    OnPaint := StatusBarPaint;
    OnResize := StatusBarRezise;
    OnShowHint := StatusBarShowHint;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Style := psSolid;
    FResized := True;
  end;

  FButtonCancel := TSpeedButton.Create(FStatusBar);
  with FButtonCancel do begin
    Parent := FStatusBar;
    Align := alNone;
    AutoSize := False;
    Flat := True;
    Anchors := [akTop, akRight, akBottom];
    AnchorSideTop.Control := FStatusBar;
    AnchorSideTop.Side := asrTop;
    BorderSpacing.Top := FControlMargin;
    AnchorSideRight.Control := FStatusBar;
    AnchorSideRight.Side := asrRight;
    BorderSpacing.Right := FControlMargin;
    AnchorSideBottom.Control := FStatusBar;
    AnchorSideBottom.Side := asrBottom;
    BorderSpacing.Bottom := FControlMargin;
    Width := Height;
    OnClick := ButtonCancelClick;
    Images := FormMain.IconList;
    ImageIndex := 24;
  end;

  StatusBarRezise(FStatusBar);

  FTimerRepaint := TTimer.Create(FStatusBar);
  FTimerRepaint.Interval := 1000;
  FTimerRepaint.OnTimer := TimerRepaintTimer;
  FTimerRepaint.Enabled := True;
  FNeedRepaint := True;
end;

procedure TUpdateListManagerThread.SyncDestroy;
begin
  FStatusBar.Free;
end;

procedure TUpdateListManagerThread.TimerRepaintTimer(Sender: TObject);
begin
  if FNeedRepaint then
  begin
    FNeedRepaint := False;
    FStatusBar.Repaint;
  end;
end;

procedure TUpdateListManagerThread.StatusBarPaint(Sender: TObject);
var
  txtHeight: integer;
  barPercents: Double;
begin
  with FStatusBar.Canvas do
  begin
    Pen.Color := clActiveBorder;
    Line(0,0,FStatusBar.ClientRect.Right,0);

    if FResized then
    begin
      FStatusTextRect := FStatusBar.ClientRect;
      FStatusTextRect.Right := FButtonCancel.Left;
      FStatusTextRect.Inflate(-(FControlMargin * 2), -(FControlMargin * 2));
      FProgressBarRect := FStatusTextRect;
      FProgressBarRect.Top := FProgressBarRect.Bottom - FProgressBarHeight;
      FStatusTextRect.Bottom := FProgressBarRect.Top - FControlMargin;
      FResized := False;
    end;

    Brush.Style := bsSolid;
    Pen.Style := psSolid;

    Pen.Color := CL_ProgressBarBaseLine;
    Brush.Color := CL_ProgressBarBase;
    Rectangle(FProgressBarRect);

    if FTotalPtr = 0 then
      FTotalPtr := 100;
    if FWorkPtr > FTotalPtr then
      FWorkPtr := FTotalPtr;
    barPercents := FWorkPtr / FTotalPtr;
    if barPercents > 0 then
    begin
      FProgressBarPercentsRect := FProgressBarRect;
      FProgressBarPercentsRect.Right :=
        Round((FProgressBarPercentsRect.Right - FProgressBarPercentsRect.Left) * barPercents) + FProgressBarPercentsRect.Left;

      Pen.Color   := CL_ProgressBarLine;
      Brush.Color := CL_ProgressBar;

      Frame(FProgressBarPercentsRect);
      FProgressBarPercentsRect.Inflate(-2, -2);
      GradientFill(FProgressBarPercentsRect, BlendColor(Brush.Color, CL_ProgressBarBase, 128), Brush.Color, gdHorizontal);
    end;
    Brush.Style := bsClear;
    txtHeight := GetTextHeight(FStatusText);
    TextRect(FStatusTextRect, FStatusTextRect.Left, FStatusTextRect.Top + ((FStatusTextRect.Bottom - FStatusTextRect.Top - txtHeight) div 2), FStatusText);
  end;
end;

procedure TUpdateListManagerThread.StatusBarRezise(Sender: TObject);
begin
  FResized := True;
end;

procedure TUpdateListManagerThread.StatusBarShowHint(Sender: TObject;
  HintInfo: PHintInfo);
begin
  HintInfo^.HintStr := Trim(websites.Text);
end;

procedure TUpdateListManagerThread.ButtonCancelClick(Sender: TObject);
begin
  Self.Terminate;
end;

procedure TUpdateListManagerThread.UpdateStatusText(AStatusText: String);
begin
  if AStatusText <> FStatusText then
  begin
    FStatusText := AStatusText;
    FNeedRepaint := True;
  end;
end;

procedure TUpdateListManagerThread.Execute;
var
  j, k: Integer;
  cloghead: String;
begin
  if websites.Count = 0 then
    Exit;
  try
    websitePtr := 0;
    while websitePtr < websites.Count do
    begin
      FThreadAborted:=True;
      website := websites.Strings[websitePtr];
      if Modules.ModuleAvailable(website, Module) then
      begin
        Inc(websitePtr);

        cloghead:=Self.ClassName+', '+website+': ';
        UpdateStatusText(RS_UpdatingList + Format(' [%d/%d] %s',
          [websitePtr, websites.Count, website]) + ' | ' + RS_Preparing + '...');

        twebsite:='__'+website;
        twebsitetemp:=twebsite+'_templist';
        try
          DeleteDBDataProcess(twebsite);
          DeleteDBDataProcess(twebsitetemp);
          if (dataProcess.Website = website) and
            (dataProcess.Connected) then
            dataProcess.Backup(twebsite)
          else
          begin
            if dataProcess.WebsiteLoaded(website) then
              Synchronize(MainThreadRemoveFilter);
            CopyDBDataProcess(website, twebsite);
          end;

          if not mainDataProcess.Connect(twebsite) then
            mainDataProcess.CreateDatabase(twebsite);
          tempDataProcess.CreateDatabase(twebsitetemp);

          // get directory page count
          directoryCount := 0;
          workPtr := 0;
          Modules.AfterUpdateList(Module.ID);
          Modules.BeforeUpdateList(Module.ID);
          GetInfo(Module.TotalDirectory, CS_DIRECTORY_COUNT);

          if Terminated then
          begin
              Modules.AfterUpdateList(Module.ID);
            Break;
          end;

          mainDataProcess.OpenTable('',True);
          FIsPreListAvailable:=mainDataProcess.RecordCount>0;
          mainDataProcess.CloseTable;

          // get names and links
          workPtr := 0;
          isFinishSearchingForNewManga := False;
          j := Low(Module.TotalDirectoryPage);
          while j <= High(Module.TotalDirectoryPage) do
          begin
            workPtr := 0;
            isFinishSearchingForNewManga := False;
            Module.CurrentDirectoryIndex := j;
            GetInfo(Module.TotalDirectoryPage[j], CS_DIRECTORY_PAGE);
            Inc(j);
            if Terminated then Break;
          end;

          Modules.BeforeUpdateList(Module.ID);
          if Terminated then
            if not (OptionUpdateListNoMangaInfo and not(Module.SortedList)) then
              Break;

          UpdateStatusText(RS_UpdatingList + Format(' [%d/%d] %s',
            [websitePtr, websites.Count, website]) + ' | ' + RS_IndexingNewTitle + '...');

          tempDataProcess.OpenTable('', True);
          // get manga info
          if tempDataProcess.RecordCount>0 then
          begin
            workPtr := 0;
            FCommitCount := 0;
            if not Module.InformationAvailable or
              OptionUpdateListNoMangaInfo then
            begin
              Inc(workPtr);
              for k:=0 to tempDataProcess.RecordCount-1 do
              begin
                mainDataProcess.AddData(
                  tempDataProcess.Value[k,DATA_PARAM_TITLE],
                  tempDataProcess.Value[k,DATA_PARAM_LINK],
                  '',
                  '',
                  '',
                  '',
                  '',
                  0,
                  Now
                  );
                CheckCommit(5000);
              end;
            end
            else
              GetInfo(tempDataProcess.RecordCount, CS_INFO);
            mainDataProcess.Commit;

            if (workPtr > 0) and (not (Terminated and Module.SortedList)) then
            begin
              UpdateStatusText(RS_UpdatingList + Format(' [%d/%d] %s',
                [websitePtr, websites.Count, website]) + ' | ' + RS_SavingData + '...');
              mainDataProcess.Sort;
              mainDataProcess.Close;
              Synchronize(RefreshList);
            end;
          end;
        except
          on E: Exception do
            Logger.SendException(cloghead + 'error occured!', E);
        end;

        tempDataProcess.Close;
        mainDataProcess.Close;
        DeleteDBDataProcess(twebsite);
        DeleteDBDataProcess(twebsitetemp);

        if Terminated then
          Break;
        websites[websitePtr - 1] := UTF8Encode(#$2714) + websites[websitePtr - 1];
        FThreadAborted:=False;
      end;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
  FThreadEndNormally:=True;
  Synchronize(MainThreadEndGetting);
end;

end.
