unit uMain;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}
  cthreads,
  cmem,
  {$endif}
  Classes, SysUtils, zipper, FileUtil,LazFileUtils, LazUTF8, LazUTF8Classes,
  Forms, Dialogs, ComCtrls, StdCtrls, Clipbrd, ExtCtrls, DefaultTranslator,
  RegExpr, IniFiles, USimpleException, uMisc, uTranslation,
  httpsend, blcksock, ssl_openssl;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    itMonitor: TIdleTimer;
    lbTransferRate: TLabel;
    lbFileSize: TLabel;
    lbFileSizeValue: TLabel;
    lbStatus: TLabel;
    lbTransferRateValue: TLabel;
    pbDownload :TProgressBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender :TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender :TObject);
    procedure itMonitorTimer(Sender: TObject);
  private
    { private declarations }
  public
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    { public declarations }
  end;

  { TDownloadThread }

  TDownloadThread = class(TThread)
  private
    FTotalSize, FCurrentSize :integer;
    FHTTP                    :THTTPSend;
    FStatus                  :string;
    FProgress                :string;
    FErrorMessage            :string;
    UZip                     :TUnZipper;
  protected
    procedure SockOnStatus(Sender :TObject; Reason :THookSocketReason;
      const Value :string);
    procedure SockOnHeartBeat(Sender :TObject);
    procedure UZipOnStartFile(Sender :TObject; const AFileName :string);
    procedure MainThreadUpdateStatus;
    procedure MainThreadUpdateProgress;
    procedure MainThreadUpdateProgressLabel;
    procedure MainThreadErrorGetting;
    procedure UpdateStatus(AStatus :string);
    procedure ShowErrorMessage(AMessage :string);
    procedure Execute; override;
  public
    URL      :string;
    isSFURL  :boolean;
    FileName :string;
    DirPath  :string;
    MaxRetry :cardinal;
    Extract  :boolean;
    constructor Create;
    destructor Destroy; override;
  end;

procedure IncReadCount(const ACount: Int64);

var
  frmMain      :TfrmMain;
  dl           :TDownloadThread;
  isDownload   :boolean = False;
  ReadCount    :Integer = 0;
  CS_ReadCount :TRTLCriticalSection;

  _UpdApp    :boolean = False;
  _Extract   :boolean = False;
  _NoError   :boolean = False;
  _URL       :string = '';
  _MaxRetry  :cardinal = 1;
  _LaunchApp :string = '';

  ProxyType :string;
  ProxyHost :string;
  ProxyPort :string;
  ProxyUser :string;
  ProxyPass :string;

const
  Symbols: array [0..10] of Char =
    ('\', '/', ':', '*', '?', '"', '<', '>', '|', #9, ';');

  mf_data_link = 'https://www.mediafire.com/folder/fwa8eomz80uk1/Data';

resourcestring
  RS_InvalidURL = 'Invalid URL!';
  RS_Response = 'Response';
  RS_AnErrorOccured = 'An error occured when trying to download file.';
  RS_FileNotFound = 'File not found!';
  RS_ServerError = 'Server response error!';
  RS_LoadingPage = 'Loading page...';
  RS_FailedLoadPage = 'Failed loading page!';
  RS_RetryLoadPage = 'Retry loading page[%d]...';
  RS_Redirected = 'Redirected...';
  RS_Downloading = 'Downloading [%s]...';
  RS_FailedDownloadPage = 'Failed downloading file!';
  RS_RetryDownloading = 'Retry downloading[%d] [%s]...';
  RS_SaveFile = 'Saving file... ';
  RS_WaitMainApp = 'Waiting main app to close...';
  RS_UnpackFile = 'Unpacking file [%s]...';
  RS_Finished = 'Finished.';
  RS_ErrorCheckAntiVirus = 'Error saving file, check your AntiVirus!';
  RS_FileNotFound_mfdatalink =
    'File not found!' + LineEnding +
    'This site probably have been added to unofficial build.' + LineEnding +
    LineEnding +
    'Remedy:' + LineEnding +
    'Build you manga list from scratch or download manually from this link:' +
    LineEnding +
    '%s' + LineEnding +
    'Link copied to clipboard!';
  RS_7zNotFound = 'Can''t extract file because 7za.exe not found!';
  RS_Unknown = '(unknown)';

implementation

uses uMessage;

{$R *.lfm}

function RemoveSymbols(const input: String): String;
var
  i: Integer;
begin
  Result := input;
  for i := Low(Symbols) to High(Symbols) do
  begin
    if Pos(Symbols[i], Result) > 0 then
      Result := StringReplace(Result, Symbols[i], '', [rfReplaceAll]);
  end;

  if (Length(Result) > 0) and
    (Result[Length(Result)] = '.') then
  begin
    Result[Length(Result)] := '-';
  end;
end;

procedure IncReadCount(const ACount: Int64);
begin
  EnterCriticalsection(CS_ReadCount);
  try
    Inc(ReadCount, ACount);
  finally
    LeaveCriticalsection(CS_ReadCount);
  end;
end;

{  TDownloadThread }

constructor TDownloadThread.Create;
begin
  inherited Create(True);
  isDownload := True;
  FreeOnTerminate := True;
  FHTTP := THTTPSend.Create;
  FHTTP.Headers.NameValueSeparator := ':';
  FHTTP.Sock.OnStatus := @SockOnStatus;
  FHTTP.Sock.OnHeartbeat := @SockOnHeartBeat;
  FHTTP.Sock.HeartbeatRate := 250;
  FTotalSize := 0;
  FCurrentSize := 0;
  URL := '';
  FileName := '';
  DirPath := '';
  MaxRetry := 1;
  Extract := False;
end;

destructor TDownloadThread.Destroy;
begin
  FHTTP.Free;
  isDownload := False;
  inherited Destroy;
end;

procedure TDownloadThread.MainThreadUpdateStatus;
begin
  frmMain.lbStatus.Caption := FStatus;
end;

procedure TDownloadThread.UpdateStatus(AStatus :string);
begin
  FStatus := AStatus;
  Synchronize(@MainThreadUpdateStatus);
end;

procedure TDownloadThread.ShowErrorMessage(AMessage: string);
begin
  FErrorMessage := AMessage;
  Synchronize(@MainThreadErrorGetting);
end;

procedure TDownloadThread.SockOnHeartBeat(Sender :TObject);
begin
  if Self.Terminated then
  begin
    TBlockSocket(Sender).StopFlag := True;
    TBlockSocket(Sender).AbortSocket;
  end;
end;

procedure TDownloadThread.MainThreadUpdateProgress;
var
  barPos: Integer;
  prgText: string;
begin
  if Terminated then Exit;
  if FCurrentSize > 0then
  begin
    if FTotalSize > 0 then
    begin
      barPos := Round(1000 * (FCurrentSize / FTotalSize));
      prgText := FormatByteSize(FCurrentSize) + ' / ' + FormatByteSize(FTotalSize);
    end
    else
    begin
      barPos := 0;
      prgText := FormatByteSize(FCurrentSize);
    end;
  end
  else
  begin
    barPos := 0;
    prgText := RS_Unknown;
  end;
  frmMain.pbDownload.Position := barPos;
  frmMain.lbFileSizeValue.Caption := prgText;
end;

procedure TDownloadThread.MainThreadErrorGetting;
begin
  if not _NoError then
    MessageDlg('Error', FErrorMessage,
      mtError, [mbOK], 0);
end;

procedure TDownloadThread.SockOnStatus(Sender :TObject; Reason :THookSocketReason;
  const Value :string);
begin
  if Terminated then Exit;
  if FHTTP.Headers.IndexOfName('Content-Length') > -1 then
    FTotalSize := StrToIntDef(FHTTP.Headers.Values['Content-Length'], 0);
  case Reason of
    HR_Connect :FCurrentSize := 0;
    HR_ReadCount :
      begin
        Inc(FCurrentSize, StrToIntDef(Value, 0));
        IncReadCount(StrToIntDef(Value, 0));
      end;
  end;
  Synchronize(@MainThreadUpdateProgress);
end;

procedure TDownloadThread.MainThreadUpdateProgressLabel;
begin
  frmMain.lbFileSizeValue.Caption := FProgress;
end;

procedure TDownloadThread.UZipOnStartFile(Sender :TObject; const AFileName :string);
begin
  if FileExistsUTF8(AFileName) then
    DeleteFileUTF8(AFileName);
  UpdateStatus(Format(RS_UnpackFile, [ExtractFileName(AFileName)]));
end;

procedure TDownloadThread.Execute;

  procedure PrepareHTTP(var HTTP :THTTPSend);
  begin
    if HTTP <> nil then
    begin
      FHTTP.Clear;
      FHTTP.Cookies.Clear;
      FHTTP.Protocol := '1.1';
      FHTTP.UserAgent := UA_FIREFOX;
      FHTTP.Timeout := 30000;

      if ProxyType = 'HTTP' then
      begin
        HTTP.ProxyHost := ProxyHost;
        HTTP.ProxyPort := ProxyPort;
        HTTP.ProxyUser := ProxyUser;
        HTTP.ProxyPass := ProxyPass;
      end
      else
      if (ProxyType = 'SOCKS4') or (ProxyType = 'SOCKS5') then
      begin
        if ProxyType = 'SOCKS4' then
          HTTP.Sock.SocksType := ST_Socks4
        else
        if ProxyType = 'SOCKS5' then
          HTTP.Sock.SocksType := ST_Socks5;
        HTTP.Sock.SocksIP := ProxyHost;
        HTTP.Sock.SocksPort := ProxyPort;
        HTTP.Sock.SocksUsername := ProxyUser;
        http.Sock.SocksPassword := ProxyPass;
      end
      else
      begin
        HTTP.Sock.SocksIP := ProxyHost;
        HTTP.Sock.SocksPort := ProxyPort;
        HTTP.Sock.SocksUsername := ProxyUser;
        http.Sock.SocksPassword := ProxyPass;
        HTTP.ProxyHost := ProxyHost;
        HTTP.ProxyPort := ProxyPort;
        HTTP.ProxyUser := ProxyUser;
        HTTP.ProxyPass := ProxyPass;
      end;
    end;
  end;

var
  regx           : TRegExpr;
  i, ctry        : Cardinal;
  Sza,
  rurl,
  fname,
  sproject,
  sdir,
  sfile          : String;
  st, HTTPHeaders: TStringList;
  filestream     : TFileStreamUTF8;
begin
  URL := Trim(URL);
  HTTPHeaders := TStringList.Create;
  regx := TRegExpr.Create;
  try
    PrepareHTTP(FHTTP);
    HTTPHeaders.NameValueSeparator := ':';
    regx.ModifierI := True;
    if isSFURL then
    begin
      FHTTP.UserAgent := UA_CURL;
      regx.Expression := '/download$';
      URL := Trim(regx.Replace(URL, '', False));
      //**parsing SF url
      regx.Expression := '^.*sourceforge.net/projects/(.+)/files/(.+)/([^/]+)$';
      if not regx.Exec(URL) then
      begin
        regx.Free;
        UpdateStatus(RS_InvalidURL);
        Exit;
      end;
      sproject := regx.Replace(URL, '$1', True);
      sdir := regx.Replace(URL, '$2', True);
      sfile := regx.Replace(URL, '$3', True);
      if FileName = '' then
        FileName := sfile;
      rurl := 'http://sourceforge.net/projects/' + sproject + '/files/' +
        sdir + '/' + sfile + '/download';
    end
    else
    begin
      rurl := URL;
      regx.ModifierG := True;
      regx.Expression := '^.*/([^/]*filename=)?([^/]*)$';
      FileName := regx.Replace(URL, '$2', True);
      regx.Expression := '(\.\w+)[\?\&].*$';
      FileName := regx.Replace(FileName, '$1', True);
      FileName := RemoveSymbols(FileName);
      if FileName = '' then
        FileName := 'new_version.7z';
    end;

    FHTTP.Headers.Text := HTTPHeaders.Text;
    //**loading page
    UpdateStatus(RS_LoadingPage);
    ctry := 0;
    while (not FHTTP.HTTPMethod('HEAD', Trim(rurl))) or
      (FHTTP.ResultCode >= 400) or (FHTTP.ResultCode < 100) do
    begin
      if Self.Terminated then Break;
      if (FHTTP.ResultCode >= 500) or (FHTTP.ResultCode < 100) then
      begin
        if ctry >= MaxRetry then
        begin
          UpdateStatus(RS_FailedLoadPage);
          ShowErrorMessage(RS_FailedLoadPage + LineEnding + LineEnding +
            RS_Response + ':' + LineEnding +
            IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString);
          Break;
        end;
        Inc(ctry);
        UpdateStatus(Format(RS_RetryLoadPage, [ctry]));
      end
      else
      if (FHTTP.ResultCode >= 400) and (FHTTP.ResultCode < 500) then
      begin
        UpdateStatus(RS_FileNotFound);
        if _UpdApp then
          ShowErrorMessage(RS_FileNotFound + LineEnding + LineEnding +
            RS_Response + ':' + LineEnding +
            IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString)
        else
        begin
          Clipboard.AsText := mf_data_link;
          ShowErrorMessage(Format(RS_FileNotFound_mfdatalink, [mf_data_link]));
        end;
        Break;
      end;
      FHTTP.Clear;
    end;

    if (FHTTP.ResultCode >= 300) or isSFURL then
    begin
      HTTPHeaders.Values['Referer'] := ' ' + rurl;
      UpdateStatus(RS_Redirected);
      rurl := Trim(FHTTP.Headers.Values['Location']);
      if isSFURL then
      begin
        if (Pos('use_mirror=', rurl) > 0) then
        begin
          regx.Expression := '.*use_mirror=(.+)$';
          rurl := regx.Replace(rurl, '$1', True);
          rurl := 'http://' + rurl + '.dl.sourceforge.net/project/' +
            sproject + '/' + sdir + '/' + sfile;
        end;
      end;
    end;

    //**download file
    if (FHTTP.ResultCode >= 100) and (FHTTP.ResultCode < 400) then
    begin
      UpdateStatus(Format(RS_Downloading, [FileName]));
      ctry := 0;
      FHTTP.Clear;
      HTTPHeaders.Values['Accept'] := ' */*';
      FHTTP.Headers.Text := HTTPHeaders.Text;
      while (not FHTTP.HTTPMethod('GET', Trim(rurl))) or
        (FHTTP.ResultCode >= 300) do
      begin
        if Self.Terminated then Break;
        if (FHTTP.ResultCode >= 500) or (FHTTP.ResultCode < 100) then
        begin
          if ctry >= MaxRetry then
          begin
            UpdateStatus(RS_FailedDownloadPage);
            ShowErrorMessage(RS_AnErrorOccured + LineEnding + LineEnding +
              RS_Response + ':' + LineEnding +
              IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString);
            Break;
          end;
          Inc(ctry);
          UpdateStatus(Format(RS_RetryDownloading, [ctry, FileName]));
        end
        else
        if (FHTTP.ResultCode >= 400) and (FHTTP.ResultCode < 500) then
        begin
          UpdateStatus(RS_FileNotFound);
          if _UpdApp then
            ShowErrorMessage(RS_FileNotFound + LineEnding + LineEnding +
              RS_Response + ':' + LineEnding +
              IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString)
          else
          begin
            Clipboard.AsText := mf_data_link;
            ShowErrorMessage(Format(RS_FileNotFound_mfdatalink, [mf_data_link]));
          end;
          Break;
        end
        else
        if FHTTP.ResultCode >= 300 then
        begin
          HTTPHeaders.Values['Referer'] := ' ' + rurl;
          rurl := Trim(FHTTP.Headers.Values['location']);
        end;
        FHTTP.Clear;
        FHTTP.Headers.Text := HTTPHeaders.Text;
      end;
    end;

    //**save and unpack file
    if (not Self.Terminated) and
      (FHTTP.ResultCode >= 100) and (FHTTP.ResultCode < 300) and
      (FCurrentSize >= FTotalSize) then
    begin
      fname := DirPath + DirectorySeparator + FileName;
      if FileExistsUTF8(fname) then
        DeleteFileUTF8(fname);

      if ForceDirectoriesUTF8(DirPath) then
      begin
        UpdateStatus(RS_SaveFile);
        filestream := TFileStreamUTF8.Create(fname, fmCreate);
        try
          FHTTP.Document.SaveToStream(filestream);
        finally
          filestream.Free;
        end;
      end;
      if not FileExistsUTF8(fname) then
        ShowErrorMessage(RS_ErrorCheckAntiVirus);

      if Extract and FileExistsUTF8(fname) then
      begin
        UpdateStatus(Format(RS_UnpackFile, [fname]));
        Sza := GetCurrentDirUTF8 + DirectorySeparator + '7za.exe';
        if _UpdApp and
          FileExistsUTF8(GetCurrentDirUTF8 + DirectorySeparator + '7za.exe') then
          begin
            st := TStringList.Create;
            try
              FindAllFiles(st, GetCurrentDirUTF8, '*.dbg', False);
              if st.Count > 0 then
                for i := 0 to st.Count - 1 do
                  DeleteFileUTF8(st[i]);
            finally
              st.Free;
            end;
            CopyFile(GetCurrentDirUTF8 + DirectorySeparator + '7za.exe',
              GetCurrentDirUTF8 + DirectorySeparator + 'old_7za.exe');
            Sza := GetCurrentDirUTF8 + DirectorySeparator + 'old_7za.exe';
          end;
        if Pos('.zip', LowerCase(FileName)) <> 0 then
        begin
          UZip := TUnZipper.Create;
          UZip.OnStartFile := @UZipOnStartFile;
          try
            UZip.FileName := DirPath + DirectorySeparator + FileName;
            UZip.OutputPath := Dirpath;
            UZip.Examine;
            UZip.UnZipAllFiles;
          finally
            UZip.Free;
            DeleteFileUTF8(DirPath + DirectorySeparator + FileName);
          end;
        end
        else
        begin
          if FileExistsUTF8(Sza) then
          begin
            RunExternalProcess(Sza, ['x', fname, '-o' +
              AnsiQuotedStr(DirPath, '"'), '-aoa'], False, False);
            Sleep(250);
            DeleteFileUTF8(fname);
          end
          else
            ShowErrorMessage(RS_7zNotFound);
        end;
        if FileExistsUTF8(GetCurrentDirUTF8 + DirectorySeparator + 'old_7za.exe') then
          if FileExistsUTF8(GetCurrentDirUTF8 + DirectorySeparator + '7za.exe') then
            DeleteFileUTF8(GetCurrentDirUTF8 + DirectorySeparator + 'old_7za.exe')
          else
            RenameFileUTF8(GetCurrentDirUTF8 + DirectorySeparator + 'old_7za.exe',
              GetCurrentDirUTF8 + DirectorySeparator + '7za.exe');
      end;
    end;
    UpdateStatus(RS_Finished);
    if (not Self.Terminated) and _UpdApp and (_LaunchApp <> '') then
      RunExternalProcess(_LaunchApp, [''], True, True);
  except
    on E: Exception do
      frmMain.ExceptionHandler(Self, E);
  end;
  regx.Free;
  HTTPHeaders.Free;
end;

{ TfrmMain }

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if isDownload then
  begin
    dl.Terminate;
    dl.WaitFor;
  end;
  CloseAction := caFree;
end;

procedure TfrmMain.FormCreate(Sender :TObject);
var
  config :TIniFile;
begin
  Randomize;
  InitSimpleExceptionHandler(ChangeFileExt(Application.ExeName, '.log'));
  uTranslation.LangDir := CleanAndExpandDirectory(GetCurrentDirUTF8) + 'languages';
  uTranslation.LangAppName := 'updater';
  uTranslation.CollectLanguagesFiles;
  InitCriticalSection(CS_ReadCount);
  //load proxy config from fmd
  config := TIniFile.Create('config/config.ini');
  try
    if config.ReadBool('connections', 'UseProxy', False) then
    begin
      ProxyType := config.ReadString('connections', 'ProxyType', 'HTTP');
      ProxyHost := config.ReadString('connections', 'Host', '');
      ProxyPort := config.ReadString('connections', 'Port', '');
      ProxyUser := config.ReadString('connections', 'User', '');
      ProxyPass := config.ReadString('connections', 'Pass', '');
    end
    else
    begin
      ProxyType := '';
      ProxyHost := '';
      ProxyPort := '';
      ProxyUser := '';
      ProxyPass := '';
    end;
  finally
    FreeAndNil(config);
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DoneCriticalsection(CS_ReadCount);
end;

procedure TfrmMain.FormShow(Sender :TObject);
var
  s :string;
  i :Integer;
  sh :boolean = False;
begin
  if Paramcount > 0 then
  begin
    for i := 1 to Paramcount do
    begin
      s := LowerCase(ParamStrUTF8(i));
      //**Help
      if (s = '-h') or (s = '-help') or (s = '-?') or (s = '/?') then
      begin
        sh := True;
        Break;
      end
      else if s = '-q' then
        _NoError := True
      else if s = '-x' then
        _Extract := True
      else if i + 1 <= Paramcount then
      begin
        //**Update Mode
        if (s = '-a') or (s = '-d') then
        begin
          _UpdApp := (s = '-a');
          _URL := ParamStrUTF8(i + 1);
        end
        //**Max Retry
        else if s = '-r' then
          _MaxRetry := StrToIntDef(ParamStrUTF8(i + 1), 1)
        else if s = '-l' then
          _LaunchApp := ParamStrUTF8(i + 1)
        else if (LowerCase(s) = '--lang') then
          uTranslation.SetLang(ParamStrUTF8(i + 1));
      end;
    end;
  end;

  //**Show help
  if sh then
  begin
    frmMessage.Caption := Application.Title + ' - Help';
    frmMessage.ShowModal;
    Self.Close;
  end
  else
  if (_URL <> '') then
  begin
    if Pos('://', _URL) = 0 then
      _URL := 'http://' + _URL;
    if ExecRegExpr('^\w+?\://[^/]*\w+\.\w+(\:\d+)?(/|\Z).*$', _URL) then
    begin
      dl := TDownloadThread.Create;
      dl.URL := _URL;
      dl.isSFURL := (Pos('sourceforge.net/', LowerCase(dl.URL)) <> 0) or
        (Pos('sf.net/', dl.URL) <> 0);
      dl.MaxRetry := _MaxRetry;
      if _UpdApp then
        dl.DirPath := GetCurrentDirUTF8
      else
        dl.DirPath := CleanAndExpandDirectory(GetCurrentDirUTF8) + 'data';
      dl.Extract := _Extract;
      dl.Start;
      itMonitor.Enabled := True;
    end
    else
    begin
      MessageDlg(Application.Title, RS_InvalidURL, mtError, [mbOK], 0);
      Self.Close;
    end;
  end;
end;

procedure TfrmMain.itMonitorTimer(Sender: TObject);
begin
  if isDownload then
  begin
    EnterCriticalsection(CS_ReadCount);
    try
      lbTransferRateValue.Caption := FormatByteSize(ReadCount, True);
      ReadCount := 0;
    finally
      LeaveCriticalsection(CS_ReadCount);
    end;
  end
  else
  begin
    itMonitor.Enabled := False;
    Self.Close
  end;
end;

procedure TfrmMain.ExceptionHandler(Sender : TObject; E : Exception);
begin
  USimpleException.ExceptionHandle(Sender, E);
end;

end.
