unit uMain;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}
  cthreads,
  cmem,
  {$endif}
  Classes, windows, SysUtils, zipper, ShellApi, FileUtil, UTF8Process, Forms,
  Dialogs, ComCtrls, StdCtrls, Clipbrd, ExtCtrls, RegExpr, IniFiles, process,
  USimpleException, httpsend, blcksock, ssl_openssl;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btDownload :TButton;
    edFilename :TEdit;
    edURL      :TEdit;
    itMonitor: TIdleTimer;
    lbFilename :TLabel;
    lbProgress: TStaticText;
    lbStatus: TStaticText;
    lbURL      :TLabel;
    pbDownload :TProgressBar;
    procedure btDownloadClick(Sender :TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender :TObject);
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

function HeaderByName(const AHeaders :TStrings; const HeaderName :string) :string;

var
  frmMain    :TfrmMain;
  dl         :TDownloadThread;
  isDownload :boolean = False;

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

  _UA_CHROME = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.118 Safari/537.36';
  _UA_CURL = 'curl/7.21.0 (i686-pc-linux-gnu) libcurl/7.21.0 OpenSSL/0.9.8o zlib/1.2.3.4 libidn/1.18';
  mf_data_link = 'https://www.mediafire.com/folder/fwa8eomz80uk1/Data';

resourcestring
  ST_InvalidURL = 'Invalid URL!';
  ST_Response = 'Response';
  ST_AnErrorOccured = 'An error occured when trying to download file.';
  ST_FileNotFound = 'File not found!';
  ST_ServerError = 'Server response error!';
  ST_LoadingPage = 'Loading page...';
  ST_FailedLoadPage = 'Failed loading page!';
  ST_RetryLoadPage = 'Retry loading page... ';
  ST_Redirected = 'Redirected...';
  ST_Download = 'Downloading';
  ST_FailedDownloadPage = 'Failed downloading file!';
  ST_RetryDownload = 'Retry downloading';
  ST_SaveFile = 'Saving file... ';
  ST_WaitMainApp = 'Waiting main app to close...';
  ST_UnpackFile = 'Unpacking file';
  ST_Finished = 'Finished.';
  ST_ErrorCheckAntiVirus = 'Error saving file, check your AntiVirus!';
  ST_FileNotFound_mfdatalink =
    'File not found!' + LineEnding +
    'This site probably have been added to unofficial build.' + LineEnding +
    LineEnding +
    'Remedy:' + LineEnding +
    'Build you manga list from scratch or download manually from this link:';
  ST_LinkCopiedToClipboard = 'Link copied to clipboard!';
  ST_7zNotFound = 'Can''t extract file because 7za.exe not found!';

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

function RunAsAdmin(path, params: String; showWindow: Integer = SW_SHOWNORMAL;
    isPersistent: Boolean = False): Boolean;
var
 {$IFDEF WINDOWS}
  sei: TShellExecuteInfoA;
 {$ELSE}
  Process: TProcessUTF8;
 {$ENDIF}
begin
  {$IFDEF WINDOWS}
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := 0;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  if isPersistent then
    sei.fMask := sei.fMask or SEE_MASK_NOCLOSEPROCESS;
  sei.lpVerb := 'runas';
  sei.lpFile := PAnsiChar(path);
  sei.lpParameters := PAnsiChar(params);
  sei.nShow := showWindow;
  Result := ShellExecuteExA(@sei);
  if isPersistent then
    WaitForSingleObject(sei.hProcess, INFINITE);
  {$ELSE}
  Process := TProcessUTF8.Create(nil);
  Process.CommandLine := path + ' ' + params;
  Process.Execute;
  Process.Free;
  {$ENDIF}
end;

function RunExternalProcess(Exe: String; Params: array of String; ShowWind: Boolean = True;
  Detached: Boolean = False): Boolean;
var
  Process: TProcessUTF8;
  I: Integer;
begin
  Result := True;
  Process := TProcessUTF8.Create(nil);
  try
    Process.InheritHandles := False;
    Process.Executable := Exe;
    Process.Parameters.AddStrings(Params);
    if Detached then
      Process.Options := []
    else
      Process.Options := Process.Options + [poWaitOnExit];
    if ShowWind then
      Process.ShowWindow := swoShow
    else
      Process.ShowWindow := swoHIDE;
    // Copy default environment variables including DISPLAY variable for GUI application to work
    for I := 0 to GetEnvironmentVariableCount - 1 do
      Process.Environment.Add(GetEnvironmentString(I));
    Process.Execute;
  except
    Result := False;
  end;
  Process.Free;
end;

function HeaderByName(const AHeaders :TStrings; const HeaderName :string) :string;
var
  i, p :cardinal;
  hn   :string;
begin
  Result := '';
  if AHeaders.Count < 1 then
    Exit;

  hn := HeaderName;
  for i := 0 to AHeaders.Count - 1 do
  begin
    p := Pos(LowerCase(hn), LowerCase(AHeaders.Strings[i]));
    if p > 0 then
    begin
      p := Pos(':', AHeaders.Strings[i]);
      if p > 0 then
      begin
        Result := Copy(AHeaders.Strings[i], p + 2, Length(AHeaders.Strings[i]) - p + 1);
        Break;
      end;
    end;
  end;
end;

function FormatByteSize(const bytes :longint) :string;
const
  B  = 1;
  KB = 1024 * B;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if bytes > GB then
    Result := FormatFloat('#.## GB', bytes / GB)
  else
  if bytes > MB then
    Result := FormatFloat('#.## MB', bytes / MB)
  else
  if bytes > KB then
    Result := FormatFloat('#.## KB', bytes / KB)
  else
  if bytes = 0 then
    Result := '0 bytes'
  else
    Result := FormatFloat('#.## bytes', bytes);
end;

{  TDownloadThread }

constructor TDownloadThread.Create;
begin
  inherited Create(True);
  isDownload := True;
  FreeOnTerminate := True;
  FHTTP := THTTPSend.Create;
  FHTTP.Sock.OnStatus := @SockOnStatus;
  FHTTP.Sock.OnHeartbeat := @SockOnHeartBeat;
  FHTTP.Sock.HeartbeatRate := 500;
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
    prgText := '0 / 0';
  end;
  frmMain.pbDownload.Position := barPos;
  frmMain.lbProgress.Caption := prgText;
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
  if Pos('Content-Length: ', FHTTP.Headers.Text) > 0 then
    FTotalSize := StrToIntDef(HeaderByName(FHTTP.Headers, 'Content-Length'), 0);;
  case Reason of
    HR_Connect :FCurrentSize := 0;
    HR_ReadCount :Inc(FCurrentSize, StrToIntDef(Value, 0));
  end;
  Synchronize(@MainThreadUpdateProgress);
end;

procedure TDownloadThread.MainThreadUpdateProgressLabel;
begin
  frmMain.lbProgress.Caption := FProgress;
end;

procedure TDownloadThread.UZipOnStartFile(Sender :TObject; const AFileName :string);
begin
  if FileExistsUTF8(AFileName) then
    DeleteFileUTF8(AFileName);
  UpdateStatus(ST_UnpackFile + ' [' + ExtractFileName(AFileName) + ']...');
end;

procedure TDownloadThread.Execute;

  procedure PrepareHTTP(var HTTP :THTTPSend);
  begin
    if HTTP <> nil then
    begin
      FHTTP.Clear;
      FHTTP.Cookies.Clear;
      FHTTP.Protocol := '1.1';
      FHTTP.UserAgent := _UA_CHROME;
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
  regx                  :TRegExpr;
  ctry                  :cardinal;
  Sza, rurl, fname,
  sproject, sdir, sfile :string;
begin
  URL := Trim(URL);
  regx := TRegExpr.Create;
  try
    PrepareHTTP(FHTTP);
    regx.ModifierI := True;
    if isSFURL then
    begin
      FHTTP.UserAgent := _UA_CURL;
      regx.Expression := '/download$';
      URL := Trim(regx.Replace(URL, '', False));
      //**parsing SF url
      regx.Expression := '^.*sourceforge.net/projects/(.+)/files/(.+)/([^/]+)$';
      if not regx.Exec(URL) then
      begin
        regx.Free;
        UpdateStatus('Invalid URL');
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

    //**loading page
    UpdateStatus(ST_LoadingPage);
    ctry := 0;
    while (not FHTTP.HTTPMethod('HEAD', rurl)) or
      (FHTTP.ResultCode >= 400) or (FHTTP.ResultCode < 100) do
    begin
      if Self.Terminated then
        Break;
      if (FHTTP.ResultCode >= 400) and (FHTTP.ResultCode < 500) then
      begin
        UpdateStatus(ST_FileNotFound);
        if _UpdApp then
          ShowErrorMessage(ST_FileNotFound + LineEnding + LineEnding +
            ST_Response + ':' + LineEnding +
            IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString)
        else
        begin
          Clipboard.AsText := mf_data_link;
          ShowErrorMessage(ST_FileNotFound_mfdatalink + LineEnding + mf_data_link +
            LineEnding + ST_LinkCopiedToClipboard);
        end;
        Break;
      end;
      if ctry >= MaxRetry then
      begin
        UpdateStatus(ST_FailedLoadPage);
        ShowErrorMessage(ST_FailedLoadPage + LineEnding + LineEnding +
          ST_Response + ':' + LineEnding +
          IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString);
        Break;
      end;
      Inc(ctry);
      UpdateStatus(ST_RetryLoadPage + '(' + IntToStr(ctry) + ')');
      FHTTP.Clear;
    end;

    if (FHTTP.ResultCode > 300) or isSFURL then
    begin
      rurl := HeaderByName(FHTTP.Headers, 'location: ');
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
    UpdateStatus(ST_Download + ' [' + FileName + ']...');
    if (FHTTP.ResultCode >= 100) and (FHTTP.ResultCode < 400) then
    begin
      ctry := 0;
      FHTTP.Clear;
      FHTTP.Headers.Add('Accept: */*');
      FHTTP.Headers.Add('Referer: ' + URL);
      while (not FHTTP.HTTPMethod('GET', rurl)) or
        (FHTTP.ResultCode >= 400) do
      begin
        if Self.Terminated then
          Break;
        if (FHTTP.ResultCode >= 400) and (FHTTP.ResultCode < 500) then         
        begin
          UpdateStatus(ST_FileNotFound);
          if _UpdApp then
            ShowErrorMessage(ST_FileNotFound + LineEnding + LineEnding +
              ST_Response + ':' + LineEnding +
              IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString)
          else
          begin
            Clipboard.AsText := mf_data_link;
            ShowErrorMessage(ST_FileNotFound_mfdatalink + LineEnding + mf_data_link +
              LineEnding + ST_LinkCopiedToClipboard);
          end;
          Break;
        end;
        if ctry >= MaxRetry then
        begin
          UpdateStatus(ST_FailedDownloadPage);
          ShowErrorMessage(ST_AnErrorOccured + LineEnding + LineEnding +
            ST_Response + ':' + LineEnding +
            IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString);
          Break;
        end;
        Inc(ctry);
        UpdateStatus(ST_RetryDownload + ' [' + FileName + ']... ' + '(' +
          IntToStr(ctry) + ')');
        FHTTP.Clear;
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
        UpdateStatus(ST_SaveFile);
        FHTTP.Document.SaveToFile(fname);
      end;
      if not FileExistsUTF8(fname) then
        ShowErrorMessage(ST_ErrorCheckAntiVirus);

      if Extract and FileExistsUTF8(fname) then
      begin
        UpdateStatus(ST_UnpackFile);
        Sza := GetCurrentDirUTF8 + DirectorySeparator + '7za.exe';
        if _UpdApp and
          FileExistsUTF8(GetCurrentDirUTF8 + DirectorySeparator + '7za.exe') then
          begin
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
            {$IFDEF USEADMIN}
            RunAsAdmin(Sza, ' x ' + fname + ' -o' + AnsiQuotedStr(DirPath, '"') +
              ' -aoa', SW_HIDE, True);
            {$ELSE}
            RunExternalProcess(Sza, ['x', fname, '-o' +
              AnsiQuotedStr(DirPath, '"'), '-aoa'], False, False);
            {$ENDIF}
            Sleep(100);
            DeleteFileUTF8(fname);
          end
          else
            ShowErrorMessage(ST_7zNotFound);
        end;
        if FileExistsUTF8(GetCurrentDirUTF8 + DirectorySeparator + 'old_7za.exe') then
          if FileExistsUTF8(GetCurrentDirUTF8 + DirectorySeparator + '7za.exe') then
            DeleteFileUTF8(GetCurrentDirUTF8 + DirectorySeparator + 'old_7za.exe')
          else
            RenameFileUTF8(GetCurrentDirUTF8 + DirectorySeparator + 'old_7za.exe',
              GetCurrentDirUTF8 + DirectorySeparator + '7za.exe');
      end;
    end;
    UpdateStatus(ST_Finished);
    if (not Self.Terminated) and _UpdApp and (_LaunchApp <> '') then
      {$IFDEF USEADMIN}
      RunAsAdmin(_LaunchApp, '');
      {$ELSE}
      RunExternalProcess(_LaunchApp, [''], True, True);
      {$ENDIF}
  except
    on E: Exception do
      frmMain.ExceptionHandler(Self, E);
  end;
  regx.Free;
end;

{ TfrmMain }

procedure TfrmMain.btDownloadClick(Sender :TObject);
begin
  if (not isDownload) and (edURL.Text <> '') then
  begin
    dl := TDownloadThread.Create;
    dl.URL := edURL.Text;
    dl.FileName := edFilename.Text;
    dl.DirPath := GetCurrentDirUTF8;
    dl.Extract := False;
    dl.MaxRetry := 5;
    dl.Start;
    Height := 70;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if isDownload then
  begin
    dl.Terminate;
    dl.WaitFor;
  end;
end;

procedure TfrmMain.FormCreate(Sender :TObject);
var
  config :TIniFile;
begin
  Randomize;
  InitSimpleExceptionHandler(ChangeFileExt(Application.ExeName, '.log'));
  lbStatus.Caption := 'Waiting...';
  lbProgress.Caption := '';
  edURL.Clear;
  edFilename.Clear;

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
          _LaunchApp := ParamStrUTF8(i + 1);
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
      dl.DirPath := GetCurrentDirUTF8;
      if not _UpdApp then
        dl.DirPath := dl.DirPath + DirectorySeparator + 'data';
      dl.Extract := _Extract;
      dl.Start;
      Self.Height := 70;
      itMonitor.Enabled := True;
    end
    else
    begin
      MessageDlg(Application.Title, ST_InvalidURL, mtError, [mbOK], 0);
      Self.Close;
    end;
  end;
end;

procedure TfrmMain.itMonitorTimer(Sender: TObject);
begin
  if not isDownload then
  begin
    itMonitor.Enabled := False;
    Self.Close;
  end;
end;

procedure TfrmMain.ExceptionHandler(Sender : TObject; E : Exception);
begin
  USimpleException.ExceptionHandle(Sender, E);
end;

end.
