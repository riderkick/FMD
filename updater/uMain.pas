unit uMain;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}
  cthreads,
  cmem,
  {$endif}
  Classes, SysUtils, zipper, FileUtil, Forms,
  Dialogs, ComCtrls, StdCtrls, Clipbrd, USimpleException, httpsend, blcksock;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btDownload :TButton;
    edFilename :TEdit;
    edURL      :TEdit;
    lbFilename :TLabel;
    lbURL      :TLabel;
    lbStatus   :TLabel;
    lbProgress :TLabel;
    pbDownload :TProgressBar;
    procedure btDownloadClick(Sender :TObject);
    procedure FormCreate(Sender :TObject);
    procedure FormShow(Sender :TObject);
  private
    { private declarations }
  public
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    { public declarations }
  end;

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
    procedure Execute; override;
  public
    URL      :string;
    FileName :string;
    DirPath  :string;
    MaxRetry :cardinal;
    Unzip    :boolean;
    constructor Create;
    destructor Destroy; override;
  end;

function HeaderByName(const AHeaders :TStrings; const HeaderName :string) :string;

var
  frmMain    :TfrmMain;
  dl         :TDownloadThread;
  isDownload :boolean = False;

  ProxyType :string;
  ProxyHost :string;
  ProxyPort :string;
  ProxyUser :string;
  ProxyPass :string;

const
  mf_data_link = 'https://www.mediafire.com/folder/fwa8eomz80uk1/Data';

resourcestring
  ST_AnErrorOccured = 'An error occured when trying to download file.';
  ST_FileNotFound = 'File not found!';
  ST_LoadingPage = 'Loading page...';
  ST_FailedLoadPage = 'Failed loading page!';
  ST_RetryLoadPage = 'Retry loading page... ';
  ST_Redirected = 'Redirected...';
  ST_Download = 'Downloading';
  ST_FailedDownloadPage = 'Failed downloading file!';
  ST_RetryDownload = 'Retry downloading';
  ST_SaveFile = 'Saving file... ';
  ST_UnpackFile = 'Unpacking file';
  ST_FileNotFound_mfdatalink =
          'File not found!' + LineEnding +
          'This site probably have been added to unofficial build.' + LineEnding +
          LineEnding +
          'Remedy:' + LineEnding +
          'Build you manga list from scratch or download manually from this link:';
  ST_LinkCopiedToClipboard = 'Link copied to clipboard!';

implementation

uses RegExpr, IniFiles;

{$R *.lfm}

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
    Result := FormatFloat('#.## bytes', bytes);
end;

{  TDownloadThread }

constructor TDownloadThread.Create;
begin
  isDownload := True;
  inherited Create(True);
  FreeOnTerminate := True;
  FHTTP := THTTPSend.Create;
  FHTTP.Sock.OnHeartbeat := @SockOnHeartBeat;
  FHTTP.Sock.HeartbeatRate := 500;
  FTotalSize := 0;
  FCurrentSize := 0;
  URL := '';
  FileName := '';
  DirPath := '';
  MaxRetry := 1;
  Unzip := False;
end;

destructor TDownloadThread.Destroy;
begin
  FHTTP.Free;
  isDownload := False;
  inherited Destroy;
  Application.Terminate;
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

procedure TDownloadThread.SockOnHeartBeat(Sender :TObject);
begin
  if Self.Terminated then
    (Sender as TBlockSocket).AbortSocket;
end;

procedure TDownloadThread.MainThreadUpdateProgress;
begin
  if (FCurrentSize > 0) and (FTotalSize > 0) then
  begin
    try
      frmMain.pbDownload.Position := Round(1000 * (FCurrentSize / FTotalSize));
    finally
      frmMain.lbProgress.Caption :=
        FormatByteSize(FCurrentSize) + ' / ' + FormatByteSize(FTotalSize);
    end;
  end
  else
  begin
    frmMain.pbDownload.Position := 0;
    frmMain.lbProgress.Caption := '0 / 0';
  end;
end;

procedure TDownloadThread.MainThreadErrorGetting;
begin
  MessageDlg('Error', FErrorMessage,
    mtError, [mbOK], 0);
end;

procedure TDownloadThread.SockOnStatus(Sender :TObject; Reason :THookSocketReason;
  const Value :string);
begin
  case Reason of
    HR_CanRead:
    begin
      if FHTTP.Headers.Count > 0 then
        FTotalSize := StrToIntDef(HeaderByName(FHTTP.Headers, 'Content-Length'), 0);
    end;
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
      FHTTP.UserAgent :=
        'curl/7.21.0 (i686-pc-linux-gnu) libcurl/7.21.0 OpenSSL/0.9.8o zlib/1.2.3.4 libidn/1.18';
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
  found                 :boolean = False;
  rurl, furl            :string;
  sproject, sdir, sfile :string;
label
  loadp;
begin
  URL := Trim(URL);
  regx := TRegExpr.Create;
  try
    regx.ModifierI := True;
    regx.Expression := '/download$';
    URL := Trim(regx.Replace(URL, '', False));

    //parsing SF url
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

    PrepareHTTP(FHTTP);
    ctry := 0;

    //loading page
    UpdateStatus(ST_LoadingPage);

    loadp:
      //retry loading page
      while (not FHTTP.HTTPMethod('GET', rurl)) or
      (FHTTP.ResultCode >= 500) or
      (FHTTP.ResultCode >= 400) do
      begin
        if FHTTP.ResultCode >= 400 then
        begin
          regx.Free;
          UpdateStatus(ST_FileNotFound);
          FErrorMessage := ST_FileNotFound_mfdatalink + LineEnding + mf_data_link +
            LineEnding + ST_LinkCopiedToClipboard;
          Clipboard.AsText := mf_data_link;
          Synchronize(@MainThreadErrorGetting);
          Exit;
        end;
        if ctry >= MaxRetry then
        begin
          regx.Free;
          UpdateStatus(ST_FailedLoadPage);
          FErrorMessage := ST_AnErrorOccured;
          Synchronize(@MainThreadErrorGetting);
          Exit;
        end;
        Inc(ctry);
        UpdateStatus(ST_RetryLoadPage + '(' + IntToStr(ctry) + ')');
        FHTTP.Clear;
      end;

    while (FHTTP.ResultCode = 301) or
      (FHTTP.ResultCode = 302) or
      (FHTTP.ResultCode = 307) do
    begin
      rurl := HeaderByName(FHTTP.Headers, 'location: ');
      if (Pos('use_mirror=', rurl) > 0) then
      begin
        found := True;
        Break;
      end
      else
      begin
        UpdateStatus(ST_Redirected);
        FHTTP.Clear;
        FHTTP.HTTPMethod('GET', rurl);
      end;
    end;

    if found then
    begin
      regx.Expression := '.*use_mirror=(.+)$';
      furl := regx.Replace(rurl, '$1', True);
      furl := 'http://' + furl + '.dl.sourceforge.net/project/' +
        sproject + '/' + sdir + '/' + sfile;
    end;

    UpdateStatus(ST_Download + ' [' + FileName + ']...');
    FHTTP.Sock.OnStatus := @SockOnStatus;

    //download file
    ctry := 0;
    FHTTP.Clear;
    //FHTTP.Headers.Add('Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8');
    FHTTP.Headers.Add('Accept: */*');
    FHTTP.Headers.Add('Referer: ' + URL);

    while (not FHTTP.HTTPMethod('GET', furl)) or
      (FHTTP.ResultCode >= 500) or
      (FHTTP.ResultCode >= 400) do
    begin
      if FHTTP.ResultCode >= 400 then
      begin
        regx.Free;
        UpdateStatus(ST_FileNotFound);
        FErrorMessage := ST_FileNotFound_mfdatalink + LineEnding + mf_data_link +
          LineEnding + ST_LinkCopiedToClipboard;
        Clipboard.AsText := mf_data_link;
        Synchronize(@MainThreadErrorGetting);
        Exit;
      end;
      if ctry >= MaxRetry then
      begin
        regx.Free;
        UpdateStatus(ST_FailedDownloadPage);
        FErrorMessage := ST_AnErrorOccured;
        Synchronize(@MainThreadErrorGetting);
        Exit;
      end;
      Inc(ctry);
      UpdateStatus(ST_RetryDownload + ' [' + FileName + ']... ' + '(' +
        IntToStr(ctry) + ')');
      FHTTP.Clear;
    end;

    if FileExistsUTF8(DirPath + DirectorySeparator + FileName) then
      DeleteFileUTF8(DirPath + DirectorySeparator + FileName);

    if not DirectoryExistsUTF8(DirPath) then
      CreateDirUTF8(DirPath);
    if DirectoryExistsUTF8(DirPath) then
    begin
      UpdateStatus(ST_SaveFile);
      FHTTP.Document.SaveToFile(DirPath + DirectorySeparator + FileName);
    end;

    if Unzip {and (Pos('.zip', LowerCase(FileName)) > 0)} then
    begin
      UpdateStatus(ST_UnpackFile);
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
    end;
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
    dl.Unzip := False;
    dl.MaxRetry := 5;
    dl.Start;
    Height := 70;
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
begin
  if Paramcount > 0 then
  begin
    if LowerCase(ParamStrUTF8(1)) = 'data' then
    begin
      if ParamStrUTF8(2) <> '' then
      begin
        dl := TDownloadThread.Create;
        dl.URL := ParamStrUTF8(2);
        dl.DirPath := GetCurrentDirUTF8 + DirectorySeparator + 'data';
        dl.Unzip := True;
        if ParamStrUTF8(3) <> '' then
          dl.MaxRetry := StrToIntDef(ParamStrUTF8(3), 1);
        dl.Start;
        Height := 70;
      end;
    end
    else
    if LowerCase(ParamStrUTF8(1)) = 'update' then
    begin
      //for update
    end;
  end;
end;

procedure TfrmMain.ExceptionHandler(Sender : TObject; E : Exception);
begin
  USimpleException.ExceptionHandle(Sender, E);
end;

end.
