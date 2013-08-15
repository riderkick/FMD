{
        File: iecore.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit iecore;

{$mode delphi}

interface

function  IEGetPage(var output: TObject; URL: String; const Reconnect: Cardinal): Boolean;

implementation

function  IEGetPage(var output: TObject; URL: String; const Reconnect: Cardinal): Boolean;
begin

end;

{uses
  Classes, SysUtils, MSHTML_4_0_TLB, SHDocVw, Variants, ActiveX, activexcontainer, Forms;

const
  GET_FEATURE_FROM_THREAD = $00000001;
  GET_FEATURE_FROM_PROCESS = $00000002;
  GET_FEATURE_FROM_REGISTRY = $00000004;
  GET_FEATURE_FROM_THREAD_LOCALMACHINE = $00000008;
  GET_FEATURE_FROM_THREAD_INTRANET = $00000010;
  GET_FEATURE_FROM_THREAD_TRUSTED = $00000020;
  GET_FEATURE_FROM_THREAD_INTERNET = $00000040;
  GET_FEATURE_FROM_THREAD_RESTRICTED = $00000080;

  SET_FEATURE_ON_THREAD = $00000001;
  SET_FEATURE_ON_PROCESS = $00000002;
  SET_FEATURE_IN_REGISTRY = $00000004;
  SET_FEATURE_ON_THREAD_LOCALMACHINE = $00000008;
  SET_FEATURE_ON_THREAD_INTRANET = $00000010;
  SET_FEATURE_ON_THREAD_TRUSTED = $00000020;
  SET_FEATURE_ON_THREAD_INTERNET = $00000040;
  SET_FEATURE_ON_THREAD_RESTRICTED = $00000080;

type
  INTERNETFEATURELIST = (
    FEATURE_OBJECT_CACHING,
    FEATURE_ZONE_ELEVATION,
    FEATURE_MIME_HANDLING,
    FEATURE_MIME_SNIFFING,
    FEATURE_WINDOW_RESTRICTIONS,
    FEATURE_WEBOC_POPUPMANAGEMENT,
    FEATURE_BEHAVIORS,
    FEATURE_DISABLE_MK_PROTOCOL,
    FEATURE_LOCALMACHINE_LOCKDOWN,
    FEATURE_SECURITYBAND,
    FEATURE_RESTRICT_ACTIVEXINSTALL,
    FEATURE_VALIDATE_NAVIGATE_URL,
    FEATURE_RESTRICT_FILEDOWNLOAD,
    FEATURE_ADDON_MANAGEMENT,
    FEATURE_PROTOCOL_LOCKDOWN,
    FEATURE_HTTP_USERNAME_PASSWORD_DISABLE,
    FEATURE_SAFE_BINDTOOBJECT,
    FEATURE_UNC_SAVEDFILECHECK,
    FEATURE_GET_URL_DOM_FILEPATH_UNENCODED,
    FEATURE_TABBED_BROWSING,
    FEATURE_SSLUX,
    FEATURE_DISABLE_NAVIGATION_SOUNDS,
    FEATURE_DISABLE_LEGACY_COMPRESSION,
    FEATURE_FORCE_ADDR_AND_STATUS,
    FEATURE_XMLHTTP,
    FEATURE_DISABLE_TELNET_PROTOCOL,
    FEATURE_FEEDS,
    FEATURE_BLOCK_INPUT_PROMPTS,
    FEATURE_ENTRY_COUNT
  );

type
  TIECore = class
  private
    procedure   OnDocumentComplete(Sender: TObject; pDisp: IDispatch; var URL: OleVariant);
  public
    isLoadComplete: Boolean;
    Browser       : TWebBrowser;
    constructor Create(AOwner: TComponent);
    destructor  Destroy; override;

    procedure   Load(const URL: String);
    function    GetHTMLSource: String;
  end;

function  IEGetPage(var output: TObject; URL: String; const Reconnect: Cardinal): Boolean;
function  CoInternetSetFeatureEnabled(FeatureEntry: INTERNETFEATURELIST; dwFlags: DWORD; fEnable: Boolean): HRESULT; stdcall; external 'urlmon.dll';

implementation

uses
  LogForm, mainunit, baseunit;

// reconnect is not working yet
function  IEGetPage(var output: TObject; URL: String; const Reconnect: Cardinal): Boolean;
var
  Saved8087CW: Word;
  Core    : TIECore;
  rnd     : Cardinal;
  rndS    : String;
  retry   : Cardinal = 0;
  baseTime: Cardinal;
begin
  Core:= TIECore.Create(LogForm.Log.pnIE);

  CoInternetSetFeatureEnabled(FEATURE_DISABLE_NAVIGATION_SOUNDS, SET_FEATURE_ON_PROCESS, TRUE);

  baseTime:= fmdGetTickCount;

  Saved8087CW:= Get8087CW;

  Set8087CW($133F);
  Core.Load(URL);

 // while NOT Core.isLoadComplete do
  while (Core.Browser.ReadyState <> READYSTATE_COMPLETE) do
  begin
    Sleep(16);
    Application.ProcessMessages;
    if fmdGetTickCount-baseTime > 30000 then
    begin
      break;
    end;
  end;
  Core.Browser.Stop;
  Set8087CW(Saved8087CW);
  if output is TStringList then
  begin
    rnd:= Random(4000000);
    rndS:= ExtractFilePath(ParamStr(0)) + 'fmd_' + IntToStr(rnd) + '.tmp';
    TStringList(output).Text:= Core.GetHTMLSource;
    TStringList(output).SaveToFile(rndS);
    TStringList(output).Clear;
    TStringList(output).LoadFromFile(rndS);
    DeleteFile(rndS);
  end;
  Core.Free;
  CoInternetSetFeatureEnabled(FEATURE_DISABLE_NAVIGATION_SOUNDS, SET_FEATURE_ON_PROCESS, FALSE);
  Result:= TRUE;
end;

procedure   TIECore.OnDocumentComplete(Sender: TObject; pDisp: IDispatch; var URL: OleVariant);
begin
  isLoadComplete:= TRUE;
end;

constructor TIECore.Create(AOwner: TComponent);
begin
  inherited Create;
  Browser:= TWebBrowser.Create(AOwner);
  Browser.Silent:= TRUE;
  Browser.UseDockManager:= FALSE;
  Browser.OnDocumentComplete:= OnDocumentComplete;
  isLoadComplete:= FALSE;
end;

destructor  TIECore.Destroy;
begin
  Browser.Free;
  inherited Destroy;
end;

procedure   TIECore.Load(const URL: String);
begin
  Browser.Navigate(URL);
  isLoadComplete:= FALSE;
end;

function    TIECore.GetHTMLSource: String;
var
  iall: IHTMLElement;
begin
  iall:= (Browser.Document as IHTMLDocument2).body;
  while iall.parentElement <> nil do
  begin
    iall:= iall.parentElement;
  end;
  Result:= iall.outerHTML;
end; }

end.

