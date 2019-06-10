unit WebsiteModulesSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
    TProxyType = (ptDefault, ptDirect, ptHTTP, ptSOCKS4, ptSOCKS5);

  { TProxySettings }

  TProxySettings = class
  private
    FProxyHost: String;
    FProxyPassword: String;
    FProxyPort: String;
    FProxyType: TProxyType;
    FProxyUsername: String;
  published
    property ProxyType: TProxyType read FProxyType write FProxyType default ptDefault;
    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: String read FProxyPort write FProxyPort;
    property ProxyUsername: String read FProxyUsername write FProxyUsername;
    property ProxyPassword: String read FProxyPassword write FProxyPassword;
  end;

  { THTTPSettings }

  THTTPSettings = class
  private
    FCookies: String;
    FProxy: TProxySettings;
    FUserAgent: String;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Cookies: String read FCookies write FCookies;
    property UserAgent: String read FUserAgent write FUserAgent;
    property Proxy: TProxySettings read FProxy write FProxy;
  end;
  
  { TCloudflareBypassSettings }
  
  TCloudflareBypass = class
  private
    FDisableCloudflareBypass: Boolean;
    FCloudflareCookies: String;
    FCloudflareUserAgent: String;
    FBypassLock: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property BypassLock: Boolean read FBypassLock write FBypassLock;
  published
    property DisableCloudflareBypass: Boolean read FDisableCloudflareBypass write FDisableCloudflareBypass;
    property Cookies: String read FCloudflareCookies write FCloudflareCookies;
    property UserAgent: String read FCloudflareUserAgent write FCloudflareUserAgent;
  end;

  { TWebsiteModuleSettings }

  TWebsiteModuleSettings = class
  private
    FEnabled: Boolean;
    FHTTP: THTTPSettings;
    FCloudflareBypass: TCloudflareBypass;
    FMaxConnectionLimit: Integer;
    FMaxTaskLimit: Integer;
    FMaxThreadPerTaskLimit: Integer;
    FUpdateListDirectoryPageNumber: Integer;
    FUpdateListNumberOfThread: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property MaxTaskLimit: Integer read FMaxTaskLimit write FMaxTaskLimit default 0;
    property MaxThreadPerTaskLimit: Integer read FMaxThreadPerTaskLimit write FMaxThreadPerTaskLimit default 0;
    property MaxConnectionLimit: Integer read FMaxConnectionLimit write FMaxConnectionLimit default 0;
    property UpdateListNumberOfThread: Integer read FUpdateListNumberOfThread write FUpdateListNumberOfThread default 0;
    property UpdateListDirectoryPageNumber: Integer read FUpdateListDirectoryPageNumber write FUpdateListDirectoryPageNumber default 0;
    property HTTP: THTTPSettings read FHTTP write FHTTP;
    property CloudflareBypass: TCloudflareBypass read FCloudflareBypass write FCloudflareBypass;
  end;

implementation

{ THTTPSettings }

constructor THTTPSettings.Create;
begin
  Proxy:=TProxySettings.Create;
end;

destructor THTTPSettings.Destroy;
begin
  Proxy.Free;
  inherited Destroy;
end;

{ TCloudflareBypass }

constructor TCloudflareBypass.Create;
begin
  
end;

destructor TCloudflareBypass.Destroy;
begin
  inherited Destroy;
end;

{ TWebsiteModuleSettings }

constructor TWebsiteModuleSettings.Create;
begin
  HTTP:=THTTPSettings.Create;
  CloudflareBypass:=TCloudflareBypass.Create;
end;

destructor TWebsiteModuleSettings.Destroy;
begin
  HTTP.Free;
  CloudflareBypass.Free;
  inherited Destroy;
end;

end.

