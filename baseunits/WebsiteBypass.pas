unit WebsiteBypass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsendthread;

type

  { TWebsiteBypass }

  TWebsiteBypass = class
  public
    WebsiteModule: TObject;
    Guardian: TRTLCriticalSection;
    constructor Create(AWebsiteModule: TObject);
    destructor Destroy; override;
  end;

function WebsiteBypassRequest(const AHTTP: THTTPSendThread; const Method, AURL: String; const Response: TObject; const AWebsiteBypass: TWebsiteBypass): Boolean;

implementation

uses
  FMDOptions, WebsiteModules, LuaWebsiteModules, lua53, LuaBase, LuaHTTPSend, LuaUtils,
  MultiLog;

var
  luadump_websitebypass_checkantibot,
  luadump_websitebypass: TMemoryStream;

  luafile_websitebypass_checkantibot,
  luafile_websitebypass: String;

procedure doInit;
begin
  luafile_websitebypass_checkantibot := LUA_REPO_FOLDER + 'websitebypass' + PathDelim + 'checkantibot.lua';
  luafile_websitebypass := LUA_REPO_FOLDER + 'websitebypass' + PathDelim + 'websitebypadd.lua';

  if FileExists(luafile_websitebypass_checkantibot) then
    luadump_websitebypass_checkantibot := LuaDumpFileToStream(luafile_websitebypass_checkantibot);
  if FileExists(luafile_websitebypass) then
    luadump_websitebypass := LuaDumpFileToStream(luafile_websitebypass);
end;

procedure doFinal;
begin
  if Assigned(luadump_websitebypass_checkantibot) then
    luadump_websitebypass_checkantibot.Free;
  if Assigned(luadump_websitebypass) then
    luadump_websitebypass.Free;
end;

function CheckAntiBotActive(const AHTTP: THTTPSendThread): Boolean;
var
  L: Plua_State;
begin
  Result := False;
  if not Assigned(luadump_websitebypass_checkantibot) then Exit;
  L := luaL_newstate;
  try
    luaL_openlibs(L);
    LuaBaseRegisterPrint(L);
    luaPushObject(L, AHTTP, 'http', @luaHTTPSendThreadAddMetaTable);
    LuaExecute(L, luadump_websitebypass_checkantibot, luafile_websitebypass_checkantibot, LUA_MULTRET);
    Result := lua_toboolean(L, -1);
  except
    on E: Exception do
      Logger.SendError(E.Message + ': ' + luaGetString(L, -1));
  end;
  lua_close(L);
end;

function WebsiteBypassGetAnswer(const AHTTP: THTTPSendThread; AURL: String; const AWebsiteBypass: TWebsiteBypass): Boolean;
var
  L: Plua_State;
begin
  Result := False;
  if not Assigned(luadump_websitebypass) then Exit;
  L := LuaNewBaseState;
  try
    luaPushObject(L, AHTTP, 'http', @luaHTTPSendThreadAddMetaTable);
    luaPushObject(L, TModuleContainer(AWebsiteBypass.WebsiteModule), 'module', @luaWebsiteModuleAddMetaTable);
    LuaExecute(L, luadump_websitebypass, luafile_websitebypass, LUA_MULTRET);
    Result := lua_toboolean(L, -1);
  except
    on E: Exception do
      Logger.SendError(E.Message + ': ' + luaGetString(L, -1));
  end;
  lua_close(L);
end;

function WebsiteBypassRequest(const AHTTP: THTTPSendThread; const Method, AURL: String; const Response: TObject; const AWebsiteBypass: TWebsiteBypass): Boolean;
begin
  Result := False;
  if AHTTP = nil then Exit;
  AHTTP.AllowServerErrorResponse := True;
  Result := AHTTP.HTTPRequest(Method, AURL);
  if CheckAntiBotActive(AHTTP) then begin
    if TryEnterCriticalsection(AWebsiteBypass.Guardian) > 0 then
      try
        Result := WebsiteBypassGetAnswer(AHTTP, AURL, AWebsiteBypass);
      finally
        LeaveCriticalsection(AWebsiteBypass.Guardian);
      end
    else begin
      if not AHTTP.ThreadTerminated then
        Result := AHTTP.HTTPRequest(Method, AURL);
    end;
  end;
  if Assigned(Response) then
    if Response is TStringList then
      TStringList(Response).LoadFromStream(AHTTP.Document)
    else
    if Response is TStream then
      AHTTP.Document.SaveToStream(TStream(Response));
end;

{ TWebsiteBypass }

constructor TWebsiteBypass.Create(AWebsiteModule: TObject);
begin
  WebsiteModule:=AWebsiteModule;
  InitCriticalSection(Guardian);
end;

destructor TWebsiteBypass.Destroy;
begin
  DoneCriticalsection(Guardian);
  inherited Destroy;
end;

initialization
  doInit;

finalization
  doFinal;

end.
