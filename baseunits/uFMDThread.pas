{
        File: uFMDThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uFMDThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, SimpleLogger;

type

  { TFMDThread }

  TFMDThread = class(TThread)
  private
    FOnCustomTerminate: TNotifyEvent;
    function GetTerminated: Boolean;
    procedure CallCustomTerminate;
  protected
    procedure DoTerminate; override;
  public
    constructor Create(CreateSuspended: Boolean = True);
    procedure Terminate;
    property IsTerminated: Boolean read GetTerminated;
    property OnCustomTerminate: TNotifyEvent read FOnCustomTerminate write FOnCustomTerminate;
  end;

implementation

function TFMDThread.GetTerminated: Boolean;
begin
  Result := Self.Terminated;
end;

procedure TFMDThread.CallCustomTerminate;
begin
  FOnCustomTerminate(Self);
end;

procedure TFMDThread.DoTerminate;
begin
  if (FatalException <> nil) and (FatalException is Exception) then
    WriteLog_E('TFMDThread.FatalException!', Exception(FatalException), Self);
  inherited DoTerminate;
end;

constructor TFMDThread.Create(CreateSuspended: Boolean = True);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TFMDThread.Terminate;
begin
  inherited Terminate;
  if Assigned(FOnCustomTerminate) then
    Synchronize(CallCustomTerminate);
end;

end.
