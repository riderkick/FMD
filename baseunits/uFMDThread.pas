{
        File: uFMDThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uFMDThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, USimpleLogger;

type

  { TFMDThread }

  TFMDThread = class(TThread)
  private
    FOnCustomTerminate: TNotifyEvent;
    function GetTerminated: Boolean;
  protected
    procedure DoTerminate; override;
  public
    constructor Create(CreateSuspended: Boolean = True);
    property IsTerminated: Boolean read GetTerminated;
    procedure Terminate;
    property OnCustomTerminate: TNotifyEvent read FOnCustomTerminate write FOnCustomTerminate;
  end;

implementation

function TFMDThread.GetTerminated: Boolean;
begin
  Result := Self.Terminated;
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
    FOnCustomTerminate(Self);
end;

end.
