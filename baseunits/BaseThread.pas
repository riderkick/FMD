unit BaseThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TBaseThread }

  TBaseThread = class(TThread)
  private
    FOnCustomTerminate: TNotifyEvent;
    function GetTerminated: Boolean;
    procedure CallOnCustomTerminate; inline;
  public
    constructor Create(CreateSuspended: Boolean = True);
    destructor Destroy; override;
    procedure Terminate;
    property IsTerminated: Boolean read GetTerminated;
    property OnCustomTerminate: TNotifyEvent read FOnCustomTerminate write FOnCustomTerminate;
  end;

implementation

{ TBaseThread }

function TBaseThread.GetTerminated: Boolean;
begin
  Result := Self.Terminated;
end;

procedure TBaseThread.CallOnCustomTerminate;
begin
  FOnCustomTerminate(Self);
end;

constructor TBaseThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

destructor TBaseThread.Destroy;
begin
  inherited Destroy;
end;

procedure TBaseThread.Terminate;
begin
  inherited Terminate;
  if Assigned(FOnCustomTerminate) then
    FOnCustomTerminate(Self);
end;

end.

