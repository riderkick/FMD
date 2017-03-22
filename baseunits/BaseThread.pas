unit BaseThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TBaseThread }

  TBaseThread = class(TThread)
  private
    FObjectList: TFPList;
    FOnCustomTerminate: TNotifyEvent;
    function GetTerminated: Boolean;
    procedure CallOnCustomTerminate; inline;
  public
    constructor Create(CreateSuspended: Boolean = True);
    destructor Destroy; override;
    procedure Terminate;
    property IsTerminated: Boolean read GetTerminated;
    property OnCustomTerminate: TNotifyEvent read FOnCustomTerminate write FOnCustomTerminate;
    property ObjectList: TFPList read FObjectList; // Object to be freed on Destroy
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
  FObjectList := TFPList.Create;
end;

destructor TBaseThread.Destroy;
var
  i: Integer;
begin
  if FObjectList.Count <> 0 then
    for i := 0 to FObjectList.Count - 1 do
      if Assigned(FObjectList[i]) then
        try
          TObject(FObjectList[i]).Free;
        except
        end;
  FObjectList.Free;
  inherited Destroy;
end;

procedure TBaseThread.Terminate;
begin
  inherited Terminate;
  if Assigned(FOnCustomTerminate) then
    FOnCustomTerminate(Self);
end;

end.

