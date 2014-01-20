unit uFMDThread;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TFMDThread = class(TThread)
  private
    FIsTerminated,
    FIsSuspended: Boolean;
    procedure   MainThreadSetIsTerminatedTRUE;
    procedure   MainThreadSetIsTerminatedFALSE;

    procedure   SetIsTerminated(ABool: Boolean);
  public
    constructor Create(CreateSuspended: Boolean);
    destructor  Destroy; override;

    property    IsTerminated: Boolean read FIsTerminated write SetIsTerminated;
    property    IsSuspended: Boolean read FIsSuspended write FIsSuspended;
  end;

implementation

procedure   TFMDThread.MainThreadSetIsTerminatedTRUE;
begin
  isTerminated:= TRUE;
end;

procedure   TFMDThread.MainThreadSetIsTerminatedFALSE;
begin
  isTerminated:= FALSE;
end;

// Setters

procedure   TFMDThread.SetIsTerminated(ABool: Boolean);
begin
  if ABool then
    MainThreadSetIsTerminatedTRUE
  else
    MainThreadSetIsTerminatedFALSE;
end;

// Getters

// ----- Public methods -----

constructor TFMDThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(FALSE);
  FIsSuspended := TRUE;
  FIsTerminated:= FALSE;
  FreeOnTerminate:= TRUE;
end;

destructor  TFMDThread.Destroy;
begin
  FIsTerminated:= TRUE;
  inherited Destroy;
end;

end.

