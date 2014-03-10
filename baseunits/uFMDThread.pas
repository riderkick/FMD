{
        File: uFMDThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uFMDThread;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TFMDThread = class(TThread)
  private
    FIsTerminateCalled,
    FIsTerminated,
    FIsSuspended: Boolean;
    procedure   MainThreadSetIsTerminatedTRUE;
    procedure   MainThreadSetIsTerminatedFALSE;

    procedure   SetIsTerminated(ABool: Boolean);
  public
    constructor Create(CreateSuspended: Boolean);
    destructor  Destroy; override;

    procedure   Terminate;

    property    IsTerminated: Boolean read FIsTerminated write FIsTerminated;
    property    IsSuspended: Boolean read FIsSuspended write FIsSuspended;
    property    IsTerminateCalled: Boolean read FIsTerminateCalled write FIsTerminateCalled;
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
    Synchronize(MainThreadSetIsTerminatedTRUE)
  else
    Synchronize(MainThreadSetIsTerminatedFALSE);
end;

// Getters

// ----- Public methods -----

constructor TFMDThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(FALSE);
  FIsSuspended := TRUE;
  FIsTerminated:= FALSE;
  FreeOnTerminate:= TRUE;
  FIsTerminateCalled:= FALSE
end;

destructor  TFMDThread.Destroy;
begin
  FIsTerminated:= TRUE;
  inherited Destroy;
end;

procedure   TFMDThread.Terminate;
begin
  FIsTerminateCalled:= TRUE;
  TThread(self).Terminate;
end;

end.

