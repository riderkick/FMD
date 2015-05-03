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

  { TFMDThread }

  TFMDThread = class(TThread)
  protected
    function GetTerminated: Boolean;
    procedure DoTerminate; override;
  public
    constructor Create(CreateSuspended: Boolean = True);
    property IsTerminated: Boolean read GetTerminated;
  end;

implementation

uses frmMain;

function TFMDThread.GetTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TFMDThread.DoTerminate;
begin
  if (FatalException <> nil) and (FatalException is Exception) then
  begin
    Exception(FatalException).Message :=
      'FatalException, ' + Exception(FatalException).Message;
    MainForm.ExceptionHandler(Self, Exception(FatalException));
  end;
  inherited DoTerminate;
end;

// ----- Public methods -----

constructor TFMDThread.Create(CreateSuspended: Boolean = True);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

end.
