{
        File: uSubThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uSubThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, Forms, uBaseUnit, uFMDThread;

type

  { TSubThread }

  TSubThread = class(TFMDThread)
  protected
    procedure Execute; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  frmMain;

{ TSubThread }

procedure TSubThread.Execute;
begin
  MainForm.isSubthread := True;
  try
    while not Terminated do
    begin
      with MainForm do
      begin
        while (SilentThreadManager.MetaData.Count > 0) and
          (SilentThreadManager.Threads.Count < DLManager.maxDLThreadsPerTask) do
          SilentThreadManager.CheckOut;
      end;
      Sleep(500);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

destructor TSubThread.Destroy;
begin
  MainForm.isSubthread := False;
  inherited Destroy;
end;

end.
