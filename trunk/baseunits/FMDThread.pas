unit FMDThread;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TFMDThread = class(TThread)
  private
    FIsTerminated,
    FIsSuspended: Boolean;
  public
    property    IsTerminated: Boolean read FIsTerminated write FIsTerminated;
    property    IsSuspended: Boolean read FIsSuspended write FIsSuspended;
  end;

implementation

end.

