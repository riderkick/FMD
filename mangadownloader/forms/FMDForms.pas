unit FMDForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, Forms;

type

  { TFMDForm }

  TFMDForm = class(TForm)
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TFormManager }

  TFormManager = class
  private
    FList: TList;
    FGuardian: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AForm: TForm);
    procedure Remove(AForm: TForm);
    procedure Clear;
  end;

var
  FormManager: TFormManager;

implementation

{ TFMDForm }

constructor TFMDForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FormManager.Add(Self);
end;

destructor TFMDForm.Destroy;
begin
  FormManager.Remove(Self);
  inherited Destroy;
end;

{ TFormManager }

constructor TFormManager.Create;
begin
  FGuardian := TCriticalSection.Create;
  FList := TList.Create;
end;

destructor TFormManager.Destroy;
begin
  Clear;
  FList.Free;
  FGuardian.Free;
  inherited Destroy;
end;

procedure TFormManager.Add(AForm: TForm);
begin
  FGuardian.Enter;
  try
    FList.Add(AForm);
  finally
    FGuardian.Leave;
  end;
end;

procedure TFormManager.Remove(AForm: TForm);
begin
  FGuardian.Enter;
  try
    FList.Remove(AForm);
  finally
    FGuardian.Leave;
  end;
end;

procedure TFormManager.Clear;
var
  f: Pointer;
begin
  for f in FList do
  begin
    TForm(f).Close;
  end;
  FList.Clear;
end;

initialization
  FormManager := TFormManager.Create;

finalization
  FormManager.Free;

end.

