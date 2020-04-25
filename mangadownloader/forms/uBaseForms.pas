unit uBaseForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, Forms;

type

  { TBaseForm }

  TBaseForm = class(TForm)
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TRegisterBaseForm }

  TRegisterBaseForm = class
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
  FormManager: TRegisterBaseForm;

implementation

{ TBaseForm }

constructor TBaseForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FormManager.Add(Self);
end;

destructor TBaseForm.Destroy;
begin
  FormManager.Remove(Self);
  inherited Destroy;
end;

{ TRegisterBaseForm }

constructor TRegisterBaseForm.Create;
begin
  FGuardian := TCriticalSection.Create;
  FList := TList.Create;
end;

destructor TRegisterBaseForm.Destroy;
begin
  Clear;
  FList.Free;
  FGuardian.Free;
  inherited Destroy;
end;

procedure TRegisterBaseForm.Add(AForm: TForm);
begin
  FGuardian.Enter;
  try
    FList.Add(AForm);
  finally
    FGuardian.Leave;
  end;
end;

procedure TRegisterBaseForm.Remove(AForm: TForm);
begin
  FGuardian.Enter;
  try
    FList.Remove(AForm);
  finally
    FGuardian.Leave;
  end;
end;

procedure TRegisterBaseForm.Clear;
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
  FormManager := TRegisterBaseForm.Create;

finalization
  FormManager.Free;

end.

