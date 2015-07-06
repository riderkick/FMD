unit frmDropTarget;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, ActiveX, comobj, Forms, Controls,
  ExtCtrls, Menus, LCLType, DefaultTranslator;

type

  { TFormDropTarget }

  TFormDropTarget = class(TForm, IDropTarget)
    ImResize: TImage;
    ImDropIcon: TImage;
    miClose: TMenuItem;
    pmDropTarget: TPopupMenu;
    shBorder: TShape;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure ImResizeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure miCloseClick(Sender: TObject);
  private
    { private declarations }
    md: Boolean;
    x0, y0: Integer;
    // IDropTarget
    function DragEnter(const dataObj: IDataObject; grfKeyState: DWORD;
      pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
    function {%H-}DragOver(grfKeyState: DWORD; pt: TPoint;
      var dwEffect: DWORD): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: DWORD;
      pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { public declarations }
  end;

var
  FormDropTarget: TFormDropTarget;
  FAlphaBlendValue: Integer = 255;
  FWidth: Integer = 64;
  FHeight: Integer = 64;
  FLeft: Integer = -1;
  FTop: Integer = -1;
  OnDropChekout: procedure(S: String) of object;

implementation

uses
  frmMain;

{$R *.lfm}

{ TFormDropTarget }

procedure TFormDropTarget.FormCreate(Sender: TObject);
begin
  ShowWindow(Self.Handle, SW_HIDE);
  SetWindowLong(Self.Handle, GWL_EXSTYLE, getWindowLong(Self.Handle,
    GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
  ShowWindow(Self.Handle, SW_SHOW);
  OleInitialize(nil);
  OleCheck(RegisterDragDrop(Handle, Self));
  AlphaBlend := True;
end;

procedure TFormDropTarget.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  MainForm.SaveDropTargetFormInformation;
  CloseAction := caFree;
end;

procedure TFormDropTarget.FormDestroy(Sender: TObject);
begin
  RevokeDragDrop(Handle);
  OleUninitialize;
  FormDropTarget := nil;
  MainForm.ckDropTarget.Checked := False;
end;

procedure TFormDropTarget.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    md := True;
    x0 := X;
    y0 := Y;
  end;
end;

procedure TFormDropTarget.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if md then
  begin
    Left := Left - x0 + X;
    Top := Top - y0 + Y;
    FLeft := Left;
    FTop := Top;
  end;
end;

procedure TFormDropTarget.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  md := False;
end;

procedure TFormDropTarget.FormShow(Sender: TObject);
begin
  AlphaBlendValue := FAlphaBlendValue;
  Width := FWidth;
  Height := FHeight;
  if FLeft = -1 then
    FLeft := Screen.WorkAreaWidth - Width - 15;
  if FTop = -1 then
    FTop := Screen.WorkAreaHeight - Height - 15;
  Left := FLeft;
  Top := FTop;
end;

procedure TFormDropTarget.ImResizeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if md then
  begin
    Width := Width + X - x0;
    Height := Height + Y - y0;
    FWidth := Width;
    FHeight := Height;
  end;
end;

procedure TFormDropTarget.miCloseClick(Sender: TObject);
begin
  Self.Close;
end;

function TFormDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: DWORD;
  pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
begin
  dwEffect := DROPEFFECT_COPY;
  Result := S_OK;
end;

function TFormDropTarget.DragOver(grfKeyState: DWORD; pt: TPoint;
  var dwEffect: DWORD): HResult; stdcall;
begin
  dwEffect := DROPEFFECT_COPY;
  Result := S_OK;
end;

function TFormDropTarget.DragLeave: HResult; stdcall;
begin
  Result := S_OK;
end;

function TFormDropTarget.Drop(const dataObj: IDataObject; grfKeyState: DWORD;
  pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
var
  aFmtEtc: TFORMATETC;
  aStgMed: TSTGMEDIUM;
  pData: PChar;
begin
  // Support dropping text on edit control
  // Make certain the data rendering is available
  if (dataObj = nil) then
    raise Exception.Create('IDataObject Pointer is not valid!');
  with aFmtEtc do
  begin
    cfFormat := CF_TEXT;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  // Get the data
  OleCheck(dataObj.GetData(aFmtEtc, aStgMed));
  try
    // Lock the global memory handle to get a pointer to the data
    pData := GlobalLock(aStgMed.hGlobal);
    // Replace Text in the control you want
    if Assigned(OnDropChekout) then
      OnDropChekout(pData);
  finally
    // Finished with the pointer
    GlobalUnlock(aStgMed.hGlobal);
    // Free the memory
    ReleaseStgMedium(aStgMed);
  end;
  Result := S_OK;
end;

procedure TFormDropTarget.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle and WS_EX_TOOLWINDOW and (not WS_EX_APPWINDOW);
  Params.WndParent := GetDesktopWindow;
end;

end.
