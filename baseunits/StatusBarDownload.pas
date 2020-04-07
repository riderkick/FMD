unit StatusBarDownload;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseThread, httpsendthread, blcksock, ExtCtrls, Forms,
  Controls, Buttons, Graphics, ComCtrls;

type

  { TStatusBarDownload }

  TStatusBarDownload = class(TBaseThread)
  private
    FOwnerForm: TForm;
    FImageList: TImageList;
    FStatusBar: TPanel;
    FButtonCancel: TSpeedButton;
    FButtonCancelImageIndex: Integer;

    FControlMargin: Integer;
    FResized: Boolean;
    FProgressBarRect,
    FProgressBarPercentsRect,
    FStatusTextRect: TRect;
    FProgressText,
    FStatusText: String;

    FTimerRepaint: TTimer;
    FNeedRepaint: Boolean;

    FHTTP: THTTPSendThread;
    FTotalSize,
    FCurrentSize: Integer;
    FPercents: Double;
  protected
    procedure SyncCreate;
    procedure SyncDestroy;
    procedure TimerRepaintTimer(Sender: TObject);
    procedure StatusBarPaint(Sender: TObject);
    procedure StatusBarRezise(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure HTTPSockOnStatus(Sender: TObject; Reason: THookSocketReason;
      const Value: String);
    procedure UpdateStatusText(AStatusText: String);
  public
    constructor Create(CreateSuspended: Boolean = True; AOwnerForm: TForm = nil;
      AImageList: TImageList = nil; AButtonCancelImageIndex: Integer = -1);
    destructor Destroy; override;
  published
    property HTTP: THTTPSendThread read FHTTP;
    property StatusBar: TPanel read FStatusBar;
  end;

implementation

uses uBaseUnit;

const
  CL_BarGrayLine        = $bcbcbc;
  CL_BarGray            = $e6e6e6;

  CL_BarGreenLine       = $25b006;
  CL_BarGreen           = $42d932;

{ TStatusBarDownload }

procedure TStatusBarDownload.SyncCreate;
begin
  FControlMargin := FOwnerForm.ScaleFontTo96(2);
  FStatusBar := TPanel.Create(nil);
  with FStatusBar do begin
    Parent := FOwnerForm;
    DoubleBuffered := True;
    Align := alBottom;
    AutoSize := False;
    Height := ScaleFontTo96(26);
    Caption := '';
    Color := clBtnFace;
    BevelOuter := bvNone;
    BevelInner := bvNone;
    BorderStyle := bsNone;
    BorderSpacing.Top := FControlMargin;
    OnPaint := @StatusBarPaint;
    OnResize := @StatusBarRezise;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Style := psSolid;
    FResized := True;
  end;

  FButtonCancel := TSpeedButton.Create(FStatusBar);
  with FButtonCancel do begin
    Parent := FStatusBar;
    Align := alNone;
    AutoSize := False;
    Flat := True;
    Anchors := [akTop, akRight, akBottom];
    AnchorSideTop.Control := FStatusBar;
    AnchorSideTop.Side := asrTop;
    BorderSpacing.Top := FControlMargin;
    AnchorSideRight.Control := FStatusBar;
    AnchorSideRight.Side := asrRight;
    BorderSpacing.Right := FControlMargin;
    AnchorSideBottom.Control := FStatusBar;
    AnchorSideBottom.Side := asrBottom;
    BorderSpacing.Bottom := FControlMargin;
    Width := Height;
    OnClick := @ButtonCancelClick;
    if Assigned(FImageList) and (FButtonCancelImageIndex > -1) then begin
      Images := FImageList;
      ImageIndex := FButtonCancelImageIndex;
    end;
  end;

  StatusBarRezise(FStatusBar);

  FTimerRepaint := TTimer.Create(FStatusBar);
  FTimerRepaint.Interval := 1000;
  FTimerRepaint.OnTimer := @TimerRepaintTimer;
  FTimerRepaint.Enabled := True;
  FNeedRepaint := True;
end;

procedure TStatusBarDownload.SyncDestroy;
begin
  FStatusBar.Free;
end;

procedure TStatusBarDownload.TimerRepaintTimer(Sender: TObject);
begin
  if FNeedRepaint then
  begin
    FNeedRepaint := False;
    FStatusBar.Repaint;
  end;
end;

procedure TStatusBarDownload.StatusBarPaint(Sender: TObject);
var
  txtWidth, txtHeight: integer;
begin
  with StatusBar.Canvas do begin
    if FResized then begin
      FProgressBarRect := StatusBar.ClientRect;
      FProgressBarRect.Inflate(-FControlMargin, -(FControlMargin*2));
      FStatusTextRect := StatusBar.ClientRect;
      FProgressBarRect.Width := GetTextWidth('999.99 MB/999.99 MB');
      FStatusTextRect.Left := FProgressBarRect.Right + (FControlMargin * 2);
      FStatusTextRect.Right := FButtonCancel.Left - FControlMargin;
      FResized := False;
    end;

    Brush.Style := bsSolid;
    Pen.Style := psSolid;

    Pen.Color := clActiveBorder;
    Line(0,0,StatusBar.ClientRect.Right,0);

    Pen.Color := CL_BarGrayLine;
    Brush.Color := CL_BarGray;
    Rectangle(FProgressBarRect);

    if FPercents > 0 then begin
      FProgressBarPercentsRect := FProgressBarRect;
      FProgressBarPercentsRect.Right :=
        Round((FProgressBarPercentsRect.Right - FProgressBarPercentsRect.Left) * FPercents) + FProgressBarPercentsRect.Left;

      Pen.Color   := CL_BarGreenLine;
      Brush.Color := CL_BarGreen;

      Frame(FProgressBarPercentsRect);
      FProgressBarPercentsRect.Inflate(-2, -2);
      GradientFill(FProgressBarPercentsRect, BlendColor(Brush.Color, CL_BarGray, 128), Brush.Color, gdHorizontal);

      Brush.Style := bsClear;

      GetTextSize(FProgressText, txtWidth, txtHeight);
      TextRect(FProgressBarRect, FProgressBarRect.Left + ((FProgressBarRect.Right - FProgressBarRect.Left - txtWidth) div 2),
        FProgressBarRect.Top + ((FProgressBarRect.Bottom - FProgressBarRect.Top - txtHeight) div 2), FProgressText);
    end;
    txtHeight := GetTextHeight(FStatusText);
    TextRect(FStatusTextRect, FStatusTextRect.Left, FStatusTextRect.Top + ((FStatusTextRect.Bottom - FStatusTextRect.Top - txtHeight) div 2), FStatusText);
  end;
end;

procedure TStatusBarDownload.StatusBarRezise(Sender: TObject);
begin
  FResized := True;
end;

procedure TStatusBarDownload.ButtonCancelClick(Sender: TObject);
begin
  Self.Terminate;
end;

procedure TStatusBarDownload.HTTPSockOnStatus(Sender: TObject;
  Reason: THookSocketReason; const Value: String);
begin
  if Terminated then
    Exit;
  if Reason = HR_ReadCount then
  begin
    FNeedRepaint := True;
    if FTotalSize = 0 then
    begin
      FTotalSize := StrToIntDef(Trim(FHTTP.Headers.Values['Content-Length']), 0);
      FPercents := 0;
    end;

    Inc(FCurrentSize, StrToInt(Value));
    if (FCurrentSize <> 0) then
    begin
      if FTotalSize < FCurrentSize then
        FPercents := 0
      else
        FPercents := FCurrentSize / FTotalSize;
    end;
    FProgressText := FormatByteSize(FCurrentSize);
    if FTotalSize <> 0 then
     FProgressText := FProgressText + '/' + FormatByteSize(FTotalSize);
  end
  else
  if Reason = HR_Connect then
  begin
    FNeedRepaint := True;
    FCurrentSize := 0;
    FTotalSize := 0;
  end;
end;

procedure TStatusBarDownload.UpdateStatusText(AStatusText: String);
begin
  if AStatusText <> FStatusText then
  begin
    FStatusText := AStatusText;
    FNeedRepaint := True;
  end;
end;

constructor TStatusBarDownload.Create(CreateSuspended: Boolean;
  AOwnerForm: TForm; AImageList: TImageList; AButtonCancelImageIndex: Integer);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FOwnerForm := AOwnerForm;
  FImageList := AImageList;
  FButtonCancelImageIndex := AButtonCancelImageIndex;
  FProgressText := '';
  FStatusText := '';

  FHTTP := THTTPSendThread.Create(Self);
  FHTTP.UserAgent := UserAgentCURL;
  FHTTP.Sock.OnStatus := @HTTPSockOnStatus;
  FTotalSize := 0;
  FTotalSize := 0;
  FPercents := 0;

  Synchronize(@SyncCreate);
end;

destructor TStatusBarDownload.Destroy;
begin
  Synchronize(@SyncDestroy);

  FHTTP.Free;
  inherited Destroy;
end;

end.

