{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVGifImageAnimator: displaying animation for   }
{       TImagingGif by Marek Mauder.                    }
{                                                       }
{       Copyright © Sergey Galezdinov                   }
{       http://www.qip.ru                               }
{                                                       }
{*******************************************************}

{$I RV_Defs.inc}

unit RVVampireGifAnimate;

interface

{$IFNDEF RVDONOTUSEANIMATION}

uses
  Windows, Classes, Graphics, CRVFData, RVAnimate, RVItem,
  ImagingTypes, ImagingClasses, ImagingComponents;

type
  TRVGifImageAnimator = class (TRVAnimator)
    private
      procedure CalcInterval;
    protected
      function GetFrameCount: Integer; override;
    procedure ResetBackground; override;
    public
      procedure Reset; override;
      procedure ChangeFrame; override;
      procedure Draw(X, Y: Integer; Canvas: TCanvas; Animation: Boolean); override;
      function GetExportImageSize: TSize; override;
      procedure DrawForExport(Canvas: TCanvas); override;
  end;

{$ENDIF}

implementation

{$IFNDEF RVDONOTUSEANIMATION}
{ Determines how long to display the current frame. }
procedure TRVGifImageAnimator.CalcInterval;
var gif: TGifImage;
begin
  gif := TGifImage(TRVGraphicItemInfo(item).Image);
  Interval := 10;
  if not gif.Frames.Valid then Exit;

  if gif.Frames[FrameIndex].Extra <> nil then
  if gif.Frames[FrameIndex].Extra.Delay > 0 then
  begin
    Interval := gif.Frames[FrameIndex].Extra.Delay;
    if (Interval < 3) then
      Interval := 3;
    if (Interval > 1000) then
      Interval := 1000;
  end;
  Interval := Interval * 10;
end;

{ Change frame to the next one. Updates Interval. }
procedure TRVGifImageAnimator.ChangeFrame;
begin
  CalcNextFrameIndex;
  CalcInterval;
end;

{ Draws the current frame }
procedure TRVGifImageAnimator.Draw(X, Y: Integer; Canvas: TCanvas; Animation: Boolean);
var
  gif: TGifImage;
  R: TRect;
  bmpsrc: TBitmap;
  FBackColor: TColor;
  UseSrcBitmap: Boolean;
begin
  gif := TGifImage(TRVGraphicItemInfo(item).Image);
  gif.BackgroundSharing := True;
  gif.SelfAnimated := False;
  gif.ReAnimate    := False;

  gif.ActiveIndex := FrameIndex;

  R := Rect(0, 0, 0, 0);
  bmpsrc := TBitmap.Create;
  bmpsrc.PixelFormat := pf32bit;
  with gif do
  begin
    bmpsrc.Width  := Width;
    bmpsrc.Height := Height;
  end;

  RVData.GetItemBackground(RVData.DrawItems[item.DrawItemNo].ItemNo, R, True,
    FBackColor, bmpsrc, UseSrcBitmap);

  gif.BackGroundColor := FBackColor;
  if UseSrcBitmap then
    gif.BackGroundBitmap := bmpsrc;

  try
    canvas.Draw(X, Y, gif);
    gif.BackGroundBitmap := nil;
    gif.BackGroundColor := clNone;
  except
    //SendDebug('error animated painting');
  end;
  bmpsrc.FreeImage;
  bmpsrc.Free;
end;

{ Draws the first frame for RTF export }
procedure TRVGifImageAnimator.DrawForExport(Canvas: TCanvas);
begin
  TGifImage(TRVGraphicItemInfo(item).Image).ActiveIndex := 0;
  Draw(0, 0, Canvas, False);
end;

{ Image size for RTF saving }
function TRVGifImageAnimator.GetExportImageSize: TSize;
begin
  Result.cy := TGifImage(TRVGraphicItemInfo(item).Image).Height;
  Result.cx := TGifImage(TRVGraphicItemInfo(item).Image).Width;
end;

{ Returns a number of frames in gif }
function GetGifFrameCount(gif: TGifImage): Integer;
begin
  Result := 0;
  if (gif = nil) or gif.Empty then
    Exit;

  if gif.Frames.AllImagesValid then
    Result := gif.Frames.ImageCount
  else
    Result := 1;
end;

{ Returns a number of frames }
function TRVGifImageAnimator.GetFrameCount: Integer;
begin
  Result := GetGifFrameCount(TGifImage(TRVGraphicItemInfo(item).Image));
end;

{ Rewinds to the first frame. Updates Interval. }
procedure TRVGifImageAnimator.Reset;
var
  gif: TGifImage;
begin
  gif := TGifImage(TRVGraphicItemInfo(item).Image);
  gif.ActiveIndex := 0;
  gif.BackgroundSharing := True;
  gif.SelfAnimated := False;
  gif.ReAnimate    := False;
  FrameIndex := 0;
  CalcInterval;
end;


procedure TRVGifImageAnimator.ResetBackground;
begin
  inherited;
end;

var DefMakeAnimator: TRVMakeAnimatorProc;
{ This procedure creates an animator (anim) for the item, if it's necessary.
  This procedure can create only TRVVampireGifImageAnimator.
  If it cannot be applied, it calls the stored value of RV_MakeAnimator. }
procedure RV_MakeAnimatorGif(item: TCustomRVItemInfo; RVData: TCustomRVFormattedData;
  var anim: TRVAnimator);
begin
  if (item is TRVGraphicItemInfo) and
     (TRVGraphicItemInfo(item).Image is TGifImage) and
     (GetGifFrameCount(TGifImage(TRVGraphicItemInfo(item).Image))>1) then
  begin
    if (anim<>nil) and not (anim is TRVGifImageAnimator) then
    begin
      anim.Free;
      anim := nil;
    end;
    if anim=nil then
    begin
      anim := TRVGifImageAnimator.Create(RVData, Item);
      RVData.InsertAnimator(TObject(anim));
      end
    else
    if anim<>nil then
    begin
      anim.Update(RVData, Item);
      anim.Reset;
    end;
    exit;
  end;     //}
  DefMakeAnimator(item, RVData, anim)
end;

initialization
  DefMakeAnimator := RV_MakeAnimator;
  RV_MakeAnimator := RV_MakeAnimatorGif;

{$ENDIF}

end.
