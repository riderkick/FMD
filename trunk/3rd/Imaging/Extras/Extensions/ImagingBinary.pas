{
  $Id: ImagingBinary.pas 134 2008-08-27 20:50:07Z galfar $
  Vampyre Imaging Library
  by Marek Mauder 
  http://imaginglib.sourceforge.net

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
}

{ Unit with operations on binary images. Binary images in Imaging are
  ifGray8 images where pixels with value 0 are considerend off, an pixels > 0
  are on.}
unit ImagingBinary;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ImagingFormats, ImagingUtility;

type
  { Basic morphologic operators.}
  TMorphologyOp = (
    moErode,  // Erosion
    moDilate  // Dilatation
  );

  { Structuring element for morphology operations. Use ones and
   zeroes to define your struct elements.}
  TStructElement = array of array of Byte;

{ Thresholding using Otsu's method (which chooses the threshold
  to minimize the intraclass variance of the black and white pixels!).
  If Threshold is nil Image is automatically converted to binary using
  computed threshold level. Otherwise computed threshold is stored in Threshold
  and Image is not modified (if you're just interesting in global threshold level).}
procedure OtsuThresholding(var Image: TImageData; Threshold: PInteger = nil);
{ Applies basic morphology operators (Erode/Dilate) on Image using given structuring element
  Strel. You can do composite operations (Open/Close) by calling this function
  twice each time with different operator.}
procedure Morphology(var Image: TImageData; const Strel: TStructElement; Op: TMorphologyOp);

implementation

procedure OtsuThresholding(var Image: TImageData; Threshold: PInteger);
var
  Histogram: array[Byte] of Single;
  Level, Max, Min, I, J, NumPixels: Integer;
  Pix: PByte;
  Mean, Variance: Single;
  Mu, Omega, LevelMean, LargestMu: Single;
begin
  ConvertImage(Image, ifGray8);

  FillChar(Histogram, SizeOf(Histogram), 0);
  Min := 255;
  Max := 0;
  Level := 0;
  NumPixels := Image.Width * Image.Height;
  Pix := Image.Bits;

  // Compute histogram and determine min and max pixel values
  for I := 0 to NumPixels - 1 do
  begin
    Histogram[Pix^] := Histogram[Pix^] + 1.0;
    if Pix^ < Min then
      Min := Pix^;
    if Pix^ > Max then
      Max := Pix^;
    Inc(Pix);
  end;

  // Normalize histogram
  for I := 0 to 255 do
    Histogram[I] := Histogram[I] / NumPixels;

  // Compute image mean and variance
  Mean := 0.0;
  Variance := 0.0;
  for I := 0 to 255 do
    Mean := Mean + (I + 1) * Histogram[I];
  for I := 0 to 255 do
    Variance := Variance + Sqr(I + 1 - Mean) * Histogram[I];

  // Now finally compute threshold level
  LargestMu := 0;

  for I := 0 to 255 do
  begin
    Omega := 0.0;
    LevelMean := 0.0;

    for J := 0 to I - 1 do
    begin
      Omega := Omega + Histogram[J];
      LevelMean := LevelMean + (J + 1) * Histogram[J];
    end;

    Mu := Sqr(Mean * Omega - LevelMean);
    Omega := Omega * (1.0 - Omega);

    if Omega > 0.0 then
      Mu := Mu / Omega
    else
      Mu := 0;

    if Mu > LargestMu then
    begin
      LargestMu := Mu;
      Level := I;
    end;
  end;

  if Threshold = nil then
  begin
    // Do thresholding using computed level
    Pix := Image.Bits;
    for I := 0 to Image.Width * Image.Height - 1 do
    begin
      if Pix^ >= Level then
        Pix^ := 255
      else
        Pix^ := 0;
      Inc(Pix);
    end;
  end
  else
    Threshold^ := Level;
end;

procedure Morphology(var Image: TImageData; const Strel: TStructElement; Op: TMorphologyOp);
var
  X, Y, I, J: Integer;
  SWidth, SHeight, PixCount, PixVal, NumOnes, PosX, PosY: Integer;
  ImgOut: TImageData;
  OutPix: PByte;
begin
  Assert(Image.Format = ifGray8);
  Assert((Length(Strel) > 0) and (Length(Strel[0]) > 0));

  SWidth := Length(Strel);
  SHeight := Length(Strel[0]);

  NumOnes := 0;
  if Op = moErode then
  begin
    // We need to know number of ones in the strel for erosion
    for I := 0 to SWidth - 1 do
      for J := 0 to SHeight - 1 do
        NumOnes := NumOnes + Strel[I, J];
  end;

  InitImage(ImgOut);
  NewImage(Image.Width, Image.Height, ifGray8, ImgOut);
  OutPix := ImgOut.Bits;

  for J := 0 to Image.Height - 1 do
    for I := 0 to Image.Width - 1 do
    begin
      PixCount := 0;

      for X := 0 to SWidth - 1 do
      begin
        PosX := ClampInt(X + I - SWidth div 2, 0, Image.Width - 1);
        for Y := 0 to SHeight - 1 do
        begin
          PosY := ClampInt(Y + J - SHeight div 2, 0, Image.Height - 1);
          if (PosX >= 0) and (PosX < Image.Width) and
            (PosY >= 0) and (PosY < Image.Height) then
          begin
            PixVal := PByteArray(Image.Bits)[PosY * Image.Width + PosX];
          end
          else
            PixVal := 0;

          if (Strel[X, Y] > 0) and (PixVal > 0) then
            Inc(PixCount);
        end;
      end;

      case Op of
        moErode:  OutPix^ := Iff(PixCount = NumOnes, 255, 0);
        moDilate: OutPix^ := Iff(PixCount > 0, 255, 0);
      end;

      Inc(OutPix);
    end;

  FreeImage(Image);
  Image := ImgOut;
end;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.25.0 Changes/Bug Fixes -----------------------------------
    - Unit created with basic stuff (otsu and erode/dilate morphology ops).

}

end.
