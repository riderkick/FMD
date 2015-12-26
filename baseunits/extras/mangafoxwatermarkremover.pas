unit mangafoxwatermarkremover;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ImagingCompare, ImagingExtras, ImagingBinary,
  ImagingClasses, ImagingTypes, ImagingCanvases, Imaging, FileUtil,
  LazFileUtils;

function LoadTemplate(const TempDir: String): Integer;
function RemoveWatermark(const AFilename: String): Boolean;
procedure ClearTemplate;

var
  minwhiteborder: Integer = 4;

implementation

var
  imgtemplate: TDynImageDataArray;
  lockproc: TRTLCriticalSection;

function LoadTemplate(const TempDir: String): Integer;
var
  flist: TStringList;
  i: Integer;
begin
  Result := 0;
  if DirectoryExists(TempDir) = False then Exit;
  EnterCriticalsection(lockproc);
  flist := TStringList.Create;
  try
    FreeImagesInArray(imgtemplate);
    // load all images in template folder
    FindAllFiles(flist, CleanAndExpandDirectory(TempDir), '*.*', False);
    if flist.Count > 0 then
      for i := 0 to flist.Count - 1 do begin
        SetLength(imgtemplate, Length(imgtemplate) + 1);
        InitImage(imgtemplate[High(imgtemplate)]);
        if LoadImageFromFile(flist.Strings[i], imgtemplate[High(imgtemplate)]) then begin
          // convert to grayscale and do thresholding
          ConvertImage(imgtemplate[High(imgtemplate)], ifGray8);
          OtsuThresholding(imgtemplate[High(imgtemplate)], True);
        end
        else begin
          FreeImage(imgtemplate[High(imgtemplate)]);
          SetLength(imgtemplate, Length(imgtemplate) - 1);
        end;
      end;
  finally
    Result := Length(imgtemplate);
    flist.Free;
    LeaveCriticalsection(lockproc);
  end;
end;

procedure ClearTemplate;
begin
  FreeImagesInArray(imgtemplate);
end;

function ColorIsWhite(const Color: TColor32Rec): Boolean;
begin
  Result := (Color.R = 255) and (Color.G = 255) and (Color.B = 255);
end;

function RemoveWatermark(const AFilename: String): Boolean;
var
  imgbase, imgproc: TImageData;
  i, x, y: Integer;
  nh, bmi: Integer;
  bmv, PSNR, MSE, RMSE, PAE, MAE: single;
  invalidborder: Boolean;
begin
  Result := False;
  if not FileExistsUTF8(AFilename) then Exit;
  if Length(imgtemplate) = 0 then Exit;

  EnterCriticalsection(lockproc);
  InitImage(imgbase);
  InitImage(imgproc);
  try
    // not supported image file
    if not LoadImageFromFile(AFilename, imgbase) then Exit;

    // probably doesn't have watermark / 728px is default mangafox image width
    if imgbase.Width <> 728 then Exit;

    bmi := -1;
    bmv := 0;
    // compare image to all template
    for i := Low(imgtemplate) to High(imgtemplate) do
      if imgbase.Height > imgtemplate[i].Height then begin
        invalidborder := False;
        // crop image to match template
        NewImage(imgtemplate[i].Width, imgtemplate[i].Height, imgtemplate[i].Format, imgproc);
        CopyRect(imgbase, 0, imgbase.Height - imgtemplate[i].Height, imgbase.Width, imgbase.Height - imgtemplate[i].Height, imgproc, 0, 0);
        // do thresholding
        OtsuThresholding(imgproc, True);

        // check minimal white border
        for y := 1 to minwhiteborder do begin
          for x := 1 to imgproc.Width do begin
            if not ColorIsWhite(GetPixel32(imgproc, x, y)) then begin
              invalidborder := True;
              Break;
            end;
          end;
          if invalidborder then Break;
        end;

        if not invalidborder then begin
          // compute error metrics
          ComputeErrorMetrics(imgtemplate[i], imgproc, PSNR, MSE, RMSE, PAE, MAE);
          if PSNR > bmv then begin
            bmi := i;
            bmv := PSNR;
          end;
        end;
        FreeImage(imgproc);
      end;

    // save cropped image
    if bmi > -1 then begin
      nh := imgbase.Height - imgtemplate[bmi].Height;
      NewImage(imgbase.Width, nh, imgbase.Format, imgproc);
      CopyRect(imgbase, 0, 0, imgbase.Width, nh, imgproc, 0, 0);
      if DeleteFileUTF8(AFilename) then
        Result := SaveImageToFile(AFilename, imgproc);
    end;
  finally
    FreeImage(imgbase);
    FreeImage(imgproc);
    LeaveCriticalsection(lockproc);
  end;
end;

procedure doInitialize;
begin
  InitCriticalSection(lockproc);
  Initialize(imgtemplate);
end;

procedure doFinalize;
begin
  FreeImagesInArray(imgtemplate);
  DoneCriticalsection(lockproc);
end;

initialization
  doInitialize;

finalization
  doFinalize;

end.

