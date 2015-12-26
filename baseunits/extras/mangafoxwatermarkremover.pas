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
  minpsnr: Single = 9.0;

implementation

var
  imgtemplate: TDynImageDataArray;
  lockproc: TRTLCriticalSection;
  colorwhite: TColor32Rec = (Color: $FFFFFFFF);

function LoadTemplate(const TempDir: String): Integer;
var
  flist: TStringList;
  i: Integer;
begin
  Result := Length(imgtemplate);
  if DirectoryExists(TempDir) = False then Exit;
  if TryEnterCriticalsection(lockproc) > 0 then begin
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
      flist.Free;
    end;
    Result := Length(imgtemplate);
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
  imgbase, imgproc, imgtemp: TImageData;
  i, x, y, bmi: Integer;
  bmv, PSNR, MSE, RMSE, PAE, MAE: Single;
  invalidborder: Boolean;
begin
  Result := False;
  if not FileExistsUTF8(AFilename) then Exit;
  if Length(imgtemplate) = 0 then Exit;

  EnterCriticalsection(lockproc);
  InitImage(imgbase);
  try
    // not supported image file
    if not LoadImageFromFile(AFilename, imgbase) then Exit;

    bmi := -1;
    bmv := 0;
    // compare image to all template
    for i := Low(imgtemplate) to High(imgtemplate) do
      if imgbase.Height > imgtemplate[i].Height then begin
        InitImage(imgproc);
        InitImage(imgtemp);
        try
          invalidborder := False;

          // crop image to match template
          NewImage(imgbase.Width, imgtemplate[i].Height, imgtemplate[i].Format, imgproc);
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
            // adjust imagetemp width
            if imgbase.Width <> imgtemplate[i].Width then begin
              NewImage(imgbase.Width, imgtemplate[i].Height, imgtemplate[i].Format, imgtemp);
              FillRect(imgtemp, 0, 0, imgtemp.Width, imgtemp.Height, @colorwhite);
              if imgbase.Width > imgtemplate[i].Width then begin
                CopyRect(imgtemplate[i], 0, 0, imgtemplate[i].Width, imgtemp.Height, imgtemp, round((imgtemp.Width - imgtemplate[i].Width) / 2), 0);
              end
              else begin
                CopyRect(imgtemplate[i], round((imgtemplate[i].Width-imgtemp.Width) / 2), 0, imgtemp.Width, imgtemp.Height, imgtemp, 0, 0);
              end;
            end
            else
              CloneImage(imgtemplate[i], imgtemp);

            // compute error metrics
            ComputeErrorMetrics(imgtemp, imgproc, PSNR, MSE, RMSE, PAE, MAE);
            if PSNR > bmv then begin
              bmi := i;
              bmv := PSNR;
            end;
          end;
        finally
          FreeImage(imgproc);
          FreeImage(imgtemp);
        end;
      end;

    // save cropped image
    if (bmi > -1) and (bmv > minpsnr) then begin
      InitImage(imgproc);
      try
        NewImage(imgbase.Width, imgbase.Height - imgtemplate[bmi].Height, imgbase.Format, imgproc);
        CopyRect(imgbase, 0, 0, imgbase.Width, imgbase.Height - imgtemplate[bmi].Height, imgproc, 0, 0);
        if DeleteFileUTF8(AFilename) then
          Result := SaveImageToFile(AFilename, imgproc);
      finally
        FreeImage(imgproc);
      end;
    end;
  finally
    FreeImage(imgbase);
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

