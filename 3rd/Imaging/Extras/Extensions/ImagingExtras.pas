{
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

{ This is helper unit that registers all image file formats in Extras package
  to Imaging core loading and saving functions. Just put this unit in your uses
  clause instead of adding every unit that provides new file format support.
  Also new constants for SetOption/GetOption functions for new file formats
  are located here.}
unit ImagingExtras;

{$I ImagingOptions.inc}

//{$DEFINE DONT_LINK_JPEG2000}    // link support for JPEG2000 images
//{$DEFINE DONT_LINK_TIFF}        // link support for TIFF images
//{$DEFINE DONT_LINK_PSD}         // link support for PSD images
//{$DEFINE DONT_LINK_PCX}         // link support for PCX images
//{$DEFINE DONT_LINK_XPM}         // link support for XPM images
{$IFNDEF FULL_FEATURE_SET}
  {$DEFINE DONT_LINK_ELDER}        // link support for Elder Imagery images
{$ENDIF}

{$IF not (
  (Defined(DCC) and Defined(CPUX86) and not Defined(MACOS)) or
  (Defined(FPC) and not Defined(MSDOS) and
    ((Defined(CPUX86) and (Defined(LINUX) or Defined(WIN32) or Defined(MACOS)) or
     (Defined(CPUX64) and Defined(LINUX)))))
  )}
  // JPEG2000 only for 32bit Windows/Linux/OSX and for 64bit Unix with FPC
  {$DEFINE DONT_LINK_JPEG2000}
{$IFEND}

{$IF not (Defined(DCC) and Defined(CPUX86) and not Defined(MACOS))}
  {$DEFINE DONT_LINK_TIFF} // Only for Delphi now
{$IFEND}

interface

const
  { Those are new options for GetOption/SetOption interface. }

  { Controls JPEG 2000 lossy compression quality. It is number in range 1..100.
    1 means small/ugly file, 100 means large/nice file. Default is 80.}
  ImagingJpeg2000Quality             = 55;
  { Controls whether JPEG 2000 image is saved with full file headers or just
    as code stream. Default value is False (0).}
  ImagingJpeg2000CodeStreamOnly      = 56;
  { Specifies JPEG 2000 image compression type. If True (1), saved JPEG 2000 files
    will be losslessly compressed. Otherwise lossy compression is used.
    Default value is False (0).}
  ImagingJpeg2000LosslessCompression = 57;
  { Specifies compression scheme used when saving TIFF images. Supported values
    are 0 (Uncompressed), 1 (LZW), 2 (PackBits RLE), 3 (Deflate - ZLib), 4 (JPEG),
    5 (CCITT Group 4 fax encoding - for binary images only).
    Default is 1 (LZW). Note that not all images can be stored with
    JPEG compression - these images will be saved with default compression if
    JPEG is set.}
  ImagingTiffCompression             = 65;
  { Controls compression quality when selected TIFF compression is Jpeg.
    It is number in range 1..100. 1 means small/ugly file,
    100 means large/nice file. Accessible trough ImagingTiffJpegQuality option.}
  ImagingTiffJpegQuality             = 66;
  { When activated (True = 1) existing TIFF files are not overwritten when saving but
    new images are instead appended thus producing multipage TIFFs.
    Default value is False (0).}
  ImagingTiffAppendMode              = 67;
  { If enabled image data is saved as layer of PSD file. This is required
    to get proper transparency when opened in Photoshop for images with
    alpha data (will be opened with one layer, RGB color channels, and transparency).
    If you don't need this Photoshop compatibility turn this option off as you'll get
    smaller file (will be opened in PS as background raster with RGBA channels).
    Default value is True (1). }
  ImagingPSDSaveAsLayer              = 70;

implementation

uses
{$IFNDEF DONT_LINK_JPEG2000}
  ImagingJpeg2000,
{$ENDIF}
{$IFNDEF DONT_LINK_TIFF}
  ImagingLibTiffDelphi,
{$ENDIF}
{$IFNDEF DONT_LINK_PSD}
  ImagingPsd,
{$ENDIF}
{$IFNDEF DONT_LINK_PCX}
  ImagingPcx,
{$ENDIF}
{$IFNDEF DONT_LINK_XPM}
  ImagingXpm,
{$ENDIF}
{$IFNDEF DONT_LINK_ELDER}
  ElderImagery,
{$ENDIF}
  Imaging;

{
  File Notes:

 -- TODOS -----------------------------------------------------
    - nothing now

  -- 0.77 -----------------------------------------------------
    - Added ImagingTiffAppendMode option.

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Added Group 4 Fax encoding as compression for TIFF files.
    - Added ImagingTiffJpegQuality option.

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Allowed JPEG2000 for Mac OS X x86

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - ElderImagery formats are disabled by default, TIFF enabled.
    - Changed _LINK_ symbols according to changes in ImagingOptions.inc.

  -- 0.24.1 Changes/Bug Fixes ---------------------------------
    - Allowed JPEG2000 for x86_64 CPUS in Linux

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Better IF conditional to disable JPEG2000 on unsupported platforms.
    - Added PSD and TIFF related stuff.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Created with initial stuff.

}

end.
