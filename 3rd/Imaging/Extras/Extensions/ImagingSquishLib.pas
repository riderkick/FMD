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

{ High quality DXTC compressor using Squish library (dynamicaly linked).}
unit ImagingSquishLib;

interface

{$I ImagingOptions.inc}

uses
  ImagingTypes, Imaging, ImagingFormats;

type
  TDXTCompressor = (
    dcClusterFit,   // Use a slow but high quality colour compressor (the default).
    dcRangeFit,     // Use a fast but low quality colour compressor.
    dcClusterFitAlphaWeighted // Cluster fit that weights the colour by alpha.
                              // For images that are rendered using alpha blending,
                              // this can significantly increase the perceived quality.
  );

  TColorMetric = (
    cmPerceptual,   // Use a perceptual metric for colour error (the default).
    cmUniform       // Use a uniform metric for colour error.
  );

{ Compresses SrcImage using selected DXTn compression into DestImage.
  DestImage should be cleared before calling.}
procedure DXTCompressImage(const SrcImage: TImageData; var DestImage: TImageData;
  DXTFormat: TImageFormat; Compressor: TDXTCompressor = dcClusterFit;
  Metric: TColorMetric = cmPerceptual);

implementation

const
  FlagDXT1 = 1 shl 0;
  FlagDXT3 = 1 shl 1;
  FlagDXT5 = 1 shl 2;
  FlagColourClusterFit       = 1 shl 3;
  FlagColourRangeFit         = 1 shl 4;
  FlagColourMetricPerceptual = 1 shl 5;
  FlagColourMetricUniform    = 1 shl 6;
  FlagWeightColourByAlpha    = 1 shl 7;

(* @brief Compresses an image in memory.

	@param rgba		The pixels of the source.
	@param width	The width of the source image.
	@param height	The height of the source image.
	@param blocks	Storage for the compressed output.
	@param flags	Compression flags.

	The source pixels should be presented as a contiguous array of width*height
	rgba values, with each component as 1 byte each. In memory this should be:

		{ r1, g1, b1, a1, .... , rn, gn, bn, an } for n = width*height

	The flags parameter should specify either kDxt1, kDxt3 or kDxt5 compression,
	however, DXT1 will be used by default if none is specified. When using DXT1
	compression, 8 bytes of storage are required for each compressed DXT block.
	DXT3 and DXT5 compression require 16 bytes of storage per block.

	The flags parameter can also specify a preferred colour compressor and
	colour error metric to use when fitting the RGB components of the data.
	Possible colour compressors are: kColourClusterFit (the default) or
	kColourRangeFit. Possible colour error metrics are: kColourMetricPerceptual
	(the default) or kColourMetricUniform. If no flags are specified in any
	particular category then the default will be used. Unknown flags are
	ignored.

	When using kColourClusterFit, an additional flag can be specified to
	weight the colour of each pixel by its alpha value. For images that are
	rendered using alpha blending, this can significantly increase the
	perceived quality.

	Internally this function calls squish::Compress for each block. To see how
	much memory is required in the compressed image, use
	squish::GetStorageRequirements.
*)

procedure CompressImage(RGBA: PByte; Width, Height: Integer; Blocks: Pointer;
  Flags: Integer); cdecl; external 'libsquish.dll';


procedure DXTCompressImage(const SrcImage: TImageData; var DestImage: TImageData;
  DXTFormat: TImageFormat; Compressor: TDXTCompressor = dcClusterFit;
  Metric: TColorMetric = cmPerceptual);
var
  Width, Height: Integer;
  Info: TImageFormatInfo;
  TempImage: TImageData;
  Flags: Integer;

  function GetSquishFlags: Integer;
  begin
    Result := 0;

    case DXTFormat of
      ifDXT1: Result := FlagDXT1;
      ifDXT3: Result := FlagDXT3;
      ifDXT5: Result := FlagDXT5;
    end;

    case Compressor of
      dcClusterFit: Result := Result or FlagColourClusterFit;
      dcRangeFit:   Result := Result or FlagColourRangeFit;
      dcClusterFitAlphaWeighted: Result := Result or FlagColourClusterFit or FlagWeightColourByAlpha;
    end;

    case Metric of
      cmPerceptual: Result := Result or FlagColourMetricPerceptual;
      cmUniform: Result := Result or FlagColourMetricUniform;
    end;
  end;

begin
  Assert(DXTFormat in [ifDXT1, ifDXT3, ifDXT5]);

  Width := SrcImage.Width;
  Height := SrcImage.Height;
  Flags := GetSquishFlags;

  // Check if input has correct dimensions and change them if needed
  GetImageFormatInfo(DXTFormat, Info);
  Info.CheckDimensions(DXTFormat, Width, Height);

  try
    // Create temp image as input for squish (must be ABGR order with
    // dimensions being multiples of 4)
    NewImage(Width, Height, ifA8R8G8B8, TempImage);
    CopyRect(SrcImage, 0, 0, SrcImage.Width, SrcImage.Height, TempImage, 0, 0);
    SwapChannels(TempImage, ChannelRed, ChannelBlue);

    // Init and create out image
    InitImage(DestImage);
    NewImage(Width, Height, DXTFormat, DestImage);

    // Finally call Squish
    CompressImage(TempImage.Bits, Width, Height, DestImage.Bits, Flags);
  finally
    FreeImage(TempImage);
  end;
end;

end.
