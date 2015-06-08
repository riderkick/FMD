unit LibTiffDelphi;

{$ALIGN 8}
{$MINENUMSIZE 1}

interface

uses
  Windows, SysUtils, Classes;

const
  //DW
  LibTiffDelphiVersionString = 'LibTiffDelphi 3.9.4.00'#13#10'Pre-compiled LibTiff for Delphi'#13#10'http://www.awaresystems.be/'#13#10'3.9.4 implementaion by Do-wan Kim'#13#10;

  TIFF_NOTYPE                           = 0;
  TIFF_BYTE                             = 1;       { 8-bit unsigned integer }
  TIFF_ASCII                            = 2;       { 8-bit bytes w/ last byte null }
  TIFF_SHORT                            = 3;       { 16-bit unsigned integer }
  TIFF_LONG                             = 4;       { 32-bit unsigned integer }
  TIFF_RATIONAL                         = 5;       { 64-bit unsigned fraction }
  TIFF_SBYTE                            = 6;       { !8-bit signed integer }
  TIFF_UNDEFINED                        = 7;       { !8-bit untyped data }
  TIFF_SSHORT                           = 8;       { !16-bit signed integer }
  TIFF_SLONG                            = 9;       { !32-bit signed integer }
  TIFF_SRATIONAL                        = 10;      { !64-bit signed fraction }
  TIFF_FLOAT                            = 11;      { !32-bit IEEE floating point }
  TIFF_DOUBLE                           = 12;      { !64-bit IEEE floating point }
  TIFF_IFD                              = 13;      { %32-bit unsigned integer (offset) }
  TIFF_UNICODE                          = 14;
  TIFF_COMPLEX                          = 15;
  TIFF_LONG8                            = 16;
  TIFF_SLONG8                           = 17;
  TIFF_IFD8                             = 18;


  TIFFTAG_SUBFILETYPE                   = 254;     { subfile data descriptor }
    FILETYPE_REDUCEDIMAGE               = $1;      { reduced resolution version }
    FILETYPE_PAGE                       = $2;      { one page of many }
    FILETYPE_MASK                       = $4;      { transparency mask }
  TIFFTAG_OSUBFILETYPE                  = 255;     { kind of data in subfile }
    OFILETYPE_IMAGE                     = 1;       { full resolution image data }
    OFILETYPE_REDUCEDIMAGE              = 2;       { reduced size image data }
    OFILETYPE_PAGE                      = 3;       { one page of many }
  TIFFTAG_IMAGEWIDTH                    = 256;     { image width in pixels }
  TIFFTAG_IMAGELENGTH                   = 257;     { image height in pixels }
  TIFFTAG_BITSPERSAMPLE                 = 258;     { bits per channel (sample) }
  TIFFTAG_COMPRESSION                   = 259;   { data compression technique }
    COMPRESSION_NONE                    = 1;     { dump mode }
    COMPRESSION_CCITTRLE                = 2;     { CCITT modified Huffman RLE }
    COMPRESSION_CCITTFAX3               = 3;     { CCITT Group 3 fax encoding }
    COMPRESSION_CCITT_T4                = 3;       { CCITT T.4 (TIFF 6 name) }
    COMPRESSION_CCITTFAX4	        = 4;	   { CCITT Group 4 fax encoding }
    COMPRESSION_CCITT_T6                = 4;       { CCITT T.6 (TIFF 6 name) }
    COMPRESSION_LZW		        = 5;       { Lempel-Ziv  & Welch }
    COMPRESSION_OJPEG		        = 6;	   { !6.0 JPEG }
    COMPRESSION_JPEG		        = 7;	   { %JPEG DCT compression }
    COMPRESSION_NEXT		        = 32766;   { NeXT 2-bit RLE }
    COMPRESSION_CCITTRLEW	        = 32771;   { #1 w/ word alignment }
    COMPRESSION_PACKBITS	        = 32773;   { Macintosh RLE }
    COMPRESSION_THUNDERSCAN	        = 32809;   { ThunderScan RLE }
    { codes 32895-32898 are reserved for ANSI IT8 TIFF/IT <dkelly@apago.com) }
    COMPRESSION_IT8CTPAD	        = 32895;   { IT8 CT w/padding }
    COMPRESSION_IT8LW		        = 32896;   { IT8 Linework RLE }
    COMPRESSION_IT8MP		        = 32897;   { IT8 Monochrome picture }
    COMPRESSION_IT8BL		        = 32898;   { IT8 Binary line art }
    { compression codes 32908-32911 are reserved for Pixar }
    COMPRESSION_PIXARFILM	        = 32908;   { Pixar companded 10bit LZW }
    COMPRESSION_PIXARLOG	        = 32909;   { Pixar companded 11bit ZIP }
    COMPRESSION_DEFLATE		        = 32946;   { Deflate compression }
    COMPRESSION_ADOBE_DEFLATE           = 8;       { Deflate compression, as recognized by Adobe }
    { compression code 32947 is reserved for Oceana Matrix <dev@oceana.com> }
    COMPRESSION_DCS                     = 32947;   { Kodak DCS encoding }
    COMPRESSION_JBIG		        = 34661;   { ISO JBIG }
    COMPRESSION_SGILOG		        = 34676;   { SGI Log Luminance RLE }
    COMPRESSION_SGILOG24	        = 34677;   { SGI Log 24-bit packed }
    COMPRESSION_JP2000                  = 34712;   { Leadtools JPEG2000 }
  TIFFTAG_PHOTOMETRIC                   = 262;     { photometric interpretation }
    PHOTOMETRIC_MINISWHITE              = 0;       { min value is white }
    PHOTOMETRIC_MINISBLACK              = 1;       { min value is black }
    PHOTOMETRIC_RGB                     = 2;       { RGB color model }
    PHOTOMETRIC_PALETTE                 = 3;       { color map indexed }
    PHOTOMETRIC_MASK                    = 4;       { $holdout mask }
    PHOTOMETRIC_SEPARATED               = 5;       { !color separations }
    PHOTOMETRIC_YCBCR                   = 6;       { !CCIR 601 }
    PHOTOMETRIC_CIELAB                  = 8;       { !1976 CIE L*a*b* }
    PHOTOMETRIC_ICCLAB                  = 9;       { ICC L*a*b* [Adobe TIFF Technote 4] }
    PHOTOMETRIC_ITULAB                  = 10;      { ITU L*a*b* }
    PHOTOMETRIC_LOGL                    = 32844;   { CIE Log2(L) }
    PHOTOMETRIC_LOGLUV                  = 32845;   { CIE Log2(L) (u',v') }
  TIFFTAG_THRESHHOLDING                 = 263;     { thresholding used on data }
    THRESHHOLD_BILEVEL                  = 1;       { b&w art scan }
    THRESHHOLD_HALFTONE                 = 2;       { or dithered scan }
    THRESHHOLD_ERRORDIFFUSE             = 3;       { usually floyd-steinberg }
  TIFFTAG_CELLWIDTH                     = 264;     { +dithering matrix width }
  TIFFTAG_CELLLENGTH                    = 265;     { +dithering matrix height }
  TIFFTAG_FILLORDER                     = 266;     { data order within a byte }
    FILLORDER_MSB2LSB                   = 1;       { most significant -> least }
    FILLORDER_LSB2MSB                   = 2;       { least significant -> most }
  TIFFTAG_DOCUMENTNAME                  = 269;     { name of doc. image is from }
  TIFFTAG_IMAGEDESCRIPTION              = 270;     { info about image }
  TIFFTAG_MAKE                          = 271;     { scanner manufacturer name }
  TIFFTAG_MODEL                         = 272;     { scanner model name/number }
  TIFFTAG_STRIPOFFSETS                  = 273;     { offsets to data strips }
  TIFFTAG_ORIENTATION                   = 274;     { +image orientation }
    ORIENTATION_TOPLEFT                 = 1;       { row 0 top, col 0 lhs }
    ORIENTATION_TOPRIGHT                = 2;       { row 0 top, col 0 rhs }
    ORIENTATION_BOTRIGHT                = 3;       { row 0 bottom, col 0 rhs }
    ORIENTATION_BOTLEFT                 = 4;       { row 0 bottom, col 0 lhs }
    ORIENTATION_LEFTTOP                 = 5;       { row 0 lhs, col 0 top }
    ORIENTATION_RIGHTTOP                = 6;       { row 0 rhs, col 0 top }
    ORIENTATION_RIGHTBOT                = 7;       { row 0 rhs, col 0 bottom }
    ORIENTATION_LEFTBOT                 = 8;       { row 0 lhs, col 0 bottom }
  TIFFTAG_SAMPLESPERPIXEL               = 277;     { samples per pixel }
  TIFFTAG_ROWSPERSTRIP                  = 278;     { rows per strip of data }
  TIFFTAG_STRIPBYTECOUNTS               = 279;     { bytes counts for strips }
  TIFFTAG_MINSAMPLEVALUE                = 280;     { +minimum sample value }
  TIFFTAG_MAXSAMPLEVALUE                = 281;     { +maximum sample value }
  TIFFTAG_XRESOLUTION                   = 282;     { pixels/resolution in x }
  TIFFTAG_YRESOLUTION                   = 283;     { pixels/resolution in y }
  TIFFTAG_PLANARCONFIG                  = 284;     { storage organization }
    PLANARCONFIG_CONTIG                 = 1;       { single image plane }
    PLANARCONFIG_SEPARATE               = 2;       { separate planes of data }
  TIFFTAG_PAGENAME                      = 285;     { page name image is from }
  TIFFTAG_XPOSITION                     = 286;     { x page offset of image lhs }
  TIFFTAG_YPOSITION                     = 287;     { y page offset of image lhs }
  TIFFTAG_FREEOFFSETS                   = 288;     { +byte offset to free block }
  TIFFTAG_FREEBYTECOUNTS                = 289;     { +sizes of free blocks }

  {matched with tag reference up to this point}

  TIFFTAG_GRAYRESPONSEUNIT              = 290;     { $gray scale curve accuracy }
    GRAYRESPONSEUNIT_10S                = 1;       { tenths of a unit }
    GRAYRESPONSEUNIT_100S               = 2;       { hundredths of a unit }
    GRAYRESPONSEUNIT_1000S              = 3;       { thousandths of a unit }
    GRAYRESPONSEUNIT_10000S             = 4;       { ten-thousandths of a unit }
    GRAYRESPONSEUNIT_100000S            = 5;       { hundred-thousandths }
  TIFFTAG_GRAYRESPONSECURVE             = 291;     { $gray scale response curve }
  TIFFTAG_GROUP3OPTIONS                 = 292;     { 32 flag bits }
  TIFFTAG_T4OPTIONS                     = 292;     { TIFF 6.0 proper name alias }
    GROUP3OPT_2DENCODING                = $1;      { 2-dimensional coding }
    GROUP3OPT_UNCOMPRESSED              = $2;      { data not compressed }
    GROUP3OPT_FILLBITS                  = $4;      { fill to byte boundary }
  TIFFTAG_GROUP4OPTIONS                 = 293;     { 32 flag bits }
  TIFFTAG_T6OPTIONS                     = 293;     { TIFF 6.0 proper name }
    GROUP4OPT_UNCOMPRESSED              = $2;      { data not compressed }
  TIFFTAG_RESOLUTIONUNIT                = 296;     { units of resolutions }
    RESUNIT_NONE                        = 1;       { no meaningful units }
    RESUNIT_INCH                        = 2;       { english }
    RESUNIT_CENTIMETER                  = 3;       { metric }
  TIFFTAG_PAGENUMBER                    = 297;     { page numbers of multi-page }
  TIFFTAG_COLORRESPONSEUNIT             = 300;     { $color curve accuracy }
    COLORRESPONSEUNIT_10S               = 1;       { tenths of a unit }
    COLORRESPONSEUNIT_100S              = 2;       { hundredths of a unit }
    COLORRESPONSEUNIT_1000S             = 3;       { thousandths of a unit }
    COLORRESPONSEUNIT_10000S            = 4;       { ten-thousandths of a unit }
    COLORRESPONSEUNIT_100000S           = 5;       { hundred-thousandths }
  TIFFTAG_TRANSFERFUNCTION              = 301;     { !colorimetry info }
  TIFFTAG_SOFTWARE                      = 305;     { name & release }
  TIFFTAG_DATETIME                      = 306;     { creation date and time }
  TIFFTAG_ARTIST                        = 315;     { creator of image }
  TIFFTAG_HOSTCOMPUTER                  = 316;     { machine where created }
  TIFFTAG_PREDICTOR                     = 317;     { prediction scheme w/ LZW }
  TIFFTAG_WHITEPOINT                    = 318;     { image white point }
  TIFFTAG_PRIMARYCHROMATICITIES         = 319;     { !primary chromaticities }
  TIFFTAG_COLORMAP                      = 320;     { RGB map for pallette image }
  TIFFTAG_HALFTONEHINTS                 = 321;     { !highlight+shadow info }
  TIFFTAG_TILEWIDTH                     = 322;     { !rows/data tile }
  TIFFTAG_TILELENGTH                    = 323;     { !cols/data tile }
  TIFFTAG_TILEOFFSETS                   = 324;     { !offsets to data tiles }
  TIFFTAG_TILEBYTECOUNTS                = 325;     { !byte counts for tiles }
  TIFFTAG_BADFAXLINES                   = 326;     { lines w/ wrong pixel count }
  TIFFTAG_CLEANFAXDATA                  = 327;     { regenerated line info }
    CLEANFAXDATA_CLEAN                  = 0;       { no errors detected }
    CLEANFAXDATA_REGENERATED            = 1;       { receiver regenerated lines }
    CLEANFAXDATA_UNCLEAN                = 2;       { uncorrected errors exist }
  TIFFTAG_CONSECUTIVEBADFAXLINES        = 328;     { max consecutive bad lines }
  TIFFTAG_SUBIFD                        = 330;     { subimage descriptors }
  TIFFTAG_INKSET                        = 332;     { !inks in separated image }
    INKSET_CMYK                         = 1;       { !cyan-magenta-yellow-black color }
    INKSET_MULTIINK                     = 2;       { !multi-ink or hi-fi color }
  TIFFTAG_INKNAMES                      = 333;     { !ascii names of inks }
  TIFFTAG_NUMBEROFINKS                  = 334;     { !number of inks }
  TIFFTAG_DOTRANGE                      = 336;     { !0% and 100% dot codes }
  TIFFTAG_TARGETPRINTER                 = 337;     { !separation target }
  TIFFTAG_EXTRASAMPLES                  = 338;     { !info about extra samples }
    EXTRASAMPLE_UNSPECIFIED             = 0;       { !unspecified data }
    EXTRASAMPLE_ASSOCALPHA              = 1;       { !associated alpha data }
    EXTRASAMPLE_UNASSALPHA              = 2;       { !unassociated alpha data }
  TIFFTAG_SAMPLEFORMAT                  = 339;     { !data sample format }
    SAMPLEFORMAT_UINT                   = 1;       { !unsigned integer data }
    SAMPLEFORMAT_INT                    = 2;       { !signed integer data }
    SAMPLEFORMAT_IEEEFP                 = 3;       { !IEEE floating point data }
    SAMPLEFORMAT_VOID                   = 4;       { !untyped data }
    SAMPLEFORMAT_COMPLEXINT             = 5;       { !complex signed int }
    SAMPLEFORMAT_COMPLEXIEEEFP          = 6;       { !complex ieee floating }
  TIFFTAG_SMINSAMPLEVALUE               = 340;     { !variable MinSampleValue }
  TIFFTAG_SMAXSAMPLEVALUE               = 341;     { !variable MaxSampleValue }
  TIFFTAG_CLIPPATH                      = 343;     { %ClipPath [Adobe TIFF technote 2] }
  TIFFTAG_XCLIPPATHUNITS                = 344;     { %XClipPathUnits [Adobe TIFF technote 2] }
  TIFFTAG_YCLIPPATHUNITS                = 345;     { %YClipPathUnits [Adobe TIFF technote 2] }
  TIFFTAG_INDEXED                       = 346;     { %Indexed [Adobe TIFF Technote 3] }
  TIFFTAG_JPEGTABLES                    = 347;     { %JPEG table stream }
  TIFFTAG_OPIPROXY                      = 351;     { %OPI Proxy [Adobe TIFF technote] }
  { Tags 512-521 are obsoleted by Technical Note #2
  which specifies a revised JPEG-in-TIFF scheme. }
  TIFFTAG_JPEGPROC                      = 512;     { !JPEG processing algorithm }
    JPEGPROC_BASELINE                   = 1;       { !baseline sequential }
    JPEGPROC_LOSSLESS                   = 14;      { !Huffman coded lossless }
  TIFFTAG_JPEGIFOFFSET                  = 513;     { !pointer to SOI marker }
  TIFFTAG_JPEGIFBYTECOUNT               = 514;     { !JFIF stream length }
  TIFFTAG_JPEGRESTARTINTERVAL           = 515;     { !restart interval length }
  TIFFTAG_JPEGLOSSLESSPREDICTORS        = 517;     { !lossless proc predictor }
  TIFFTAG_JPEGPOINTTRANSFORM            = 518;     { !lossless point transform }
  TIFFTAG_JPEGQTABLES                   = 519;     { !Q matrice offsets }
  TIFFTAG_JPEGDCTABLES                  = 520;     { !DCT table offsets }
  TIFFTAG_JPEGACTABLES                  = 521;     { !AC coefficient offsets }
  TIFFTAG_YCBCRCOEFFICIENTS             = 529;     { !RGB -> YCbCr transform }
  TIFFTAG_YCBCRSUBSAMPLING              = 530;     { !YCbCr subsampling factors }
  TIFFTAG_YCBCRPOSITIONING              = 531;     { !subsample positioning }
    YCBCRPOSITION_CENTERED              = 1;       { !as in PostScript Level 2 }
    YCBCRPOSITION_COSITED               = 2;       { !as in CCIR 601-1 }
  TIFFTAG_REFERENCEBLACKWHITE           = 532;     { !colorimetry info }
  TIFFTAG_XMLPACKET                     = 700;     { %XML packet [Adobe XMP technote 9-14-02] (dkelly@apago.com) }
  TIFFTAG_OPIIMAGEID                    = 32781;   { %OPI ImageID [Adobe TIFF technote] }
  { tags 32952-32956 are private tags registered to Island Graphics }
  TIFFTAG_REFPTS                        = 32953;   { image reference points }
  TIFFTAG_REGIONTACKPOINT               = 32954;   { region-xform tack point }
  TIFFTAG_REGIONWARPCORNERS             = 32955;   { warp quadrilateral }
  TIFFTAG_REGIONAFFINE                  = 32956;   { affine transformation mat }
  { tags 32995-32999 are private tags registered to SGI }
  TIFFTAG_MATTEING                      = 32995;   { $use ExtraSamples }
  TIFFTAG_DATATYPE                      = 32996;   { $use SampleFormat }
  TIFFTAG_IMAGEDEPTH                    = 32997;   { z depth of image }
  TIFFTAG_TILEDEPTH                     = 32998;   { z depth/data tile }
  { tags 33300-33309 are private tags registered to Pixar }
  { TIFFTAG_PIXAR_IMAGEFULLWIDTH and TIFFTAG_PIXAR_IMAGEFULLLENGTH are set when an image has been cropped out of a larger image.
    They reflect the size of the original uncropped image. The TIFFTAG_XPOSITION and TIFFTAG_YPOSITION can be used to determine the
    position of the smaller image in the larger one. }
  TIFFTAG_PIXAR_IMAGEFULLWIDTH          = 33300;   { full image size in x }
  TIFFTAG_PIXAR_IMAGEFULLLENGTH         = 33301;   { full image size in y }
  { Tags 33302-33306 are used to identify special image modes and data used by Pixar's texture formats. }
  TIFFTAG_PIXAR_TEXTUREFORMAT           = 33302;   { texture map format }
  TIFFTAG_PIXAR_WRAPMODES               = 33303;   { s & t wrap modes }
  TIFFTAG_PIXAR_FOVCOT                  = 33304;   { cotan(fov) for env. maps }
  TIFFTAG_PIXAR_MATRIX_WORLDTOSCREEN    = 33305;
  TIFFTAG_PIXAR_MATRIX_WORLDTOCAMERA    = 33306;
  { tag 33405 is a private tag registered to Eastman Kodak }
  TIFFTAG_WRITERSERIALNUMBER            = 33405;   { device serial number }
  { tag 33432 is listed in the 6.0 spec w/ unknown ownership }
  TIFFTAG_COPYRIGHT                     = 33432;   { copyright string }
  { IPTC TAG from RichTIFF specifications }
  TIFFTAG_RICHTIFFIPTC                  = 33723;
  { 34016-34029 are reserved for ANSI IT8 TIFF/IT <dkelly@apago.com) }
  TIFFTAG_IT8SITE                       = 34016;   { site name }
  TIFFTAG_IT8COLORSEQUENCE              = 34017;   { color seq. [RGB,CMYK,etc] }
  TIFFTAG_IT8HEADER                     = 34018;   { DDES Header }
  TIFFTAG_IT8RASTERPADDING              = 34019;   { raster scanline padding }
  TIFFTAG_IT8BITSPERRUNLENGTH           = 34020;   { # of bits in short run }
  TIFFTAG_IT8BITSPEREXTENDEDRUNLENGTH   = 34021;   { # of bits in long run }
  TIFFTAG_IT8COLORTABLE                 = 34022;   { LW colortable }
  TIFFTAG_IT8IMAGECOLORINDICATOR        = 34023;   { BP/BL image color switch }
  TIFFTAG_IT8BKGCOLORINDICATOR          = 34024;   { BP/BL bg color switch }
  TIFFTAG_IT8IMAGECOLORVALUE            = 34025;   { BP/BL image color value }
  TIFFTAG_IT8BKGCOLORVALUE              = 34026;   { BP/BL bg color value }
  TIFFTAG_IT8PIXELINTENSITYRANGE        = 34027;   { MP pixel intensity value }
  TIFFTAG_IT8TRANSPARENCYINDICATOR      = 34028;   { HC transparency switch }
  TIFFTAG_IT8COLORCHARACTERIZATION      = 34029;   { color character. table }
  TIFFTAG_IT8HCUSAGE                    = 34030;   { HC usage indicator }
  TIFFTAG_IT8TRAPINDICATOR              = 34031;   { Trapping indicator (untrapped=0, trapped=1) }
  TIFFTAG_IT8CMYKEQUIVALENT             = 34032;   { CMYK color equivalents }
  { tags 34232-34236 are private tags registered to Texas Instruments }
  TIFFTAG_FRAMECOUNT                    = 34232;   { Sequence Frame Count }
  { tag 34750 is a private tag registered to Adobe? }
  TIFFTAG_ICCPROFILE                    = 34675;   { ICC profile data }
  { tag 34377 is private tag registered to Adobe for PhotoShop }
  TIFFTAG_PHOTOSHOP                     = 34377;
  { tag 34750 is a private tag registered to Pixel Magic }
  TIFFTAG_JBIGOPTIONS                   = 34750;   { JBIG options }
  { tags 34908-34914 are private tags registered to SGI }
  TIFFTAG_FAXRECVPARAMS                 = 34908;   { encoded Class 2 ses. parms }
  TIFFTAG_FAXSUBADDRESS                 = 34909;   { received SubAddr string }
  TIFFTAG_FAXRECVTIME                   = 34910;   { receive time (secs) }
  { tags 37439-37443 are registered to SGI <gregl@sgi.com> }
  TIFFTAG_STONITS                       = 37439;   { Sample value to Nits }
  { tag 34929 is a private tag registered to FedEx }
  TIFFTAG_FEDEX_EDR                     = 34929;   { unknown use }
  { tag 65535 is an undefined tag used by Eastman Kodak }
  TIFFTAG_DCSHUESHIFTVALUES             = 65535;   { hue shift correction data }
  { The following are ``pseudo tags'' that can be used to control codec-specific functionality. These tags are not written to file.
    Note that these values start at 0xffff+1 so that they'll never collide with Aldus-assigned tags. }
  TIFFTAG_FAXMODE                       = 65536;   { Group 3/4 format control }
    FAXMODE_CLASSIC                     = $0;      { default, include RTC }
    FAXMODE_NORTC                       = $1;      { no RTC at end of data }
    FAXMODE_NOEOL                       = $2;      { no EOL code at end of row }
    FAXMODE_BYTEALIGN                   = $4;      { byte align row }
    FAXMODE_WORDALIGN                   = $8;      { word align row }
    FAXMODE_CLASSF                      = FAXMODE_NORTC;        { TIFF Class F }
  TIFFTAG_JPEGQUALITY                   = 65537;   { Compression quality level }
  { Note: quality level is on the IJG 0-100 scale.  Default value is 75 }
  TIFFTAG_JPEGCOLORMODE                 = 65538;   { Auto RGB<=>YCbCr convert? }
    JPEGCOLORMODE_RAW                   = $0;      { no conversion (default) }
    JPEGCOLORMODE_RGB                   = $1;      { do auto conversion }
  TIFFTAG_JPEGTABLESMODE                = 65539;   { What to put in JPEGTables }
    JPEGTABLESMODE_QUANT                = $1;      { include quantization tbls }
    JPEGTABLESMODE_HUFF                 = $2;      { include Huffman tbls }
  { Note: default is JPEGTABLESMODE_QUANT | JPEGTABLESMODE_HUFF }
  TIFFTAG_FAXFILLFUNC                   = 65540;   { G3/G4 fill function }
  TIFFTAG_PIXARLOGDATAFMT               = 65549;   { PixarLogCodec I/O data sz }
    PIXARLOGDATAFMT_8BIT                = 0;       { regular u_char samples }
    PIXARLOGDATAFMT_8BITABGR            = 1;       { ABGR-order u_chars }
    PIXARLOGDATAFMT_11BITLOG            = 2;       { 11-bit log-encoded (raw) }
    PIXARLOGDATAFMT_12BITPICIO          = 3;       { as per PICIO (1.0==2048) }
    PIXARLOGDATAFMT_16BIT               = 4;       { signed short samples }
    PIXARLOGDATAFMT_FLOAT               = 5;       { IEEE float samples }
  { 65550-65556 are allocated to Oceana Matrix <dev@oceana.com> }
  TIFFTAG_DCSIMAGERTYPE                 = 65550;   { imager model & filter }
    DCSIMAGERMODEL_M3                   = 0;       { M3 chip (1280 x 1024) }
    DCSIMAGERMODEL_M5                   = 1;       { M5 chip (1536 x 1024) }
    DCSIMAGERMODEL_M6                   = 2;       { M6 chip (3072 x 2048) }
    DCSIMAGERFILTER_IR                  = 0;       { infrared filter }
    DCSIMAGERFILTER_MONO                = 1;       { monochrome filter }
    DCSIMAGERFILTER_CFA                 = 2;       { color filter array }
    DCSIMAGERFILTER_OTHER               = 3;       { other filter }
  TIFFTAG_DCSINTERPMODE                 = 65551;   { interpolation mode }
    DCSINTERPMODE_NORMAL                = 0;       { whole image, default }
    DCSINTERPMODE_PREVIEW               = 1;       { preview of image (384x256) }
  TIFFTAG_DCSBALANCEARRAY               = 65552;   { color balance values }
  TIFFTAG_DCSCORRECTMATRIX              = 65553;   { color correction values }
  TIFFTAG_DCSGAMMA                      = 65554;   { gamma value }
  TIFFTAG_DCSTOESHOULDERPTS             = 65555;   { toe & shoulder points }
  TIFFTAG_DCSCALIBRATIONFD              = 65556;   { calibration file desc }
  { Note: quality level is on the ZLIB 1-9 scale. Default value is -1 }
  TIFFTAG_ZIPQUALITY                    = 65557;   { compression quality level }
  TIFFTAG_PIXARLOGQUALITY               = 65558;   { PixarLog uses same scale }
  { 65559 is allocated to Oceana Matrix <dev@oceana.com> }
  TIFFTAG_DCSCLIPRECTANGLE              = 65559;   { area of image to acquire }
  TIFFTAG_SGILOGDATAFMT                 = 65560;   { SGILog user data format }
    SGILOGDATAFMT_FLOAT                 = 0;       { IEEE float samples }
    SGILOGDATAFMT_16BIT                 = 1;       { 16-bit samples }
    SGILOGDATAFMT_RAW                   = 2;       { uninterpreted data }
    SGILOGDATAFMT_8BIT                  = 3;       { 8-bit RGB monitor values }
  TIFFTAG_SGILOGENCODE                  = 65561;   { SGILog data encoding control }
    SGILOGENCODE_NODITHER               = 0;       { do not dither encoded values }
    SGILOGENCODE_RANDITHER              = 1;       { randomly dither encd values }


  { Flags to pass to TIFFPrintDirectory to control printing of data structures that are potentially very large. Bit-or these flags to
    enable printing multiple items. }
  TIFFPRINT_NONE                        = $0;      { no extra info }
  TIFFPRINT_STRIPS                      = $1;      { strips/tiles info }
  TIFFPRINT_CURVES                      = $2;      { color/gray response curves }
  TIFFPRINT_COLORMAP                    = $4;      { colormap }
  TIFFPRINT_JPEGQTABLES                 = $100;    { JPEG Q matrices }
  TIFFPRINT_JPEGACTABLES                = $200;    { JPEG AC tables }
  TIFFPRINT_JPEGDCTABLES                = $200;    { JPEG DC tables }


  TIFF_ANY                              = TIFF_NOTYPE;   { for field descriptor searching }
  TIFF_VARIABLE                         = -1;      { marker for variable length tags }
  TIFF_SPP                              = -2;      { marker for SamplesPerPixel tags }
  TIFF_VARIABLE2                        = -3;      { marker for uint32 var-length tags }

  FIELD_CUSTOM                          = 65;

 {added for LibTiff 3.9.4 by Alex (leontyyy@gmail.com) Dec.2011}
  TIFFTAG_EXIFIFD                       = 34665;   { pointer to the Exif IFD }
  EXIFTAG_FOCALLENGTH                   = 37386;   { focal length }
  EXIFTAG_FOCALLENGTHIN35MMFILM         = 41989;   { indicates the equivalent focal length assuming a 35mm film camera, in mm }
  EXIFTAG_EXIFVERSION                   = 36864;   { version of exif format }
  EXIFTAG_DATETIMEDIGITIZED             = 36868;   { date and time when the image was stored as digital data }
  EXIFTAG_DATETIMEORIGINAL              = 36867;   { date and time when the original image data was generated }
  EXIFTAG_EXPOSURETIME                  = 33434;   { exposure time, given in seconds }
  EXIFTAG_FNUMBER                       = 33437;   { F number }
  EXIFTAG_EXPOSUREPROGRAM               = 34850;   { class of the program used by the camera to set exposure }
  EXIFTAG_SPECTRALSENSITIVITY           = 34852;   { spectral sensitivity of each channel of the camera used }
  EXIFTAG_ISOSPEEDRATINGS               = 34855;   { ISO Speed and ISO Latitude }
  EXIFTAG_OECF                          = 34856;   { Opto-Electric Conversion Function }
  EXIFTAG_COMPONENTSCONFIGURATION       = 37121;   { meaning of each component }
  EXIFTAG_COMPRESSEDBITSPERPIXEL        = 37122;   { compression mode }
  EXIFTAG_SHUTTERSPEEDVALUE             = 37377;   { shutter speed }
  EXIFTAG_APERTUREVALUE                 = 37378;   { lens aperture }
  EXIFTAG_BRIGHTNESSVALUE               = 37379;   { brightness }
  EXIFTAG_EXPOSUREBIASVALUE             = 37380;   { exposure bias }
  EXIFTAG_MAXAPERTUREVALUE              = 37381;   { maximum lens aperture }
  EXIFTAG_SUBJECTDISTANCE               = 37382;   { distance to the subject in meters }
  EXIFTAG_METERINGMODE                  = 37383;   { metering mode }
  EXIFTAG_LIGHTSOURCE                   = 37384;   { light source }
  EXIFTAG_FLASH                         = 37385;   { flash }
  EXIFTAG_SUBJECTAREA                   = 37396;   { subject area (in exif ver.2.2) }
  EXIFTAG_MAKERNOTE                     = 37500;   { manufacturer notes }
  EXIFTAG_USERCOMMENT                   = 37510;   { user comments }
  EXIFTAG_SUBSECTIME                    = 37520;   { DateTime subseconds }
  EXIFTAG_SUBSECTIMEORIGINAL            = 37521;   { DateTimeOriginal subseconds }
  EXIFTAG_SUBSECTIMEDIGITIZED           = 37522;   { DateTimeDigitized subseconds }
  EXIFTAG_FLASHPIXVERSION               = 40960;   { FlashPix format version }
  EXIFTAG_COLORSPACE                    = 40961;   { color space information }
  EXIFTAG_PIXELXDIMENSION               = 40962;   { valid image width }
  EXIFTAG_PIXELYDIMENSION               = 40963;   { valid image height }
  EXIFTAG_RELATEDSOUNDFILE              = 40964;   { related audio file }
  EXIFTAG_FLASHENERGY                   = 41483;   { flash energy }
  EXIFTAG_SPATIALFREQUENCYRESPONSE      = 41484;   { spatial frequency response }
  EXIFTAG_FOCALPLANEXRESOLUTION         = 41486;   { focal plane X resolution }
  EXIFTAG_FOCALPLANEYRESOLUTION         = 41487;   { focal plane Y resolution }
  EXIFTAG_FOCALPLANERESOLUTIONUNIT      = 41488;   { focal plane resolution unit }
  EXIFTAG_SUBJECTLOCATION               = 41492;   { subject location }
  EXIFTAG_EXPOSUREINDEX                 = 41493;   { exposure index }
  EXIFTAG_SENSINGMETHOD                 = 41495;   { sensing method }
  EXIFTAG_FILESOURCE                    = 41728;   { file source }
  EXIFTAG_SCENETYPE                     = 41729;   { scene type }
  EXIFTAG_CFAPATTERN                    = 41730;   { CFA pattern }
  EXIFTAG_CUSTOMRENDERED                = 41985;   { custom image processing (in exif ver.2.2) }
  EXIFTAG_EXPOSUREMODE                  = 41986;   { exposure mode (in exif ver.2.2) }
  EXIFTAG_WHITEBALANCE                  = 41987;   { white balance (in exif ver.2.2) }
  EXIFTAG_DIGITALZOOMRATIO              = 41988;   { digital zoom ratio (in exif ver.2.2) }
  EXIFTAG_SCENECAPTURETYPE              = 41990;   { scene capture type (in exif ver.2.2) }
  EXIFTAG_GAINCONTROL                   = 41991;   { gain control (in exif ver.2.2) }
  EXIFTAG_CONTRAST                      = 41992;   { contrast (in exif ver.2.2) }
  EXIFTAG_SATURATION                    = 41993;   { saturation (in exif ver.2.2) }
  EXIFTAG_SHARPNESS                     = 41994;   { sharpness (in exif ver.2.2) }
  EXIFTAG_DEVICESETTINGDESCRIPTION      = 41995;   { device settings description (in exif ver.2.2) }
  EXIFTAG_SUBJECTDISTANCERANGE          = 41996;   { subject distance range (in exif ver.2.2) }
  EXIFTAG_IMAGEUNIQUEID                 = 42016;   { Unique image ID (in exif ver.2.2) }

type

  PTIFF = Pointer;
  PTIFFRGBAImage = Pointer;

  TIFFErrorHandler = procedure(a: Pointer; b: Pointer; c: Pointer); cdecl;
  LibTiffDelphiErrorHandler = procedure(const a,b: AnsiString);
  TIFFReadWriteProc = function(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl;
  TIFFSeekProc = function(Fd: Cardinal; Off: Cardinal; Whence: Integer): Cardinal; cdecl;
  TIFFCloseProc = function(Fd: Cardinal): Integer; cdecl;
  TIFFSizeProc = function(Fd: Cardinal): Cardinal; cdecl;
  TIFFMapFileProc = function(Fd: Cardinal; PBase: PPointer; PSize: PCardinal): Integer; cdecl;
  TIFFUnmapFileProc = procedure(Fd: Cardinal; Base: Pointer; Size: Cardinal); cdecl;
  TIFFExtendProc = procedure(Handle: PTIFF); cdecl;

  TIFFInitMethod = function(Handle: PTIFF; Scheme: Integer): Integer; cdecl;

  PTIFFCodec = ^TIFFCodec;
  TIFFCodec = record
    Name: PAnsiChar;
    Scheme: Word;
    Init: TIFFInitMethod;
  end;

  PTIFFFieldInfo = ^TIFFFieldInfo;
  TIFFFieldInfo = record
    FieldTag: Cardinal;              { field's tag }
    FieldReadCount: Smallint;        { read count/TIFF_VARIABLE/TIFF_SPP }
    FieldWriteCount: Smallint;       { write count/TIFF_VARIABLE }
    FieldType: Integer;              { type of associated data }
    FieldBit: Word;                  { bit in fieldsset bit vector }
    FieldOkToChange: Byte;           { if true, can change while writing }
    FieldPassCount: Byte;            { if true, pass dir count on set }
    FieldName: PAnsiChar;                { ASCII name }
  end;

  PTIFFTagValue = ^TIFFTagValue;
  TIFFTagValue = record
    Info: PTIFFFieldInfo;
    Count: Integer;
    Value: Pointer;
  end;

function  LibTiffDelphiVersion: AnsiString;
function  TIFFGetVersion: PAnsiChar; cdecl; external;
function  TIFFFindCODEC(Scheme: Word): PTIFFCodec; cdecl; external;
function  TIFFRegisterCODEC(Scheme: Word; Name: PAnsiChar; InitMethod: TIFFInitMethod): PTIFFCodec; cdecl; external;
procedure TIFFUnRegisterCODEC(c: PTIFFCodec); cdecl; external;
function  TIFFIsCODECConfigured(Scheme: Word): Integer; cdecl; external;
function  TIFFGetConfiguredCODECs: PTIFFCodec; cdecl; external;

function  TIFFOpen(const Name: AnsiString; const Mode: AnsiString): PTIFF;
function  TIFFOpenStream(const Stream: TStream; const Mode: AnsiString): PTIFF;
function  TIFFClientOpen(Name: PAnsiChar; Mode: PAnsiChar; ClientData: Cardinal;
          ReadProc: TIFFReadWriteProc;
          WriteProc: TIFFReadWriteProc;
          SeekProc: TIFFSeekProc;
          CloseProc: TIFFCloseProc;
          SizeProc: TIFFSizeProc;
          MapProc: TIFFMapFileProc;
          UnmapProc: TIFFUnmapFileProc): PTIFF; cdecl; external;
procedure TIFFCleanup(Handle: PTIFF); cdecl; external;
procedure TIFFClose(Handle: PTIFF); cdecl; external;
function  TIFFFileno(Handle: PTIFF): Integer; cdecl; external;
function  TIFFSetFileno(Handle: PTIFF; Newvalue: Integer): Integer; cdecl; external;
function  TIFFClientdata(Handle: PTIFF): Cardinal; cdecl; external;
function  TIFFSetClientdata(Handle: PTIFF; Newvalue: Cardinal): Cardinal; cdecl; external;
function  TIFFGetMode(Handle: PTIFF): Integer; cdecl; external;
function  TIFFSetMode(Handle: PTIFF; Mode: Integer): Integer; cdecl; external;
function  TIFFFileName(Handle: PTIFF): Pointer; cdecl; external;
function  TIFFSetFileName(Handle: PTIFF; Name: PAnsiChar): PAnsiChar; cdecl; external;
function  TIFFGetReadProc(Handle: PTIFF): TIFFReadWriteProc; cdecl; external;
function  TIFFGetWriteProc(Handle: PTIFF): TIFFReadWriteProc; cdecl; external;
function  TIFFGetSeekProc(Handle: PTIFF): TIFFSeekProc; cdecl; external;
function  TIFFGetCloseProc(Handle: PTIFF): TIFFCloseProc; cdecl; external;
function  TIFFGetSizeProc(Handle: PTIFF): TIFFSizeProc; cdecl; external;
procedure TIFFError(Module: Pointer; Fmt: Pointer); cdecl; external; varargs;
function  TIFFSetErrorHandler(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl; external;
function  LibTiffDelphiGetErrorHandler: LibTiffDelphiErrorHandler;
function  LibTiffDelphiSetErrorHandler(Handler: LibTiffDelphiErrorHandler): LibTiffDelphiErrorHandler;
procedure TIFFWarning(Module: Pointer; Fmt: Pointer); cdecl; external; varargs;
function  TIFFSetWarningHandler(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl; external;
function  LibTiffDelphiGetWarningHandler: LibTiffDelphiErrorHandler;
function  LibTiffDelphiSetWarningHandler(Handler: LibTiffDelphiErrorHandler): LibTiffDelphiErrorHandler;
function  TIFFSetTagExtender(Extender: TIFFExtendProc): TIFFExtendProc; cdecl; external;


function  TIFFFlush(Handle: PTIFF): Integer; cdecl; external;
function  TIFFFlushData(Handle: PTIFF): Integer; cdecl; external;

{added for LibTiff 3.9.4 by Alex (leontyyy@gmail.com) Dec.2011}
function  TIFFReadEXIFDirectory(Handle: PTIFF; Diroff: Cardinal): Integer; cdecl; external;

function  TIFFReadDirectory(Handle: PTIFF): Integer; cdecl; external;
function  TIFFCurrentDirectory(Handle: PTIFF): Word; cdecl; external;
function  TIFFCurrentDirOffset(Handle: PTIFF): Cardinal; cdecl; external;
function  TIFFLastDirectory(Handle: PTIFF): Integer; cdecl; external;
function  TIFFNumberOfDirectories(Handle: PTIFF): Word; cdecl; external;
function  TIFFSetDirectory(Handle: PTIFF; Dirn: Word): Integer; cdecl; external;
function  TIFFSetSubDirectory(Handle: PTIFF; Diroff: Cardinal): Integer; cdecl; external;
function  TIFFCreateDirectory(Handle: PTIFF): Integer; cdecl; external;
function  TIFFWriteDirectory(Handle: PTIFF): Integer; cdecl; external;
function  TIFFUnlinkDirectory(handle: PTIFF; Dirn: Word): Integer; cdecl; external;
procedure TIFFPrintDirectory(Handle: PTIFF; Fd: Pointer; Flags: Integer); cdecl; external;

function  TIFFGetField(Handle: PTIFF; Tag: Cardinal): Integer; cdecl; external; varargs;
function  TIFFGetFieldDefaulted(Handle: PTIFF; Tag: Cardinal): Integer; cdecl; external; varargs;
function  TIFFVGetField(Handle: PTIFF; Tag: Cardinal; Ap: Pointer): Integer; cdecl; external;
function  TIFFSetField(Handle: PTIFF; Tag: Cardinal): Integer; cdecl; external; varargs;
function  TIFFVSetField(Handle: PTIFF; Tag: Cardinal; Ap: Pointer): Integer; cdecl; external;
function  TIFFIsBigEndian(Handle: PTIFF): Integer; cdecl; external;
function  TIFFIsTiled(Handle: PTIFF): Integer; cdecl; external;
function  TIFFIsByteSwapped(Handle: PTIFF): Integer; cdecl; external;
function  TIFFIsUpSampled(Handle: PTIFF): Integer; cdecl; external;
function  TIFFIsMSB2LSB(Handle: PTIFF): Integer; cdecl; external;

function  TIFFGetTagListCount(Handle: PTIFF): Integer; cdecl; external;
function  TIFFGetTagListEntry(Handle: PTIFF; TagIndex: Integer): Cardinal; cdecl; external;
procedure TIFFMergeFieldInfo(Handle: PTIFF; Info: PTIFFFieldInfo; N: Integer); cdecl; external;
function  TIFFFindFieldInfo(Handle: PTIFF; Tag: Cardinal; Dt: Integer): PTIFFFieldInfo; cdecl; external;
function  TIFFFindFieldInfoByName(Handle: PTIFF; FIeldName: PAnsiChar; Dt: Integer): PTIFFFieldInfo; cdecl; external;
function  TIFFFieldWithTag(Handle: PTIFF; Tag: Cardinal): PTIFFFieldInfo; cdecl; external;
function  TIFFFieldWithName(Handle: PTIFF; FieldName: PAnsiChar): PTIFFFieldInfo; cdecl; external;
function  TIFFDataWidth(DataType: Integer): Integer; cdecl; external;

function  TIFFReadRGBAImage(Handle: PTIFF; RWidth,RHeight: Cardinal; Raster: Pointer; Stop: Integer): Integer; cdecl; external;
function  TIFFReadRGBAImageOriented(Handle: PTIFF; RWidth,RHeight: Cardinal; Raster: Pointer; Orientation: Integer; Stop: Integer): Integer; cdecl; external;
function  TIFFReadRGBAStrip(Handle: PTIFF; Row: Cardinal; Raster: Pointer): Integer; cdecl; external;
function  TIFFReadRGBATile(Handle: PTIFF; Col,Row: Cardinal; Raster: Pointer): Integer; cdecl; external;
function  TIFFRGBAImageOk(Handle: PTIFF; Emsg: PAnsiChar): Integer; cdecl; external;
function  TIFFRGBAImageBegin(Img: PTIFFRGBAImage; Handle: PTIFF; Stop: Integer; Emsg: PAnsiChar): Integer; cdecl; external;
function  TIFFRGBAImageGet(Img: PTIFFRGBAImage; Raster: Pointer; W,H: Cardinal): Integer; cdecl; external;
procedure TIFFRGBAImageEnd(Img: PTIFFRGBAImage); cdecl; external;

function  TIFFCurrentRow(Handle: PTIFF): Cardinal; cdecl; external;

function  TIFFStripSize(Handle: PTIFF): Integer; cdecl; external;
function  TIFFRawStripSize(Handle: PTIFF; Strip: Cardinal): Integer; cdecl; external;
function  TIFFVStripSize(Handle: PTIFF; NRows: Cardinal): Integer; cdecl; external;
function  TIFFDefaultStripSize(Handle: PTIFF; Request: Cardinal): Cardinal; cdecl; external;
function  TIFFNumberOfStrips(Handle: PTIFF): Cardinal; cdecl; external;
function  TIFFComputeStrip(Handle: PTIFF; Row: Cardinal; Sample: Word): Cardinal; cdecl; external;
function  TIFFReadRawStrip(Handle: PTIFF; Strip: Cardinal; Buf: Pointer; Size: Integer): Integer; cdecl; external;
function  TIFFReadEncodedStrip(Handle: PTIFF; Strip: Cardinal; Buf: Pointer; Size: Integer): Integer; cdecl; external;
function  TIFFWriteRawStrip(Handle: PTIFF; Strip: Cardinal; Data: Pointer; Cc: Integer): Integer; cdecl; external;
function  TIFFWriteEncodedStrip(Handle: PTIFF; Strip: Cardinal; Data: Pointer; Cc: Integer): Integer; cdecl; external;
function  TIFFCurrentStrip(Handle: PTIFF): Cardinal; cdecl; external;

function  TIFFTileSize(Handle: PTIFF): Integer; cdecl; external;
function  TIFFTileRowSize(Handle: PTIFF): Integer; cdecl; external;
function  TIFFVTileSize(Handle: PTIFF; NRows: Cardinal): Integer; cdecl; external;
procedure TIFFDefaultTileSize(Handle: PTIFF; Tw: PCardinal; Th: PCardinal); cdecl; external;
function  TIFFNumberOfTiles(Handle: PTIFF): Cardinal; cdecl; external;
function  TIFFComputeTile(Handle: PTIFF; X,Y,Z: Cardinal; S: Word): Cardinal; cdecl; external;
function  TIFFReadRawTile(Handle: PTIFF; Tile: Cardinal; Buf: Pointer; Size: Integer): Integer; cdecl; external;
function  TIFFReadEncodedTile(Handle: PTIFF; Tile: Cardinal; Buf: Pointer; Size: Integer): Integer; cdecl; external;
function  TIFFWriteRawTile(Handle: PTIFF; Tile: Cardinal; Data: Pointer; Cc: Integer): Integer; cdecl; external;
function  TIFFWriteEncodedTile(Handle: PTIFF; Tile: Cardinal; Data: Pointer; Cc: Integer): Integer; cdecl; external;
function  TIFFCurrentTile(Handle: PTIFF): Cardinal; cdecl; external;

function  TIFFScanlineSize(Handle: PTIFF): Integer; cdecl; external;
function  TIFFRasterScanlineSize(Handle: PTIFF): Integer; cdecl; external;
function  TIFFReadScanline(Handle: PTIFF; Buf: Pointer; Row: Cardinal; Sample: Word): Integer; cdecl; external;
function  TIFFWriteScanline(Handle: PTIFF; Buf: Pointer; Row: Cardinal; Sample: Word): Integer; cdecl; external;

procedure TIFFSetWriteOffset(Handle: PTIFF; Off: Cardinal); cdecl; external;

procedure TIFFSwabShort(Wp: PWord); cdecl; external;
procedure TIFFSwabLong(Lp: PCardinal); cdecl; external;
procedure TIFFSwabDouble(Dp: PDouble); cdecl; external;
procedure TIFFSwabArrayOfShort(Wp: PWord; N: Cardinal); cdecl; external;
procedure TIFFSwabArrayOfLong(Lp: PCardinal; N: Cardinal); cdecl; external;
procedure TIFFSwabArrayOfDouble(Dp: PDouble; N: Cardinal); cdecl; external;
procedure TIFFReverseBits(Cp: Pointer; N: Cardinal); cdecl; external;
function  TIFFGetBitRevTable(Reversed: Integer): Pointer; cdecl; external;

function  _TIFFmalloc(s: Longint): Pointer; cdecl;
function  _TIFFrealloc(p: Pointer; s: Longint): Pointer; cdecl;
procedure _TIFFfree(p: Pointer); cdecl;

implementation

uses
  Math, LibDelphi, LibJpegDelphi, ZLibDelphi;
  
type

  TCompareFunc = function(a,b: Pointer): Integer; cdecl;

function  floor(x: Double): Double; cdecl; forward;
function  pow(x: Double; y: Double): Double; cdecl; forward;
function  sqrt(x: Double): Double; cdecl; forward;
function  atan2(y: Double; x: Double): Double; cdecl; forward;
function  exp(x: Double): Double; cdecl; forward;
function  log(x: Double): Double; cdecl; forward;
function  fabs(x: Double): Double; cdecl; forward;
function  rand: Integer; cdecl; forward;
function  strlen(s: Pointer): Cardinal; cdecl; forward;
function  strcmp(a: Pointer; b: Pointer): Integer; cdecl; forward;
function  strncmp(a: Pointer; b: Pointer; c: Longint): Integer; cdecl; forward;
procedure qsort(base: Pointer; num: Cardinal; width: Cardinal; compare: TCompareFunc); cdecl; forward;
//DW function  bsearch(key: Pointer; base: Pointer; nelem: Cardinal; width: Cardinal; fcmp: TCompareFunc): Pointer; cdecl; forward;
function  memmove(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl; forward;
function  strchr(s: Pointer; c: Integer): Pointer; cdecl; forward;

procedure _TIFFmemcpy(d: Pointer; s: Pointer; c: Longint); cdecl; forward;
procedure _TIFFmemset(p: Pointer; v: Integer; c: Longint); cdecl; forward;
function  _TIFFmemcmp(buf1: Pointer; buf2: Pointer; count: Cardinal): Integer; cdecl; forward;

var

  _TIFFwarningHandler: TIFFErrorHandler;
  _TIFFerrorHandler: TIFFErrorHandler;
  FLibTiffDelphiWarningHandler: LibTiffDelphiErrorHandler;
  FLibTiffDelphiErrorHandler: LibTiffDelphiErrorHandler;

function fabs(x: Double): Double;
begin
  if x<0 then
    Result:=-x
  else
    Result:=x;
end;

function atan2(y: Double; x: Double): Double;
begin
  Result:=ArcTan2(y,x);
end;

function rand: Integer; cdecl;
begin
  Result:=Trunc(Random*($7FFF+1));
end;

function sqrt(x: Double): Double; cdecl;
begin
  Result:=System.Sqrt(x);
end;

function log(x: Double): Double; cdecl;
begin
  Result:=Ln(x);
end;

function exp(x: Double): Double; cdecl;
begin
  Result:=System.Exp(x);
end;

function strchr(s: Pointer; c: Integer): Pointer; cdecl;
begin
  Result:=s;
  while True do
  begin
    if PByte(Result)^=c then exit;
    if PByte(Result)^=0 then
    begin
      Result:=nil;
      exit;
    end;
    Inc(PByte(Result));
  end;
end;

function memmove(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;
begin
  MoveMemory(dest,src,n);
  Result:=dest;
end;

function _TIFFmemcmp(buf1: Pointer; buf2: Pointer; count: Cardinal): Integer; cdecl;
var
  ma,mb: PByte;
  n: Integer;
begin
  ma:=buf1;
  mb:=buf2;
  n:=0;
  while Cardinal(n)<Count do
  begin
    if ma^<>mb^ then
    begin
      if ma^<mb^ then
        Result:=-1
      else
        Result:=1;
      exit;
    end;
    Inc(ma);
    Inc(mb);
    Inc(n);
  end;
  Result:=0;
end;

procedure _TIFFmemset(p: Pointer; v: Integer; c: Longint); cdecl;
begin
  FillMemory(p,c,v);
end;

(* DW
function bsearch(key: Pointer; base: Pointer; nelem: Cardinal; width: Cardinal; fcmp: TBsearchFcmp): Pointer; cdecl;
begin
  raise Exception.Create('LibTiffDelphi - call to bsearch - should presumably not occur');
end;
*)

procedure qsort(base: Pointer; num: Cardinal; width: Cardinal; compare: TCompareFunc); cdecl;
var
  m: Pointer;
  n: Integer;
  o: Pointer;
  oa,ob,oc: Integer;
  p: Integer;
begin
  if num<2 then exit;
  GetMem(m,num*width);
  if compare(base,Pointer(Cardinal(base)+width))<=0 then
    CopyMemory(m,base,(width shl 1))
  else
  begin
    CopyMemory(m,Pointer(Cardinal(base)+width),width);
    CopyMemory(Pointer(Cardinal(m)+width),base,width);
  end;
  n:=2;
  while Cardinal(n)<num do
  begin
    o:=Pointer(Cardinal(base)+Cardinal(n)*width);
    if compare(m,o)>=0 then
      ob:=0
    else
    begin
      oa:=0;
      ob:=n;
      while oa+1<ob do
      begin
        oc:=((oa+ob) shr 1);
        p:=compare(Pointer(Cardinal(m)+Cardinal(oc)*width),o);
        if p<0 then
          oa:=oc
        else if p=0 then
        begin
          ob:=oc;
          break;
        end
        else
          ob:=oc;
      end;
    end;
    if ob=0 then
    begin
      MoveMemory(Pointer(Cardinal(m)+width),m,Cardinal(n)*width);
      CopyMemory(m,o,width);
    end
    else if ob=n then
      CopyMemory(Pointer(Cardinal(m)+Cardinal(n)*width),o,width)
    else
    begin
      MoveMemory(Pointer(Cardinal(m)+Cardinal(ob+1)*width),Pointer(Cardinal(m)+Cardinal(ob)*width),Cardinal(n-ob)*width);
      CopyMemory(Pointer(Cardinal(m)+Cardinal(ob)*width),o,width);
    end;
    Inc(n);
  end;
  CopyMemory(base,m,num*width);
  FreeMem(m,num*width);
end;

function _TIFFrealloc(p: Pointer; s: Longint): Pointer; cdecl;
begin
  if p=nil then
    Result:=AllocMem(s)
  else
    Result:=ReallocMemory(p,s);
end;

function strncmp(a: Pointer; b: Pointer; c: Longint): Integer; cdecl;
var
  ma,mb: PByte;
  n: Integer;
begin
  ma:=a;
  mb:=b;
  n:=0;
  while n<c do
  begin
    if ma^<>mb^ then
    begin
      if ma^<mb^ then
        Result:=-1
      else
        Result:=1;
      exit;
    end;
    if ma^=0 then
    begin
      Result:=0;
      exit;
    end;
    Inc(ma);
    Inc(mb);
    Inc(n);
  end;
  Result:=0;
end;

function strcmp(a: Pointer; b: Pointer): Integer; cdecl;
var
  ma,mb: PByte;
begin
  ma:=a;
  mb:=b;
  while True do
  begin
    if ma^<>mb^ then
    begin
      if ma^<mb^ then
        Result:=-1
      else
        Result:=1;
      exit;
    end;
    if ma^=0 then
    begin
      Result:=0;
      exit;
    end;
    Inc(ma);
    Inc(mb);
  end;
  Result:=0;
end;

function strlen(s: Pointer): Cardinal; cdecl;
var
  m: PByte;
begin
  Result:=0;
  m:=s;
  while m^<>0 do
  begin
    Inc(Result);
    Inc(m);
  end;
end;

procedure _TIFFfree(p: Pointer); cdecl;
begin
  FreeMem(p);
end;

procedure _TIFFmemcpy(d: Pointer; s: Pointer; c: Longint); cdecl;
begin
  CopyMemory(d,s,c);
end;

function pow(x: Double; y: Double): Double; cdecl;
begin
  Result:=Power(x,y);
end;

function floor(x: Double): Double; cdecl;
begin
  Result:=Trunc(x);
end;

function _TIFFmalloc(s: Longint): Pointer; cdecl;
begin
  Result:=AllocMem(s);
end;

{LibTiffDelphi}

function  LibTiffDelphiVersion: AnsiString;
var
  m: AnsiString;
  na,nb: Integer;
begin
  Result:='';
  m:=TIFFGetVersion;
  na:=1;
  while True do
  begin
    nb:=na;
    while nb<=Length(m) do
    begin
      if m[nb]=#10 then break;
      Inc(nb);
    end;
    Result:=Result+System.Copy(m,na,nb-na);
    if nb>Length(m) then break;
    Result:=Result+#13#10;
    na:=nb+1;
  end;
  Result:=Result+#13#10+LibTiffDelphiVersionString;
end;

procedure LibTiffDelphiWarningThrp(a: Pointer; b: Pointer; c: Pointer); cdecl;
var
  m: Integer;
  n: AnsiString;
begin
  if @FLibTiffDelphiWarningHandler<>nil then
  begin
    m:=sprintfsec(nil,b,@c);
    SetLength(n,m);
    sprintfsec(Pointer(n),b,@c);
    FLibTiffDelphiWarningHandler(PAnsiChar(a),n);
  end;
end;

procedure LibTiffDelphiErrorThrp(a: Pointer; b: Pointer; c: Pointer); cdecl;
var
  m: Integer;
  n: AnsiString;
begin
  if @FLibTiffDelphiErrorHandler<>nil then
  begin
    m:=sprintfsec(nil,b,@c);
    SetLength(n,m);
    sprintfsec(Pointer(n),b,@c);
    FLibTiffDelphiErrorHandler(PAnsiChar(a),n);
  end;
end;

function LibTiffDelphiGetWarningHandler: LibTiffDelphiErrorHandler;
begin
  Result:=FLibTiffDelphiWarningHandler;
end;

function LibTiffDelphiSetWarningHandler(Handler: LibTiffDelphiErrorHandler): LibTiffDelphiErrorHandler;
begin
  Result:=FLibTiffDelphiWarningHandler;
  FLibTiffDelphiWarningHandler:=Handler;
end;

function LibTiffDelphiGetErrorHandler: LibTiffDelphiErrorHandler;
begin
  Result:=FLibTiffDelphiErrorHandler;
end;

function LibTiffDelphiSetErrorHandler(Handler: LibTiffDelphiErrorHandler): LibTiffDelphiErrorHandler;
begin
  Result:=FLibTiffDelphiErrorHandler;
  FLibTiffDelphiErrorHandler:=Handler;
end;

{tif_read}

procedure _TIFFSwab16BitData(tif: Pointer; buf: Pointer; cc: Integer); cdecl; external;
procedure _TIFFSwab24BitData(tif: pointer; buf: pointer; cc: integer); cdecl; external; //DW 3.8.2
procedure _TIFFSwab32BitData(tif: Pointer; buf: Pointer; cc: Integer); cdecl; external;
procedure _TIFFSwab64BitData(tif: Pointer; buf: Pointer; cc: Integer); cdecl; external;
procedure _TIFFNoPostDecode(tif: Pointer; buf: Pointer; cc: Integer); cdecl; external;
function  TIFFReadTile(tif: Pointer; buf: Pointer; x: Cardinal; y: Cardinal; z: Cardinal; s: Word): Integer; cdecl; external;
function TIFFFillTile(tif: Pointer; tile: longword):integer; cdecl; external; //DW 3.8.2

{$L Compiled\tif_read.obj}

{tif_dirinfo}

function  _TIFFSampleToTagType(tif: Pointer): Integer; cdecl; external;
procedure _TIFFSetupFieldInfo(tif: Pointer); cdecl; external;
function  _TIFFCreateAnonFieldInfo(tif: Pointer; tag: Cardinal; field_type: Integer): Pointer; cdecl; external;
function _TIFFGetExifFieldInfo(size : plongint):pointer; cdecl; external; //DW 3.8.2
function _TIFFDataSize(TIFFDataType : longint):longint; cdecl; external; //DW 3.8.2
function _TIFFGetFieldInfo(size : plongint):pointer; cdecl; external; //DW 3.8.2
function _TIFFMergeFieldInfo(tif: Pointer; fieldinfo : Pointer; n : Integer):Integer; cdecl; external; //DW 3.9.1

{$L Compiled\tif_dirinfo.obj}

{tif_dirwrite}

{$L Compiled\tif_dirwrite.obj}

{tif_flush}

{$L Compiled\tif_flush.obj}

{tif_write}

function  TIFFFlushData1(tif: Pointer): Integer; cdecl; external;
function  TIFFSetupStrips(tif: Pointer): Integer; cdecl; external;

{$L Compiled\tif_write.obj}

{tif_dumpmode}

function  TIFFInitDumpMode(tif: Pointer; scheme: Integer): Integer; cdecl; external;

{$L Compiled\tif_dumpmode.obj}

{tif_compress}

function  TIFFSetCompressionScheme(tif: Pointer; scheme: Integer): Integer; cdecl; external;
procedure _TIFFSetDefaultCompressionState(tif: Pointer); cdecl; external;

{$L Compiled\tif_compress.obj}

{tif_dirread}

{$L Compiled\tif_dirread.obj}

{tif_dir}

procedure TIFFFreeDirectory(tif: Pointer); cdecl; external;
function  TIFFDefaultDirectory(tif: Pointer): Integer; cdecl; external;
function  TIFFReassignTagToIgnore(task: Integer; TIFFtagID: Integer): Integer; cdecl; external;
procedure _TIFFsetString(cpp: Pointer; cp: Pointer); cdecl; external;
procedure _TIFFsetByteArray(vpp: Pointer; vp: Pointer; n: Integer); cdecl; external;

{$L Compiled\tif_dir.obj}

{tif_aux}

function  TIFFVGetFieldDefaulted(tif: Pointer; tag: Cardinal; ap: Pointer): Integer; cdecl; external;

{$L Compiled\tif_aux.obj}

{tif_color}

procedure TIFFCIELabToXYZ(cielab: Pointer; l: Cardinal; a: Integer; b: Integer; X: Pointer; Y: Pointer; Z: Pointer); cdecl; external;
procedure TIFFXYZToRGB(cielab: Pointer; X: Single; Y: Single; Z: Single; r: Pointer; g: Pointer; b: Pointer); cdecl; external;
procedure TIFFYCbCrtoRGB(ycbcr: Pointer; Y: Cardinal; Cb: Integer; Cr: Integer; r: Pointer; g: Pointer; b: Pointer); cdecl; external;
function  TIFFYCbCrToRGBInit(ycbcr: Pointer; luma: Pointer; refBlackWhite: Pointer): Integer; cdecl; external;
function  TIFFCIELabToRGBInit(cielab: Pointer; display: Pointer; refWhite: Pointer): Integer; cdecl; external;

{$L Compiled\tif_color.obj}

{tif_close}

{$L Compiled\tif_close.obj}

{tif_extension}

{$L Compiled\tif_extension.obj}

{tif_open}

function  _TIFFgetMode(mode: PAnsiChar; module: PAnsiChar): Integer; cdecl; external;

{$L Compiled\tif_open.obj}

{tif_getimage}

{$L Compiled\tif_getimage.obj}

{tif_predict}

function  TIFFPredictorInit(tif: PTIFF): Integer; cdecl; external;
function  TIFFPredictorCleanup(tif: PTIFF):integer; cdecl; external; //DW 3.8.2

{$L Compiled\tif_predict.obj}

{tif_print}

{$L Compiled\tif_print.obj}

{tif_error}

{$L Compiled\tif_error.obj}

{tif_strip}

function  _TIFFDefaultStripSize(tif: Pointer; s: Cardinal): Cardinal; cdecl; external;
function TIFFOldScanlineSize(tif: Pointer):Cardinal; cdecl; external; //DW 3.9.1

{$L Compiled\tif_strip.obj}

{tif_swab}

{$L Compiled\tif_swab.obj}

{tif_tile}

function  TIFFCheckTile(tif: Pointer; x: Cardinal; y: Cardinal; z: Cardinal; s: Word): Integer; cdecl; external;
procedure _TIFFDefaultTileSize(tif: Pointer; tw: Pointer; th: Pointer); cdecl; external;

{$L Compiled\tif_tile.obj}

{tif_warning}

{$L Compiled\tif_warning.obj}

{tif_fax3}

function  TIFFInitCCITTRLE(tif: PTIFF; scheme: Integer): Integer; cdecl; external;
function  TIFFInitCCITTRLEW(tif: PTIFF; scheme: Integer): Integer; cdecl; external;
function  TIFFInitCCITTFax3(tif: PTIFF; scheme: Integer): Integer; cdecl; external;
function  TIFFInitCCITTFax4(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L Compiled\tif_fax3.obj}

{tif_fax3sm}

{$L Compiled\tif_fax3sm.obj}

{tif_jpeg}

procedure TIFFjpeg_error_exit_raise; cdecl;
begin
  raise Exception.Create('TIFFjpeg_error_exit');
end;

function TIFFcallvjpeg_jpeg_CreateCompress(cinfo: Pointer; version: Integer; structsize: Cardinal): Integer; cdecl;
begin
  try
    jpeg_CreateCompress(cinfo,version,structsize);
    Result:=1;
  except
    Result:=0;
  end;
end;

function TIFFcallvjpeg_jpeg_CreateDecompress(cinfo: Pointer; version: Integer; structsize: Cardinal): Integer; cdecl;
begin
  try
    jpeg_CreateDecompress(cinfo,version,structsize);
    Result:=1;
  except
    Result:=0;
  end;
end;

function TIFFcallvjpeg_jpeg_set_defaults(cinfo: Pointer): Integer; cdecl;
begin
  try
    jpeg_set_defaults(cinfo);
    Result:=1;
  except
    Result:=0;
  end;
end;

function TIFFcallvjpeg_jpeg_set_colorspace(cinfo: Pointer; colorspace: Integer): Integer; cdecl;
begin
  try
    jpeg_set_colorspace(cinfo, colorspace);
    Result:=1;
  except
    Result:=0;
  end;
end;

function TIFFcallvjpeg_jpeg_set_quality(cinfo: Pointer; quality: Integer; force_baseline: Byte): Integer; cdecl;
begin
  try
    jpeg_set_quality(cinfo,quality,force_baseline);
    Result:=1;
  except
    Result:=0;
  end;
end;

function TIFFcallvjpeg_jpeg_suppress_tables(cinfo: PRJpegCompressStruct; suppress: Byte): Integer; cdecl;
begin
  try
    jpeg_suppress_tables(cinfo,suppress);
    Result:=1;
  except
    Result:=0;
  end;
end;

function TIFFcallvjpeg_jpeg_start_compress(cinfo: PRJpegCompressStruct; write_all_tables: Byte): Integer; cdecl;
begin
  try
    jpeg_start_compress(cinfo,write_all_tables);
    Result:=1;
  except
    Result:=0;
  end;
end;

function TIFFcalljpeg_jpeg_write_scanlines(errreturn: Integer; cinfo: PRJpegCompressStruct; scanlines: Pointer; num_lines: Cardinal): Integer; cdecl;
begin
  try
    Result:=jpeg_write_scanlines(cinfo,scanlines,num_lines);
  except
    Result:=errreturn;
  end;
end;

function TIFFcalljpeg_jpeg_write_raw_data(errreturn: Integer; cinfo: PRJpegCompressStruct; data: Pointer; num_lines: Cardinal): Integer; cdecl;
begin
  try
    Result:=jpeg_write_raw_data(cinfo,data,num_lines);
  except
    Result:=errreturn;
  end;
end;

function TIFFcallvjpeg_jpeg_finish_compress(cinfo: PRJpegCompressStruct): Integer; cdecl;
begin
  try
    jpeg_finish_compress(cinfo);
    Result:=1;
  except
    Result:=0;
  end;
end;

function TIFFcallvjpeg_jpeg_write_tables(cinfo: PRJpegCompressStruct): Integer; cdecl;
begin
  try
    jpeg_write_tables(cinfo);
    Result:=1;
  except
    Result:=0;
  end;
end;

function TIFFcalljpeg_jpeg_read_header(errreturn: Integer; cinfo: PRJpegDecompressStruct; require_image: Byte): Integer; cdecl;
begin
  try
    Result:=jpeg_read_header(cinfo,Boolean(require_image));
  except
    Result:=errreturn;
  end;
end;

function TIFFcallvjpeg_jpeg_start_decompress(cinfo: PRJpegDecompressStruct): Integer; cdecl;
begin
  try
    jpeg_start_decompress(cinfo);
    Result:=1;
  except
    Result:=0;
  end;
end;

function TIFFcalljpeg_jpeg_read_scanlines(errreturn: Integer; cinfo: PRJpegDecompressStruct; scanlines: Pointer; max_lines: Cardinal): Integer; cdecl;
begin
  try
    Result:=jpeg_read_scanlines(cinfo,scanlines,max_lines);
  except
    Result:=errreturn;
  end;
end;

function TIFFcalljpeg_jpeg_read_raw_data(errreturn: Integer; cinfo: PRJpegDecompressStruct; data: Pointer; max_lines: Cardinal): Integer; cdecl;
begin
  try
    Result:=jpeg_read_raw_data(cinfo,data,max_lines);
  except
    Result:=errreturn;
  end;
end;

function TIFFcalljpeg_jpeg_finish_decompress(errreturn: Integer; cinfo: PRJpegDecompressStruct): Integer; cdecl;
begin
  try
    Result:=jpeg_finish_decompress(cinfo);
  except
    Result:=errreturn;
  end;
end;

function TIFFcallvjpeg_jpeg_abort(cinfo: PRJpegCommonStruct): Integer; cdecl;
begin
  try
    jpeg_abort(cinfo);
    Result:=1;
  except
    Result:=0;
  end;
end;

function TIFFcallvjpeg_jpeg_destroy(cinfo: PRJpegCommonStruct): Integer; cdecl;
begin
  try
    jpeg_destroy(cinfo);
    Result:=1;
  except
    Result:=0;
  end;
end;

type
  jpeg_alloc_sarray = function(cinfo: PRJpegCommonStruct; pool_id: Integer; samplesperrow: Cardinal; numrows: Cardinal): Pointer; cdecl;

function TIFFcalljpeg_alloc_sarray(alloc_sarray: jpeg_alloc_sarray; cinfo: PRJpegCommonStruct; pool_id: Integer; samplesperrow: Cardinal;
                    numrows: Cardinal): Pointer; cdecl;
begin
  try
    Result:=alloc_sarray(cinfo,pool_id,samplesperrow,numrows);
  except
    Result:=nil;
  end;
end;

function  TIFFInitJPEG(tif: PTIFF; scheme: Integer): Integer; cdecl; external;
function  TIFFFillStrip(tif : PTIFF; Len : longword): integer; cdecl; external; //DW 3.8.2

{$L Compiled\tif_jpeg.obj}

{tif_luv}

function  TIFFInitSGILog(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L Compiled\tif_luv.obj}

{tif_lzw}

function  TIFFInitLZW(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L Compiled\tif_lzw.obj}

{tif_next}

function  TIFFInitNeXT(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L Compiled\tif_next.obj}

{tif_packbits}

function  TIFFInitPackBits(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L Compiled\tif_packbits.obj}

{tif_pixarlog}

function  TIFFInitPixarLog(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L Compiled\tif_pixarlog.obj}

{tif_thunder}

function  TIFFInitThunderScan(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L Compiled\tif_thunder.obj}

{tif_version}

{$L Compiled\tif_version.obj}

{tif_zip}

function  TIFFInitZIP(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L Compiled\tif_zip.obj}

{tif_codec}

function NotConfigured(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{DW 
const

  _TIFFBuiltinCODECS: array[0..17] of TIFFCodec = (
       (name:'None'; scheme: COMPRESSION_NONE; init: TIFFInitDumpMode),
       (name:'LZW'; scheme: COMPRESSION_LZW; init: TIFFInitLZW),
       (name:'PackBits'; scheme: COMPRESSION_PACKBITS; init: TIFFInitPackBits),
       (name:'ThunderScan'; scheme: COMPRESSION_THUNDERSCAN; init: TIFFInitThunderScan),
       (name:'NeXT'; scheme: COMPRESSION_NEXT; init: TIFFInitNeXT),
       (name:'JPEG'; scheme: COMPRESSION_JPEG; init: TIFFInitJPEG),
       (name:'Old-style JPEG'; scheme: COMPRESSION_OJPEG; init: NotConfigured),
       (name:'CCITT RLE'; scheme: COMPRESSION_CCITTRLE; init: TIFFInitCCITTRLE),
       (name:'CCITT RLE/W'; scheme: COMPRESSION_CCITTRLEW; init: TIFFInitCCITTRLEW),
       (name:'CCITT Group 3'; scheme: COMPRESSION_CCITTFAX3; init: TIFFInitCCITTFax3),
       (name:'CCITT Group 4'; scheme: COMPRESSION_CCITTFAX4; init: TIFFInitCCITTFax4),
       (name:'ISO JBIG'; scheme: COMPRESSION_JBIG; init: NotConfigured),
       (name:'Deflate'; scheme: COMPRESSION_DEFLATE; init: TIFFInitZIP),
       (name:'AdobeDeflate'; scheme: COMPRESSION_ADOBE_DEFLATE; init: TIFFInitZIP),
       (name:'PixarLog'; scheme: COMPRESSION_PIXARLOG; init: TIFFInitPixarLog),
       (name:'SGILog'; scheme: COMPRESSION_SGILOG; init: TIFFInitSGILog),
       (name:'SGILog24'; scheme: COMPRESSION_SGILOG24; init: TIFFInitSGILog),
       (name:nil; scheme:0; init:nil));
}

{$L Compiled\tif_codec.obj}

{LibTiffDelphi}

function  TIFFFileReadProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl; forward;
function  TIFFFileWriteProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl; forward;
function  TIFFFileSizeProc(Fd: Cardinal): Cardinal; cdecl; forward;
function  TIFFFileSeekProc(Fd: Cardinal; Off: Cardinal; Whence: Integer): Cardinal; cdecl; forward;
function  TIFFFileCloseProc(Fd: Cardinal): Integer; cdecl; forward;

function  TIFFStreamReadProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl; forward;
function  TIFFStreamWriteProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl; forward;
function  TIFFStreamSizeProc(Fd: Cardinal): Cardinal; cdecl; forward;
function  TIFFStreamSeekProc(Fd: Cardinal; Off: Cardinal; Whence: Integer): Cardinal; cdecl; forward;
function  TIFFStreamCloseProc(Fd: Cardinal): Integer; cdecl; forward;

function  TIFFNoMapProc(Fd: Cardinal; PBase: PPointer; PSize: PCardinal): Integer; cdecl; forward;
procedure TIFFNoUnmapProc(Fd: Cardinal; Base: Pointer; Size: Cardinal); cdecl; forward;

function TIFFFileCloseProc(Fd: Cardinal): Integer; cdecl;
begin
  if CloseHandle(Fd)=True then
    Result:=0
  else
    Result:=-1;
end;

function TIFFFileSizeProc(Fd: Cardinal): Cardinal; cdecl;
begin
  Result:=GetFileSize(Fd,nil);
end;

function TIFFFileSeekProc(Fd: Cardinal; Off: Cardinal; Whence: Integer): Cardinal; cdecl;
const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
var
  MoveMethod: Cardinal;
begin
  if Off=$ffffffff then
  begin
    Result:=$ffffffff;
    exit;
  end;
  case Whence of
    SEEK_SET: MoveMethod:=FILE_BEGIN;
    SEEK_CUR: MoveMethod:=FILE_CURRENT;
    SEEK_END: MoveMethod:=FILE_END;
  else
    MoveMethod:=FILE_BEGIN;
  end;
  Result:=SetFilePointer(Fd,Off,nil,MoveMethod);
end;

function TIFFFileReadProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl;
var
  m: Cardinal;
begin
  if ReadFile(Fd,Buffer^,Cardinal(Size),m,nil)=False then
    Result:=0
  else
    Result:=m;
end;

function TIFFFileWriteProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl;
var
  m: Cardinal;
begin
  if WriteFile(Fd,Buffer^,Cardinal(Size),m,nil)=False then
    Result:=0
  else
    Result:=m;
end;

function TIFFStreamCloseProc(Fd: Cardinal): Integer; cdecl;
begin
  Result:=0;
end;

function TIFFStreamSizeProc(Fd: Cardinal): Cardinal; cdecl;
begin
  try
    Result:=TStream(Fd).Size;
  except
    Result:=0;
  end;
end;

function TIFFStreamSeekProc(Fd: Cardinal; Off: Cardinal; Whence: Integer): Cardinal; cdecl;
const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
var
  MoveMethod: Word;
begin
  if Off=$ffffffff then
  begin
    Result:=$ffffffff;
    exit;
  end;
  case Whence of
    SEEK_SET: MoveMethod:=soFromBeginning;
    SEEK_CUR: MoveMethod:=soFromCurrent;
    SEEK_END: MoveMethod:=soFromEnd;
  else
    MoveMethod:=soFromBeginning;
  end;
  try
    Result:=TStream(Fd).Seek(Off,MoveMethod);
  except
    Result:=0;
  end;
end;

function TIFFStreamReadProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl;
begin
  try
    Result:=TStream(Fd).Read(Buffer^,Size);
  except
    Result:=0;
  end;
end;

function TIFFStreamWriteProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl;
begin
  try
    Result:=TStream(Fd).Write(Buffer^,Size);
  except
    Result:=0;
  end;
end;

function TIFFNoMapProc(Fd: Cardinal; PBase: PPointer; PSize: PCardinal): Integer; cdecl;
begin
  Result:=0;
end;

procedure TIFFNoUnmapProc(Fd: Cardinal; Base: Pointer; Size: Cardinal); cdecl;
begin
end;

function TIFFOpen(const Name: AnsiString; const Mode: AnsiString): PTIFF;
const
  Module: AnsiString = 'TIFFOpen';
  O_RDONLY = 0;
  O_WRONLY = 1;
  O_RDWR = 2;
  O_CREAT = $0100;
  O_TRUNC = $0200;
var
  m: Integer;
  DesiredAccess: Cardinal;
  CreateDisposition: Cardinal;
  FlagsAndAttributes: Cardinal;
  fd: THandle;
begin
  m:=_TIFFgetMode(PAnsiChar(Mode),PAnsiChar(Module));
  if m=o_RDONLY then
    DesiredAccess:=GENERIC_READ
  else
    DesiredAccess:=(GENERIC_READ or GENERIC_WRITE);
  case m of
    O_RDONLY: CreateDisposition:=OPEN_EXISTING;
    O_RDWR: CreateDisposition:=OPEN_ALWAYS;
    (O_RDWR or O_CREAT): CreateDisposition:=OPEN_ALWAYS;
    (O_RDWR or O_TRUNC): CreateDisposition:=CREATE_ALWAYS;
    (O_RDWR or O_CREAT or O_TRUNC): CreateDisposition:=CREATE_ALWAYS;
  else
    Result:=nil;
    exit;
  end;
  if m=O_RDONLY then
    FlagsAndAttributes:=FILE_ATTRIBUTE_READONLY
  else
    FlagsAndAttributes:=FILE_ATTRIBUTE_NORMAL;
  fd:=CreateFileA(PAnsiChar(Name),DesiredAccess,FILE_SHARE_READ,nil,CreateDisposition,FlagsAndAttributes,0);
  if fd=INVALID_HANDLE_VALUE then
  begin
    TiffError(PAnsiChar(Module),PAnsiChar('%s: Cannot open'),PAnsiChar(Name));
    Result:=nil;
    exit;
  end;
  Result:=TIFFClientOpen(PAnsiChar(Name),PAnsiChar(Mode),fd,@TIFFFileReadProc,@TIFFFileWriteProc,@TIFFFileSeekProc,@TIFFFileCloseProc,
              @TIFFFileSizeProc,@TIFFNoMapProc,@TIFFNoUnmapProc);
  if Result<>nil then
    TIFFSetFileno(Result,fd)
  else
    CloseHandle(fd);
end;

function TIFFOpenStream(const Stream: TStream; const Mode: AnsiString): PTIFF;
var
  m: AnsiString;
begin
  m:='Stream';
  Result:=TIFFClientOpen(PAnsiChar(m),PAnsiChar(Mode),Cardinal(Stream),@TIFFStreamReadProc,@TIFFStreamWriteProc,@TIFFStreamSeekProc,@TIFFStreamCloseProc,
              @TIFFStreamSizeProc,@TIFFNoMapProc,@TIFFNoUnmapProc);
  if Result<>nil then TIFFSetFileno(Result,Cardinal(Stream));
end;


initialization

  _TIFFwarningHandler:=LibTiffDelphiWarningThrp;
  _TIFFerrorHandler:=LibTiffDelphiErrorThrp;

end.

