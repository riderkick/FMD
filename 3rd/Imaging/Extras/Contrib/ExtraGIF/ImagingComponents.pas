{
  $Id: ImagingComponents.pas 169 2009-08-22 18:54:21Z galfar $
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

{ This unit contains VCL/LCL TGraphic descendant which uses Imaging library
  for saving and loading.
  
  Note: This modified unit is part of ExtraGIF mod of Imaging library.}
unit ImagingComponents;

{$I ImagingOptions.inc}

interface

{$IFDEF LCL}
  {$DEFINE COMPONENT_SET_LCL}
{$ENDIF}

{$IF not Defined(COMPONENT_SET_LCL) and not Defined(COMPONENT_SET_VCL)}
// If no component sets should be used just include empty unit.
implementation
{$ELSE}

uses
  SysUtils, Types, Classes,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF COMPONENT_SET_VCL}
  Graphics,
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  InterfaceBase,
  GraphType,
  Graphics,
  LCLType,
  LCLIntf,
{$ENDIF}
{$IFNDEF DONT_LINK_GIF}
  ImagingGif,
{$ENDIF}
  ImagingTypes, Imaging, ImagingClasses, ImagingCanvases, ImagingFormats;

type
  TRefMultiImage = class(TMultiImage)
  private
    FRefCount: Integer;
  public
    constructor Create; override;
    procedure ReferredFree(const DeleteFromList: Boolean = True);
    function GetCopy: TRefMultiImage;
    property RefCount: Integer read FRefCount;
  end;

  { Graphic class which uses Imaging to load images.
    It has standard TBitmap class as ancestor and it can
    Assign also to/from TImageData structres and TBaseImage
    classes. For saving is uses inherited TBitmap methods.
    This class is automatically registered to TPicture for all
    file extensions supported by Imaging (useful only for loading).
    If you just want to load images in various formats you can use this
    class or simply use  TPicture.LoadFromXXX which will create this class
    automatically. For TGraphic class that saves with Imaging look
    at TImagingGraphicForSave class.}
  TLoadingState = (lsEmpty, lsLoading, lsLoaded);

  {$WARNINGS OFF}
  TImagingGraphic = class(TGraphic)
  private
    FMultImage: TRefMultiImage;
    FWaitCounter: Integer;
    FBackGroundBitmap: TBitmap;
    FBgColor: TColor;
    FSelfAnimated, FAddedToList: Boolean;
    FActiveImage: LongInt;
    FLoadState: TLoadingState;
    FLastCanvas: TCanvas;
    FLastRect: TRect;
    FReAnimate: Boolean;
    FBGShare: Boolean;

    FIsReferenced: Boolean;
    procedure SetSelfAnimated(const Value: Boolean);
    procedure SetBackGroundBitmap(const Value: TBitmap);
    procedure SetActiveImgIndex(const Value: Integer);
    procedure DoPrepareFrame(const FrameIndex: Integer; var Frame: TImageData; Back: PImageData);
  protected
    //done before LoadFromStream
    procedure BeforeLoad(Stream: TStream); virtual;
    //internal LoadFromStream according to Reference Count
    procedure ReadDataFromStream(Stream: TStream); virtual;
    //done after LoadFromStream
    procedure AfterLoad(Stream: TStream); virtual;
    procedure AssignTo(Dest: TPersistent); override;

    //call this method every time before Draw, in case of gif this will copy the palette to frame
    procedure PrepareFrame(const FrameIndex: Integer; var Frame: TImageData; Back: PImageData); virtual;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;

    //those three methods are used when SelfAnimated switched on
    procedure DoPaintTriggered; virtual;
    function GetCurrentDelay: LongInt;
    procedure PaintTriggered;
    //workaround to reduce processor-time while graphic is invisible
    function ParentControlVisible: Boolean;
  public

    constructor Create; overload; override;
    constructor Create(const Referenced: Boolean); overload;

    destructor Destroy; override;

    procedure SaveToStream(Stream: TStream); override;

    { Loads new image from the stream. It can load all image
      file formats supported by Imaging (and enabled of course)
      even though it is called by descendant class capable of
      saving only one file format.}
    procedure LoadFromStream(Stream: TStream); override;
    { Copies the image contained in Source to this graphic object.
      Supports also TBaseImage descendants from ImagingClasses unit. }
    procedure Assign(Source: TPersistent); override;
    { Copies the image contained in TBaseImage to this graphic object.}
    procedure AssignFromImage(Image: TBaseImage);
    { Copies the current image to TBaseImage object.}
    procedure AssignToImage(Image: TBaseImage);
    { Copies the image contained in TImageData structure to this graphic object.}
    procedure AssignFromImageData(const ImageData: TImageData);
    { Copies the current image to TImageData structure.}
    procedure AssignToImageData(var ImageData: TImageData);
    { Returns TImageFileFormat descendant for this graphic class.}
    class function GetFileFormat: TImageFileFormat; virtual;
    {more fast way to resize the graphic}
    procedure SetSize(AWidth: Integer; AHeight: Integer); {$IF CompilerVersion >= 18}override;{$IFEND}
    {call this method before any change to MultiImage (so other copies won't be changed)}
    procedure UnRefImage;

    property Frames: TRefMultiImage read FMultImage;
    property ActiveIndex: Integer read FActiveImage write SetActiveImgIndex;
    //background for transparent drawing
    property BackGroundBitmap: TBitmap read FBackGroundBitmap write SetBackGroundBitmap;
    //set this property to True if you control BackgroundBitmap yourself
    property BackgroundSharing: Boolean read FBGShare write FBGShare default False;
    //if backgroundbitmap is nil, then this color is used for transparency
    property BackGroundColor: TColor read FBgColor write FBgColor;

    //this property forces the self-animation
    property SelfAnimated: Boolean read FSelfAnimated write SetSelfAnimated default True;
    //set this property to False after removing from animation List to avoid re-adding
    property ReAnimate: Boolean read FReAnimate write FReAnimate default True;
    property LoadState: TLoadingState read FLoadState;
    { TODO -oSega-Zero : make a code for ClipboardFormat recognition }
    procedure LoadFromClipboardFormat(AFormat: Word; AData: Cardinal;
      APalette: HPALETTE);
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: Cardinal;
      var APalette: HPALETTE);
  end;
  {$WARNINGS ON}
  TImagingGraphicClass = class of TImagingGraphic;

  {$IFNDEF DONT_LINK_GIF}
  TImagingGifGraphic = class(TImagingGraphic)
  private
    FRepeatCount: Integer;
    FLastFrame: TImageData;
    FCacheIndex: Integer;
    procedure DoOverlay(const FrameIndex: Integer; var Frame, BG: TImageData);
  protected
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure AfterLoad(Stream: TStream); override;
    procedure PrepareFrame(const FrameIndex: Integer; var Frame: TImageData; Back: PImageData); override;
    procedure DoPaintTriggered; override;
  public
    {returns GIF Extra or nil if not found}
    function GifData(const Index: Integer): TGifExtraData;
    class function GetFileFormat: TImageFileFormat; override;
    destructor Destroy; override;
  end;
  TImagingGifGraphicClass = class of TImagingGifGraphic;
  TGIFImage = TImagingGifGraphic;
  {$ENDIF}

  { Base class for file format specific TGraphic classes that use
    Imaging for saving. Each descendant class can load all file formats
    supported by Imaging but save only one format (TImagingBitmap
    for *.bmp, TImagingJpeg for *.jpg). Format specific classes also
    allow easy access to Imaging options that affect saving of files
    (they are properties here).}
  TImagingGraphicForSave = class(TImagingGraphic)
  protected
    FDefaultFileExt: string;
    FSavingFormat: TImageFormat;
    procedure WriteDataToStream(Stream: TStream); virtual;
  public
    constructor Create; override;
    { Saves the current image to the stream. It is saved in the
      file format according to the DefaultFileExt property.
      So each descendant class can save some other file format.}
    procedure SaveToStream(Stream: TStream); override;
  {$IFDEF COMPONENT_SET_LCL}
    { Returns file extensions of this graphic class.}
    class function GetFileExtensions: string; override;
    { Returns default MIME type of this graphic class.}
    function GetMimeType: string; override;
  {$ENDIF}
    { Default (the most common) file extension of this graphic class.}
    property DefaultFileExt: string read FDefaultFileExt;
  end;

  TImagingGraphicForSaveClass = class of TImagingGraphicForSave;

{$IFNDEF DONT_LINK_BITMAP}
  { TImagingGraphic descendant for loading/saving Windows bitmaps.
    VCL/CLX/LCL all have native support for bitmaps so you might
    want to disable this class (although you can save bitmaps with
    RLE compression with this class).}
  TImagingBitmap = class(TImagingGraphicForSave)
  protected
    FUseRLE: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { See ImagingBitmapRLE option for details.}
    property UseRLE: Boolean read FUseRLE write FUseRLE;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_JPEG}
  { TImagingGraphic descendant for loading/saving JPEG images.}
  TImagingJpeg = class(TImagingGraphicForSave)
  protected
    FQuality: LongInt;
    FProgressive: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
  {$IFDEF COMPONENT_SET_LCL}
    function GetMimeType: string; override;
  {$ENDIF}
    { See ImagingJpegQuality option for details.}
    property Quality: LongInt read FQuality write FQuality;
    { See ImagingJpegProgressive option for details.}
    property Progressive: Boolean read FProgressive write FProgressive;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_PNG}
  { TImagingGraphic descendant for loading/saving PNG images.}
  TImagingPNG = class(TImagingGraphicForSave)
  protected
    FPreFilter: LongInt;
    FCompressLevel: LongInt;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { See ImagingPNGPreFilter option for details.}
    property PreFilter: LongInt read FPreFilter write FPreFilter;
    { See ImagingPNGCompressLevel option for details.}
    property CompressLevel: LongInt read FCompressLevel write FCompressLevel;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_TARGA}
  { TImagingGraphic descendant for loading/saving Targa images.}
  TImagingTarga = class(TImagingGraphicForSave)
  protected
    FUseRLE: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { See ImagingTargaRLE option for details.}
    property UseRLE: Boolean read FUseRLE write FUseRLE;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_DDS}
  { Compresssion type used when saving DDS files by TImagingDds.}
  TDDSCompresion = (dcNone, dcDXT1, dcDXT3, dcDXT5);

  { TImagingGraphic descendant for loading/saving DDS images.}
  TImagingDDS = class(TImagingGraphicForSave)
  protected
    FCompression: TDDSCompresion;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { You can choose compression type used when saving DDS file.
      dcNone means that file will be saved in the current bitmaps pixel format.}
    property Compression: TDDSCompresion read FCompression write FCompression;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_MNG}
  { TImagingGraphic descendant for loading/saving MNG images.}
  TImagingMNG = class(TImagingGraphicForSave)
  protected
    FLossyCompression: Boolean;
    FLossyAlpha: Boolean;
    FPreFilter: LongInt;
    FCompressLevel: LongInt;
    FQuality: LongInt;
    FProgressive: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
  {$IFDEF COMPONENT_SET_LCL}
    function GetMimeType: string; override;
  {$ENDIF}
    { See ImagingMNGLossyCompression option for details.}
    property LossyCompression: Boolean read FLossyCompression write FLossyCompression;
    { See ImagingMNGLossyAlpha option for details.}
    property LossyAlpha: Boolean read FLossyAlpha write FLossyAlpha;
    { See ImagingMNGPreFilter option for details.}
    property PreFilter: LongInt read FPreFilter write FPreFilter;
    { See ImagingMNGCompressLevel option for details.}
    property CompressLevel: LongInt read FCompressLevel write FCompressLevel;
    { See ImagingMNGQuality option for details.}
    property Quality: LongInt read FQuality write FQuality;
    { See ImagingMNGProgressive option for details.}
    property Progressive: Boolean read FProgressive write FProgressive;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_JNG}
  { TImagingGraphic descendant for loading/saving JNG images.}
  TImagingJNG = class(TImagingGraphicForSave)
  protected
    FLossyAlpha: Boolean;
    FAlphaPreFilter: LongInt;
    FAlphaCompressLevel: LongInt;
    FQuality: LongInt;
    FProgressive: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { See ImagingJNGLossyAlpha option for details.}
    property LossyAlpha: Boolean read FLossyAlpha write FLossyAlpha;
    { See ImagingJNGPreFilter option for details.}
    property AlphaPreFilter: LongInt read FAlphaPreFilter write FAlphaPreFilter;
    { See ImagingJNGCompressLevel option for details.}
    property AlphaCompressLevel: LongInt read FAlphaCompressLevel write FAlphaCompressLevel;
    { See ImagingJNGQuality option for details.}
    property Quality: LongInt read FQuality write FQuality;
    { See ImagingJNGProgressive option for details.}
    property Progressive: Boolean read FProgressive write FProgressive;
  end;
{$ENDIF}

{ Returns bitmap pixel format with the closest match with given data format.}
function DataFormatToPixelFormat(Format: TImageFormat): TPixelFormat;
{ Returns data format with closest match with given bitmap pixel format.}
function PixelFormatToDataFormat(Format: TPixelFormat): TImageFormat;

{ Converts TImageData structure to VCL/CLX/LCL bitmap.}
procedure ConvertDataToBitmap(const Data: TImageData; Bitmap: TBitmap);
{ Converts VCL/CLX/LCL bitmap to TImageData structure.}
procedure ConvertBitmapToData(Bitmap: TBitmap; var Data: TImageData);
{ Converts TBaseImage instance to VCL/CLX/LCL bitmap.}
procedure ConvertImageToBitmap(Image: TBaseImage; Bitmap: TBitmap);
{ Converts VCL/CLX/LCL bitmap to TBaseImage. Image must exist before
  procedure is called. It overwrites its current image data.
  When Image is TMultiImage only the current image level is overwritten.}
procedure ConvertBitmapToImage(Bitmap: TBitmap; Image: TBaseImage);

{ Displays image stored in TImageData structure onto TCanvas. This procedure
  draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow). Dest and Src
  rectangles represent coordinates in the form (X1, Y1, X2, Y2).}
procedure DisplayImageData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
{ Displays image onto TCanvas at position [DstX, DstY]. This procedure
  draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow).}
procedure DisplayImage(DstCanvas: TCanvas; DstX, DstY: LongInt; Image: TBaseImage); overload;
{ Displays image onto TCanvas to rectangle DstRect. This procedure
  draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow).}
procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage); overload;
{ Displays part of the image specified by SrcRect onto TCanvas to rectangle DstRect.
  This procedure draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow).}
procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage; const SrcRect: TRect); overload;

{$IFDEF MSWINDOWS}
{ Displays image stored in TImageData structure onto Windows device context.
  Behaviour is the same as of DisplayImageData.}
procedure DisplayImageDataOnDC(DC: HDC; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
{$ENDIF}

implementation

uses
{$IF Defined(LCL)}
  {$IF Defined(LCLGTK2)}
    GLib2, GDK2, GTK2, GTKDef, GTKProc,
  {$ELSEIF Defined(LCLGTK)}
    GDK, GTK, GTKDef, GTKProc,
  {$IFEND}
{$IFEND}
{$IFNDEF DONT_LINK_BITMAP}
  ImagingBitmap,
{$ENDIF}
{$IFNDEF DONT_LINK_JPEG}
  ImagingJpeg,
{$ENDIF}
{$IFNDEF DONT_LINK_TARGA}
  ImagingTarga,
{$ENDIF}
{$IFNDEF DONT_LINK_DDS}
  ImagingDds,
{$ENDIF}
{$IF not Defined(DONT_LINK_PNG) or not Defined(DONT_LINK_MNG) or not Defined(DONT_LINK_JNG)}
  ImagingNetworkGraphics,
{$IFEND}
  ImagingUtility, Contnrs, OverbyteICSMD5, Messages;

var
  HashedPicList: TStringList;

resourcestring
  SBadFormatDataToBitmap = 'Cannot find compatible bitmap format for image %s';
  SBadFormatBitmapToData = 'Cannot find compatible data format for bitmap %p';
  SBadFormatDisplay = 'Unsupported image format passed';
  SUnsupportedLCLWidgetSet = 'This function is not implemented for current LCL widget set';
  SImagingGraphicName = 'Imaging Graphic AllInOne';

const
  WM_SynchronizeMe = WM_USER + 1; //custom message to make thread synchronizing inside DLLs
                                  //WParam is a method pointer, LParam is not used

type
  TDllSynchroClass = class
  private
    FSynchronizeWindow: HWND;
    procedure SynchronizeMe(var Message: TMessage); message WM_SynchronizeMe;
  protected
    procedure WndProc(var Message: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;

    property Handle: HWND read FSynchronizeWindow;
  end;

type
  //internal threaded class that performs self-animation in containers like TImage
  TPainterThread = class(TThread)
  private
    FGraphicList: TObjectList;

    FPause: Boolean;
    FCurIndex: Integer;
    FSyncWnd: TDllSynchroClass;
    procedure DoOnTerminate(Sender: TObject);
  protected
    procedure DoChange;
    procedure Execute; override;
    procedure Synchronize(AMethod: TThreadMethod); reintroduce;
  public
    constructor Create(CreateSuspended: Boolean = False);
    destructor Destroy; override;

    //appends graphic to animation list
    procedure AddNotifyObject(const AObject: TImagingGraphic);
    //removes graphic to animation list
    procedure RemoveNotifyObject(const AObject: TImagingGraphic);
  end;

var
  PainterThread: TPainterThread;
  
{ Registers types to VCL/CLX/LCL.}
procedure RegisterTypes;
var
  I: LongInt;

  procedure RegisterFileFormatAllInOne(Format: TImageFileFormat);
  var
    I: LongInt;
  begin
    for I := 0 to Format.Extensions.Count - 1 do
      TPicture.RegisterFileFormat(Format.Extensions[I], SImagingGraphicName,
        TImagingGraphic);
  end;

  procedure RegisterFileFormat(AClass: TImagingGraphicClass);
  var
    I: LongInt;
  begin
    for I := 0 to AClass.GetFileFormat.Extensions.Count - 1 do
      TPicture.RegisterFileFormat(AClass.GetFileFormat.Extensions[I],
        AClass.GetFileFormat.Name, AClass);
  end;

begin
  for I := Imaging.GetFileFormatCount - 1 downto 0 do
    RegisterFileFormatAllInOne(Imaging.GetFileFormatAtIndex(I));
  Classes.RegisterClass(TImagingGraphic);

{$IFNDEF DONT_LINK_TARGA}
  RegisterFileFormat(TImagingTarga);
  Classes.RegisterClass(TImagingTarga);
{$ENDIF}
{$IFNDEF DONT_LINK_DDS}
  RegisterFileFormat(TImagingDDS);
  Classes.RegisterClass(TImagingDDS);
{$ENDIF}
{$IFNDEF DONT_LINK_JNG}
  RegisterFileFormat(TImagingJNG);
  Classes.RegisterClass(TImagingJNG);
{$ENDIF}
{$IFNDEF DONT_LINK_MNG}
  RegisterFileFormat(TImagingMNG);
  Classes.RegisterClass(TImagingMNG);
{$ENDIF}
{$IFNDEF DONT_LINK_GIF}
  RegisterFileFormat(TImagingGifGraphic);
  Classes.RegisterClass(TImagingGifGraphic);
{$ENDIF}
{$IFNDEF DONT_LINK_PNG}
  {$IFDEF COMPONENT_SET_LCL}
    // Unregister Lazarus´ default PNG loader which crashes on some PNG files
    TPicture.UnregisterGraphicClass(TPortableNetworkGraphic);
  {$ENDIF}
  RegisterFileFormat(TImagingPNG);
  Classes.RegisterClass(TImagingPNG);
{$ENDIF}
{$IFNDEF DONT_LINK_JPEG}
  RegisterFileFormat(TImagingJpeg);
  Classes.RegisterClass(TImagingJpeg);
{$ENDIF}
{$IFNDEF DONT_LINK_BITMAP}
  RegisterFileFormat(TImagingBitmap);
  Classes.RegisterClass(TImagingBitmap);
{$ENDIF}
end;

{ Unregisters types from VCL/LCL.}
procedure UnRegisterTypes;
begin
{$IFNDEF DONT_LINK_BITMAP}
  TPicture.UnregisterGraphicClass(TImagingBitmap);
  Classes.UnRegisterClass(TImagingBitmap);
{$ENDIF}
{$IFNDEF DONT_LINK_JPEG}
  TPicture.UnregisterGraphicClass(TImagingJpeg);
  Classes.UnRegisterClass(TImagingJpeg);
{$ENDIF}
{$IFNDEF DONT_LINK_PNG}
  TPicture.UnregisterGraphicClass(TImagingPNG);
  Classes.UnRegisterClass(TImagingPNG);
{$ENDIF}
{$IFNDEF DONT_LINK_GIF}
  TPicture.UnregisterGraphicClass(TImagingGifGraphic);
  Classes.UnRegisterClass(TImagingGifGraphic);
{$ENDIF}
{$IFNDEF DONT_LINK_TARGA}
  TPicture.UnregisterGraphicClass(TImagingTarga);
  Classes.UnRegisterClass(TImagingTarga);
{$ENDIF}
{$IFNDEF DONT_LINK_DDS}
  TPicture.UnregisterGraphicClass(TImagingDDS);
  Classes.UnRegisterClass(TImagingDDS);
{$ENDIF}
  TPicture.UnregisterGraphicClass(TImagingGraphic);
  Classes.UnRegisterClass(TImagingGraphic);
end;

function DataFormatToPixelFormat(Format: TImageFormat): TPixelFormat;
begin
  case Format of
{$IFDEF COMPONENT_SET_VCL}
    ifIndex8: Result := pf8bit;
    ifR5G6B5: Result := pf16bit;
    ifR8G8B8: Result := pf24bit;
{$ENDIF}
    ifA8R8G8B8,
    ifX8R8G8B8: Result := pf32bit;
  else
    Result := pfCustom;
  end;
end;

function PixelFormatToDataFormat(Format: TPixelFormat): TImageFormat;
begin
  case Format of
    pf8bit: Result := ifIndex8;
    pf15bit: Result := ifA1R5G5B5;
    pf16bit: Result := ifR5G6B5;
    pf24bit: Result := ifR8G8B8;
    pf32bit: Result := ifA8R8G8B8;
  else
    Result := ifUnknown;
  end;
end;

procedure ConvertDataToBitmap(const Data: TImageData; Bitmap: TBitmap);
var
  I, LineBytes: LongInt;
  PF: TPixelFormat;
  Info: TImageFormatInfo;
  WorkData: TImageData;
{$IFDEF COMPONENT_SET_VCL}
  LogPalette: TMaxLogPalette;
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  RawImage: TRawImage;
  ImgHandle, ImgMaskHandle: HBitmap;
{$ENDIF}
begin
  PF := DataFormatToPixelFormat(Data.Format);
  GetImageFormatInfo(Data.Format, Info);
  if PF = pfCustom then
  begin
    // Convert from formats not supported by Graphics unit
    Imaging.InitImage(WorkData);
    Imaging.CloneImage(Data, WorkData);
    if Info.IsFloatingPoint or Info.HasAlphaChannel or Info.IsSpecial then
      Imaging.ConvertImage(WorkData, ifA8R8G8B8)
    else
{$IFDEF COMPONENT_SET_VCL}
      if Info.IsIndexed or Info.HasGrayChannel then
        Imaging.ConvertImage(WorkData, ifIndex8)
      else if Info.UsePixelFormat then
        Imaging.ConvertImage(WorkData, ifR5G6B5)
      else
        Imaging.ConvertImage(WorkData, ifR8G8B8);
{$ELSE}
        Imaging.ConvertImage(WorkData, ifA8R8G8B8);
{$ENDIF}

    PF := DataFormatToPixelFormat(WorkData.Format);
    GetImageFormatInfo(WorkData.Format, Info);
  end
  else
    WorkData := Data;

  if PF = pfCustom then
    RaiseImaging(SBadFormatDataToBitmap, [ImageToStr(WorkData)]);

  LineBytes := WorkData.Width * Info.BytesPerPixel;

{$IFDEF COMPONENT_SET_VCL}
  Bitmap.Width := WorkData.Width;
  Bitmap.Height := WorkData.Height;
  Bitmap.PixelFormat := PF;

  if (PF = pf8bit) and (WorkData.Palette <> nil) then
  begin
    // Copy palette, this must be done before copying bits
    FillChar(LogPalette, SizeOf(LogPalette), 0);
    LogPalette.palVersion := $300;
    LogPalette.palNumEntries := Info.PaletteEntries;
    for I := 0 to Info.PaletteEntries - 1 do
    with LogPalette do
    begin
      palPalEntry[I].peRed := WorkData.Palette[I].R;
      palPalEntry[I].peGreen := WorkData.Palette[I].G;
      palPalEntry[I].peBlue := WorkData.Palette[I].B;
    end;
    Bitmap.Palette := CreatePalette(PLogPalette(@LogPalette)^);
  end;
  // Copy scanlines
  for I := 0 to WorkData.Height - 1 do
    Move(PByteArray(WorkData.Bits)[I * LineBytes], Bitmap.Scanline[I]^, LineBytes);
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  // Create 32bit raw image from image data
  FillChar(RawImage, SizeOf(RawImage), 0);
  with RawImage.Description do
  begin
    Width := WorkData.Width;
    Height := WorkData.Height;
    BitsPerPixel := Info.BytesPerPixel * 8;
    Format := ricfRGBA;
    LineEnd := rileByteBoundary;
    BitOrder := riboBitsInOrder;
    ByteOrder := riboLSBFirst;
    LineOrder := riloTopToBottom;
    AlphaPrec := 8;
    RedPrec := 8;
    GreenPrec := 8;
    BluePrec := 8;
    AlphaShift := 24;
    RedShift := 16;
    GreenShift := 8;
    BlueShift := 0;
    Depth := 24;
  end;
  RawImage.Data := WorkData.Bits;
  RawImage.DataSize := WorkData.Size;

  // Create bitmap from raw image
  { If you get complitation error here upgrade to Lazarus 0.9.24+ }
  if RawImage_CreateBitmaps(RawImage, ImgHandle, ImgMaskHandle, False) then
  begin
    Bitmap.Handle := ImgHandle;
    Bitmap.MaskHandle := ImgMaskHandle;
  end;
{$ENDIF}
  if WorkData.Bits <> Data.Bits then
    Imaging.FreeImage(WorkData);
end;

procedure ConvertBitmapToData(Bitmap: TBitmap; var Data: TImageData);
var
  I, LineBytes: LongInt;
  Format: TImageFormat;
  Info: TImageFormatInfo;
{$IFDEF COMPONENT_SET_VCL}
  Colors: Word;
  LogPalette: TMaxLogPalette;
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  RawImage: TRawImage;
  LineLazBytes: LongInt;
{$ENDIF}
begin
{$IFDEF COMPONENT_SET_LCL}
  // In the current Lazarus 0.9.10 Bitmap.PixelFormat property is useless.
  // We cannot change bitmap's format by changing it (it will just release
  // old image but not convert it to new format) nor we can determine bitmaps's
  // current format (it is usually set to pfDevice). So bitmap's format is obtained
  // trough RawImage api and cannot be changed to mirror some Imaging format
  // (so formats with no coresponding Imaging format cannot be saved now).

  if RawImage_DescriptionFromBitmap(Bitmap.Handle, RawImage.Description) then
    case RawImage.Description.BitsPerPixel of
      8: Format := ifIndex8;
      16:
        if RawImage.Description.Depth = 15 then
          Format := ifA1R5G5B5
        else
          Format := ifR5G6B5;
      24: Format := ifR8G8B8;
      32: Format := ifA8R8G8B8;
      48: Format := ifR16G16B16;
      64: Format := ifA16R16G16B16;
    else
      Format := ifUnknown;
    end;
{$ELSE}
  Format := PixelFormatToDataFormat(Bitmap.PixelFormat);
  if Format = ifUnknown then
  begin
    // Convert from formats not supported by Imaging (1/4 bit)
    if Bitmap.PixelFormat < pf8bit then
       Bitmap.PixelFormat := pf8bit
    else
      Bitmap.PixelFormat := pf32bit;
    Format := PixelFormatToDataFormat(Bitmap.PixelFormat);
  end;
{$ENDIF}

  if Format = ifUnknown then
    RaiseImaging(SBadFormatBitmapToData, []);

  Imaging.NewImage(Bitmap.Width, Bitmap.Height, Format, Data);
  GetImageFormatInfo(Data.Format, Info);
  LineBytes := Data.Width * Info.BytesPerPixel;

{$IFDEF COMPONENT_SET_VCL}
  if (Format = ifIndex8) and (GetObject(Bitmap.Palette, SizeOf(Colors),
    @Colors) <> 0) then
  begin
    // Copy palette
    GetPaletteEntries(Bitmap.Palette, 0, Colors, LogPalette.palPalEntry);
    if Colors > Info.PaletteEntries  then
      Colors := Info.PaletteEntries;
    for I := 0 to Colors - 1 do
    with LogPalette do
    begin
      Data.Palette[I].A := $FF;
      Data.Palette[I].R := palPalEntry[I].peRed;
      Data.Palette[I].G := palPalEntry[I].peGreen;
      Data.Palette[I].B := palPalEntry[I].peBlue;
    end;
  end;
  // Copy scanlines
  for I := 0 to Data.Height - 1 do
    Move(Bitmap.ScanLine[I]^, PByteArray(Data.Bits)[I * LineBytes], LineBytes);
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  // Get raw image from bitmap (mask handle must be 0 or expect violations)
  if RawImage_FromBitmap(RawImage, Bitmap.Handle, 0, nil) then
  begin
    LineLazBytes := GetBytesPerLine(Data.Width, RawImage.Description.BitsPerPixel,
      RawImage.Description.LineEnd);
    // Copy scanlines
    for I := 0 to Data.Height - 1 do
      Move(PByteArray(RawImage.Data)[I * LineLazBytes],
        PByteArray(Data.Bits)[I * LineBytes], LineBytes);
    { If you get complitation error here upgrade to Lazarus 0.9.24+ }
    RawImage.FreeData;
  end;
{$ENDIF}
end;

procedure ConvertImageToBitmap(Image: TBaseImage; Bitmap: TBitmap);
begin
  ConvertDataToBitmap(Image.ImageDataPointer^, Bitmap);
end;

procedure ConvertBitmapToImage(Bitmap: TBitmap; Image: TBaseImage);
begin
  ConvertBitmapToData(Bitmap, Image.ImageDataPointer^);
end;

{$IFDEF MSWINDOWS}
procedure DisplayImageDataOnDC(DC: HDC; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
var
  OldMode: Integer;
  BitmapInfo: Windows.TBitmapInfo;
  Bmp: TBitmap;
begin
  if TestImage(ImageData) then
  begin
    Assert(ImageData.Format in [ifA8R8G8B8, ifX8R8G8B8], SBadFormatDisplay);
    OldMode := Windows.SetStretchBltMode(DC, COLORONCOLOR);

    FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
    with BitmapInfo.bmiHeader do
    begin
      biSize := SizeOf(TBitmapInfoHeader);
      biPlanes := 1;
      biBitCount := 32;
      biCompression := BI_RGB;
      biWidth := ImageData.Width;
      biHeight := -ImageData.Height;
      biSizeImage := ImageData.Size;
      biXPelsPerMeter := 0;
      biYPelsPerMeter := 0;
      biClrUsed := 0;
      biClrImportant := 0;
    end;

    try
       with SrcRect, ImageData do
        if Windows.StretchDIBits(DC, DstRect.Left, DstRect.Top,
          DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top, Left,
          Top, Right - Left, Bottom - Top, Bits, BitmapInfo, DIB_RGB_COLORS, SRCCOPY) <> Height then
        begin
          // StretchDIBits may fail on some ocassions (error 487, http://support.microsoft.com/kb/269585).
          // This fallback is slow but works every time. Thanks to Sergey Galezdinov for the fix.
          Bmp := TBitmap.Create;
          try
            ConvertDataToBitmap(ImageData, Bmp);
            StretchBlt(DC, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
              Bmp.Canvas.Handle, 0, 0, Width, Height, SRCCOPY);
          finally
            Bmp.Free;
          end;
        end;
    finally
      Windows.SetStretchBltMode(DC, OldMode);
    end;
  end;
end;
{$ENDIF}

procedure DisplayImageData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
{$IF Defined(DCC) or Defined(LCLWIN32)} // Delphi or LCL Win32
begin
  DisplayImageDataOnDC(DstCanvas.Handle, DstRect, ImageData, SrcRect);
end;
{$ELSEIF Defined(LCLGTK) or Defined(LCLGTK2)}

  procedure GDKDrawBitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY,
    SrcWidth, SrcHeight: Integer; ImageData: TImageData);
  var
    P: TPoint;
  begin
    P := TGtkDeviceContext(Dest).Offset;
    Inc(DstX, P.X);
    Inc(DstY, P.Y);
    gdk_draw_rgb_32_image(TGtkDeviceContext(Dest).Drawable, TGtkDeviceContext(Dest).GC,
      DstX, DstY, SrcWidth, SrcHeight, GDK_RGB_DITHER_NONE,
      @PLongWordArray(ImageData.Bits)[SrcY * ImageData.Width + SrcX], ImageData.Width * 4);
  end;

var
  DisplayImage: TImageData;
  NewWidth, NewHeight: Integer;
  SrcBounds, DstBounds, DstClip: TRect;
begin
  if TestImage(ImageData) then
  begin
    Assert(ImageData.Format in [ifA8R8G8B8, ifX8R8G8B8], SBadFormatDisplay);
    InitImage(DisplayImage);

    SrcBounds := RectToBounds(SrcRect);
    DstBounds := RectToBounds(DstRect);
    WidgetSet.GetClipBox(DstCanvas.Handle, @DstClip);

    ClipStretchBounds(SrcBounds.Left, SrcBounds.Top, SrcBounds.Right, SrcBounds.Bottom,
      DstBounds.Left, DstBounds.Top, DstBounds.Right, DstBounds.Bottom, ImageData.Width,
      ImageData.Height, DstClip);

    NewWidth := DstBounds.Right;
    NewHeight := DstBounds.Bottom;

    if (NewWidth > 0) and (NewHeight > 0) then
    begin
      if (SrcBounds.Right = NewWidth) and (SrcBounds.Bottom = NewHeight) then
      try
        CloneImage(ImageData, DisplayImage);
        // Swap R-B channels for GTK display compatability!
        SwapChannels(DisplayImage, ChannelRed, ChannelBlue);
        GDKDrawBitmap(DstCanvas.Handle, DstBounds.Left, DstBounds.Top,
          SrcBounds.Left, SrcBounds.Top, NewWidth, NewHeight, DisplayImage);
      finally
        FreeImage(DisplayImage);
      end
      else
      try
        // Create new image with desired dimensions
        NewImage(NewWidth, NewHeight, ImageData.Format, DisplayImage);
        // Stretch pixels from old image to new one  TResizeFilter = (rfNearest, rfBilinear, rfBicubic);
        StretchRect(ImageData, SrcBounds.Left, SrcBounds.Top, SrcBounds.Right,
          SrcBounds.Bottom, DisplayImage, 0, 0, NewWidth, NewHeight, rfNearest);
        // Swap R-B channels for GTK display compatability!
        SwapChannels(DisplayImage, ChannelRed, ChannelBlue);
        GDKDrawBitmap(DstCanvas.Handle, DstBounds.Left, DstBounds.Top, 0, 0,
          NewWidth, NewHeight, DisplayImage);
       finally
        FreeImage(DisplayImage);
      end
    end;
  end;
end;
{$ELSE}
begin
  raise Exception.Create(SUnsupportedLCLWidgetSet);
end;
{$IFEND}

procedure DisplayImage(DstCanvas: TCanvas; DstX, DstY: LongInt; Image: TBaseImage);
begin
  DisplayImageData(DstCanvas, BoundsToRect(DstX, DstY, Image.Width, Image.Height),
    Image.ImageDataPointer^, Image.BoundsRect);
end;

procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage);
begin
  DisplayImageData(DstCanvas, DstRect, Image.ImageDataPointer^, Image.BoundsRect);
end;

procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage; const SrcRect: TRect);
begin
  DisplayImageData(DstCanvas, DstRect, Image.ImageDataPointer^, SrcRect);
end;


{ TImagingGraphic class implementation }

constructor TImagingGraphic.Create;
begin
  inherited Create;
  FBgColor := clNone;
  FSelfAnimated := True;
  FReAnimate    := True;
  FAddedToList := False;
  FLoadState := lsEmpty;
  FMultImage := nil;
  FLastCanvas := nil;
  FBGShare := False;
  FActiveImage := 0;
  FIsReferenced := True;
end;

{$WARNINGS OFF}
constructor TImagingGraphic.Create(const Referenced: Boolean);
begin
  Create;
  FIsReferenced := Referenced;
end;
{$WARNINGS ON}

destructor TImagingGraphic.Destroy;
begin
  FLastCanvas := nil;
  SelfAnimated := False;
  if FMultImage <> nil then
    FMultImage.ReferredFree(FIsReferenced);

  FMultImage := nil;
  if not FBGShare then
    FreeAndNil(FBackGroundBitmap);
  inherited;
end;

procedure TImagingGraphic.DoPaintTriggered;
begin
  if (Self = nil) or Empty or (FMultImage = nil) then Exit;

  if FActiveImage < FMultImage.ImageCount - 1 then
    FActiveImage := FActiveImage + 1
  else
    FActiveImage := 0;

  try
    if GetCurrentObject(FLastCanvas.Handle, OBJ_BITMAP) <> 0 then
      Draw(FLastCanvas, FLastRect)
    else
    begin
      FLastCanvas := nil;
      FreeAndNil(FBackGroundBitmap);
      Changed(Self);
    end;
  except
    //if AV happens while accessing FLastCanvas
    FLastCanvas := nil;
    Changed(Self);
  end;
end;

procedure TImagingGraphic.Draw(ACanvas: TCanvas; const ARect: TRect);
var
  b: TBitmap;
  TransCol: TColor32Rec;
  BackData, StretchBackData, Data: TImageData;
  sRect: TRect;
  ShouldAdd: Boolean;

  function RectToSize(const szRect: TRect): TSize;
  begin
    Result.cx := Abs(szRect.Right - szRect.Left);
    Result.cy := Abs(szRect.Bottom - szRect.Top);
  end;

begin
  if (FMultImage = nil) or not (lsLoaded = FLoadState) then Exit;

  //need to add to animation thread only if animation contains more then 1 frame, should be animated/re-animated and HDC's window is not 0
  ShouldAdd := (FMultImage.ImageCount > 1) and (FSelfAnimated or (not FAddedToList and FReAnimate)) and
               (WindowFromDC(ACanvas.Handle) <> 0);
  try
  { TODO -oSega-Zero : do correct drawing when something has overlapped the part of canvas }
   
  {  if FSelfAnimated and FAddedToList and (FLastCanvas <> nil) then
    with ACanvas.ClipRect do
    if not EqualRect(ACanvas.ClipRect, ARect) then
    try
      FLastCanvas := nil;
      if not BackgroundSharing then
        FreeAndNil(FBackGroundBitmap);
      Exit;
    finally
      Changed(Self);
    end;      // }

    InitImage(BackData);
    NewImage(Width, Height, ifA8R8G8B8, BackData);

    with RectToSize(ARect) do
    if (cx <> Width) or (cy <> Height) then
    begin
      SetAlpha(BackData, 0);
      InitImage(StretchBackData);
      NewImage(cx, cy, ifA8R8G8B8, StretchBackData);
    end;

    FMultImage.ActiveImage := FActiveImage;

    with RectToSize(ARect) do
    if BackGroundBitmap <> nil then
    begin
      if (cx <> Width) or (cy <> Height) then
      begin
        ConvertBitmapToData(BackGroundBitmap, StretchBackData);
        //Set alpha channel to 255 - we don't need any transparency here
        SetAlpha(StretchBackData, 255);
      end
      else
        begin
          ConvertBitmapToData(BackGroundBitmap, BackData);
          //Set alpha channel to 255 - we don't need any transparency here
          SetAlpha(BackData, 255);
        end;
    end
    else
      if FBgColor <> clNone then
      begin
        if (cx <> Width) or (cy <> Height) then
          ConvertImage(StretchBackData, ifA8R8G8B8)
        else
          ConvertImage(BackData, ifA8R8G8B8);
        with TransCol do
        begin
          A := 255;
          R := GetRValue(ColorToRGB(FBgColor));
          G := GetGValue(ColorToRGB(FBgColor));
          B := GetBValue(ColorToRGB(FBgColor));
        end;
        if (cx <> Width) or (cy <> Height) then
          FillRect(StretchBackData, 0, 0, cx, cy, @TransCol)
        else
          FillRect(BackData, 0, 0, Width, Height, @TransCol);
      end
      else
        begin
          b := TBitmap.Create;
          b.Width := cx;
          b.Height := cy;
          b.PixelFormat := pf32bit;
          b.Canvas.CopyRect(Rect(0, 0, b.Width, b.Height), ACanvas, ARect);
          if (cx <> Width) or (cy <> Height) then
          begin
            ConvertBitmapToData(b, StretchBackData);
            SetAlpha(StretchBackData, 255);
          end
          else
            begin
              ConvertBitmapToData(b, BackData);
              SetAlpha(BackData, 255);
            end;
            
          with RectToSize(ARect) do
          if not EqualRect(ACanvas.ClipRect, ARect) and ((cx <> Width) and (cy <> Height)) then
            b.Free
          else
            begin
              FBackGroundBitmap := b;
              BackgroundSharing := False;
            end;
        end;

    PrepareFrame(FMultImage.ActiveImage, Data, @BackData);
    with RectToSize(ARect) do
    if (cx <> Width) or (cy <> Height) then
      DoPrepareFrame(FMultImage.ActiveImage, Data, @StretchBackData);

    if Data.Format <> ifA8R8G8B8 then
      ConvertImage(Data, ifA8R8G8B8);

    sRect := Rect(0, 0, Data.Width, Data.Height);
    DisplayImageData(ACanvas, ARect, Data, sRect);

    if TestImage(StretchBackData) then
      FreeImage(StretchBackData);
    FreeImage(BackData);

    FreeImage(Data);

    if ShouldAdd and (FLastCanvas <> ACanvas) then
      FLastCanvas := ACanvas;

    FLastRect := ARect;
  except
    on E: Exception do
    begin
      //SendDebug('error painting info: ' + E.Message);
      raise;
    end;
  end;

  if ShouldAdd then SelfAnimated := True;
end;

function TImagingGraphic.GetCurrentDelay: LongInt;
begin
  Result := -1;

  if (Self <> nil) and not Empty then
  with FMultImage do
  begin
    if not Valid then Exit;

    Result := 10;
    if Images[ActiveImage].Extra <> nil then
    if Images[ActiveImage].Extra.Delay > 0 then
    begin
      Result := Images[ActiveImage].Extra.Delay;
      if (Result < 3) then
        Result := 3;
      if (Result > 1000) then
        Result := 1000;
    end;
    Result := Result * 10;
  end;
end;

function TImagingGraphic.GetEmpty: Boolean;
begin
  Result := (FMultImage = nil) or ((FMultImage.ImageCount = 0) or not FMultImage.AllImagesValid);
end;

class function TImagingGraphic.GetFileFormat: TImageFileFormat;
begin
  Result := nil;
end;

function TImagingGraphic.GetHeight: Integer;
begin
  if not Empty then
    Result := FMultImage.Height
  else
    Result := 0;
end;

function TImagingGraphic.GetWidth: Integer;
begin
  if not Empty then
    Result := FMultImage.Width
  else
    Result := 0;
end;

procedure TImagingGraphic.SaveToStream(Stream: TStream);
begin
  if (FMultImage <> nil) and (GetFileFormat <> nil) then
    GetFileFormat.SaveToStream(Stream, FMultImage.DataArray);
end;

procedure TImagingGraphic.SetActiveImgIndex(const Value: Integer);
begin
  FActiveImage := Value;
//  if FMultImage.ActiveImage <> Value then
//    FMultImage.ActiveImage := Value;
end;

procedure TImagingGraphic.SetBackGroundBitmap(const Value: TBitmap);
begin
  if (BackGroundBitmap <> nil) and not FBGShare then
    FreeAndNil(FBackGroundBitmap);
  FBackGroundBitmap := Value;
end;

procedure TImagingGraphic.SetHeight(Value: Integer);
begin
  if (Value > 0) and (Width > 0) then
  begin
    UnRefImage;
    FMultImage.ResizeImages(Width, Value, rfBicubic);
    Changed(Self);
  end;
end;

procedure TImagingGraphic.SetSelfAnimated(const Value: Boolean);
begin
  FSelfAnimated := Value;
  if FSelfAnimated then
  begin
    if not FAddedToList then
      PainterThread.AddNotifyObject(Self);
    FAddedToList := True;
  end
  else
    if FAddedToList then
    begin
      PainterThread.RemoveNotifyObject(Self);
      FAddedToList := False;
    end;
end;

procedure TImagingGraphic.SetSize(AWidth, AHeight: Integer);
begin
  if (AWidth > 0) and (AHeight > 0) then
  begin
    UnRefImage;
    FMultImage.ResizeImages(AWidth, AHeight, rfBicubic);
    Changed(Self);
  end;
end;

procedure TImagingGraphic.SetWidth(Value: Integer);
begin
  if (Height > 0) and (Value > 0) then
  begin
    UnRefImage;
    FMultImage.ResizeImages(Value, Height, rfBicubic);
    Changed(Self);
  end;
end;

procedure TImagingGraphic.UnRefImage;
var
  TempStream: TMemoryStream;
begin
  if (FMultImage = nil) or not FIsReferenced then Exit;

  TempStream := TMemoryStream.Create;
  try
    SaveToStream(TempStream);
    TempStream.Position := 0; 
    FMultImage.ReferredFree;
    FMultImage := nil;
    FIsReferenced := False;
    LoadFromStream(TempStream);
  finally
    TempStream.Free;
  end;
end;

procedure TImagingGraphic.LoadFromStream(Stream: TStream);
begin
  if Stream <> nil then
  begin
    ReadDataFromStream(Stream);
    Changed(Self);
  end;
end;

procedure TImagingGraphic.PaintTriggered;
begin
  if not ParentControlVisible then
  begin
    SelfAnimated := False;
    if not FBGShare then
      FreeAndNil(FBackGroundBitmap);
    FLastRect := Rect(0, 0, 0, 0);
  end
  else
  begin
    DoPaintTriggered;
    FWaitCounter := GetCurrentDelay;
  end;
end;

procedure TImagingGraphic.PrepareFrame(const FrameIndex: Integer; var Frame: TImageData;
  Back: PImageData);
begin
  CloneImage(FMultImage.DataArray[FrameIndex], Frame);
  DoPrepareFrame(FrameIndex, Frame, Back);
end;

procedure TImagingGraphic.DoPrepareFrame(const FrameIndex: Integer; var Frame: TImageData; Back: PImageData);
var
  BackCanvas: TImagingCanvas;
  FImageCanvas: TImagingCanvas;
  sRect, ARect: TRect;
begin
  if Frame.Format <> ifA8R8G8B8 then
    ConvertImage(Frame, ifA8R8G8B8);

  if Back <> nil then
  begin
    BackCanvas := TImagingCanvas.CreateForData(Back);
    FImageCanvas:= TImagingCanvas.CreateForData(@Frame);
    sRect := Rect(0, 0, Back^.Width, Back^.Height);
    ARect := Rect(0, 0, Width, Height);
    FImageCanvas.StretchDrawAlpha(ARect, BackCanvas, sRect, rfBicubic);
    CloneImage(Back^, Frame);
    FreeAndNil(FImageCanvas);
    FreeAndNil(BackCanvas);
  end;
end;

procedure TImagingGraphic.ReadDataFromStream(Stream: TStream);
var
  StrHash: string;
  HashPos: Integer;
  buf: array of Byte;
  BackupMulti: TRefMultiImage;
begin
  //in case of data already loaded before but not freed
  BackupMulti := FMultImage;
  try
    BeforeLoad(Stream);
    if not FIsReferenced then
    begin
      FMultImage := TRefMultiImage.Create;
      if not FMultImage.LoadMultiFromStream(Stream) then
      begin
        //couldnt load picture
        FreeAndNil(FMultImage);
        Exit;
      end;
    end
    else
    begin
      StrHash := '';
      Stream.Position := 0;
      SetLength(buf, Stream.Size);
      Stream.Read(buf[0], Stream.Size);
      StrHash := Getmd5(Pointer(buf), Stream.Size);
      SetLength(buf, 0);

      //find hash in Internal RefCount hashed List
      HashPos := HashedPicList.IndexOf(StrHash);
      if HashPos < 0 then
      begin
        FMultImage := TRefMultiImage.Create;

        Stream.Position := 0;
        FLoadState := lsLoading;
        if not FMultImage.LoadMultiFromStream(Stream) then
        begin
          //couldnt load picture
          FMultImage.ReferredFree(False);
          FMultImage := BackupMulti;
          Exit;
        end;
        HashedPicList.AddObject(StrHash, FMultImage.GetCopy);
      end
      else
      begin
        //inc RefCount
        if HashedPicList.Objects[HashPos] <> nil then
          FMultImage := TRefMultiImage(HashedPicList.Objects[HashPos]).GetCopy;//}
      end;
    end;

    if BackupMulti <> nil then
    begin
      BackupMulti.ReferredFree(FIsReferenced);
      BackupMulti := nil;
    end;

    AfterLoad(Stream);
    FLoadState := lsLoaded;
    Transparent := True;
  except
    if FMultImage <> nil then
    begin
      FMultImage.ReferredFree(FIsReferenced);
      FMultImage := nil;
    end;
    FMultImage := BackupMulti;
  end;
end;

procedure TImagingGraphic.AssignTo(Dest: TPersistent);
var
  FramesToAssign: TDynImageDataArray;
  I: Integer;
begin
  if FMultImage = nil then Exit;
  begin
    SetLength(FramesToAssign , FMultImage.ImageCount);
    for I := 0 to FMultImage.ImageCount - 1 do
    begin
      InitImage(FramesToAssign[I]);
      PrepareFrame(I, FramesToAssign[I], nil);
    end;

    if Dest is TSingleImage then
      TSingleImage(Dest).CreateFromData(FramesToAssign[0])
    else
    if Dest is TMultiImage then
      TMultiImage(Dest).CreateFromArray(FramesToAssign)
    else
    if Dest is TBitmap then
      ConvertDataToBitmap(FramesToAssign[0], TBitmap(Dest))
    else
      inherited AssignTo(Dest);
    FreeImagesInArray(FramesToAssign);
    SetLength(FramesToAssign, 0);
  end;
end;

procedure TImagingGraphic.AfterLoad(Stream: TStream);
begin
//nothing here
end;

procedure TImagingGraphic.Assign(Source: TPersistent);
var
  ID: TImageData;
begin
  if Source is TImagingGraphic then
  begin
    if (Source <> nil) and (TImagingGraphic(Source).FMultImage <> nil) then
    begin
      FMultImage := TImagingGraphic(Source).Frames.GetCopy;
      FLoadState := lsLoaded;
    end
    else
      begin
        if FMultImage <> nil then FMultImage.ReferredFree(FIsReferenced);
        FMultImage := TRefMultiImage.Create;
      end;
  end
  else
  if Source is TBaseImage then
    AssignFromImage(TBaseImage(Source))
  else
    if Source is TBitmap then
    begin
      ID := FMultImage[0];
      ConvertBitmapToData(TBitmap(Source), ID);
    end
    else
      inherited Assign(Source);
end;

procedure TImagingGraphic.AssignFromImage(Image: TBaseImage);
begin
  if (Image <> nil) and Image.Valid then
    AssignFromImageData(Image.ImageDataPointer^);
end;

procedure TImagingGraphic.AssignToImage(Image: TBaseImage);
begin
  if (Image <> nil) then
  begin
    if (Image is TSingleImage) and (Image.ImageDataPointer <> nil) then
      AssignToImageData(Image.ImageDataPointer^);
  end;
end;

procedure TImagingGraphic.AssignFromImageData(const ImageData: TImageData);
begin
  if FMultImage = nil then
    FMultImage := TRefMultiImage.Create
  else
    begin
      UnRefImage;
      FMultImage.SetImageCount(0);
    end;

  FMultImage.AddImage(ImageData);
end;

procedure TImagingGraphic.AssignToImageData(var ImageData: TImageData);
begin
  if TestImage(ImageData) then
    FreeImage(ImageData);
  if FMultImage <> nil then
    PrepareFrame(0, ImageData, nil);
end;

procedure TImagingGraphic.BeforeLoad(Stream: TStream);
begin
//nothing here
end;

{ TImagingGraphicForSave class implementation }

constructor TImagingGraphicForSave.Create;
begin
  inherited Create;
  FDefaultFileExt := GetFileFormat.Extensions[0];
  FSavingFormat := ifUnknown;
  GetFileFormat.CheckOptionsValidity;
end;

procedure TImagingGraphicForSave.WriteDataToStream(Stream: TStream);
var
  Image: TSingleImage;
begin
  if FDefaultFileExt <> '' then
  begin
    Image := TSingleImage.Create;
    try
      Image.Assign(Self);
      if FSavingFormat <> ifUnknown then
        Image.Format := FSavingFormat;
      Image.SaveToStream(FDefaultFileExt, Stream);
    finally
      Image.Free;
    end;
  end;
end;

procedure TImagingGraphicForSave.SaveToStream(Stream: TStream);
begin
  WriteDataToStream(Stream);
end;

{$IFDEF COMPONENT_SET_LCL}
class function TImagingGraphicForSave.GetFileExtensions: string;
begin
  Result := StringReplace(GetFileFormat.Extensions.CommaText, ',', ';', [rfReplaceAll]);
end;

function TImagingGraphicForSave.GetMimeType: string;
begin
  Result := 'image/' + FDefaultFileExt;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_BITMAP}

{ TImagingBitmap class implementation }

constructor TImagingBitmap.Create;
begin
  inherited Create;
  FUseRLE := (GetFileFormat as TBitmapFileFormat).UseRLE;
end;

class function TImagingBitmap.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TBitmapFileFormat);
end;

procedure TImagingBitmap.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingBitmapRLE, Ord(FUseRLE));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_JPEG}

{ TImagingJpeg class implementation }

constructor TImagingJpeg.Create;
begin
  inherited Create;
  FQuality := (GetFileFormat as TJpegFileFormat).Quality;
  FProgressive := (GetFileFormat as TJpegFileFormat).Progressive;
end;

class function TImagingJpeg.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TJpegFileFormat);
end;

{$IFDEF COMPONENT_SET_LCL}
function TImagingJpeg.GetMimeType: string;
begin
  Result := 'image/jpeg';
end;
{$ENDIF}

procedure TImagingJpeg.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingJpegQuality, FQuality);
  Imaging.SetOption(ImagingJpegProgressive, Ord(FProgressive));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;

{$ENDIF}

{$IFNDEF DONT_LINK_PNG}

{ TImagingPNG class implementation }

constructor TImagingPNG.Create;
begin
  inherited Create;
  FPreFilter := (GetFileFormat as TPNGFileFormat).PreFilter;
  FCompressLevel := (GetFileFormat as TPNGFileFormat).CompressLevel;
end;

class function TImagingPNG.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TPNGFileFormat);
end;

procedure TImagingPNG.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingPNGPreFilter, FPreFilter);
  Imaging.SetOption(ImagingPNGCompressLevel, FCompressLevel);
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_TARGA}

{ TImagingTarga class implementation }

constructor TImagingTarga.Create;
begin
  inherited Create;
  FUseRLE := (GetFileFormat as TTargaFileFormat).UseRLE;
end;

class function TImagingTarga.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TTargaFileFormat);
end;

procedure TImagingTarga.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingTargaRLE, Ord(FUseRLE));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_DDS}

{ TImagingDDS class implementation }

constructor TImagingDDS.Create;
begin
  inherited Create;
  FCompression := dcNone;
end;

class function TImagingDDS.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TDDSFileFormat);
end;

procedure TImagingDDS.SaveToStream(Stream: TStream);
begin
  case FCompression of
    dcNone: FSavingFormat := ifUnknown;
    dcDXT1: FSavingFormat := ifDXT1;
    dcDXT3: FSavingFormat := ifDXT3;
    dcDXT5: FSavingFormat := ifDXT5;
  end;
  Imaging.PushOptions;
  Imaging.SetOption(ImagingDDSSaveCubeMap, Ord(False));
  Imaging.SetOption(ImagingDDSSaveVolume, Ord(False));
  Imaging.SetOption(ImagingDDSSaveMipMapCount, 1);
  Imaging.SetOption(ImagingDDSSaveDepth, 1);
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_MNG}

{ TImagingMNG class implementation }

constructor TImagingMNG.Create;
begin
  inherited Create;
  FLossyCompression := (GetFileFormat as TMNGFileFormat).LossyCompression;
  FLossyAlpha := (GetFileFormat as TMNGFileFormat).LossyAlpha;
  FPreFilter := (GetFileFormat as TMNGFileFormat).PreFilter;
  FCompressLevel := (GetFileFormat as TMNGFileFormat).CompressLevel;
  FQuality := (GetFileFormat as TMNGFileFormat).Quality;
  FProgressive := (GetFileFormat as TMNGFileFormat).Progressive;
end;

class function TImagingMNG.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TMNGFileFormat);
end;

{$IFDEF COMPONENT_SET_LCL}
function TImagingMNG.GetMimeType: string;
begin
  Result := 'video/mng';
end;
{$ENDIF}

procedure TImagingMNG.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingMNGLossyCompression, Ord(FLossyCompression));
  Imaging.SetOption(ImagingMNGLossyAlpha, Ord(FLossyAlpha));
  Imaging.SetOption(ImagingMNGPreFilter, FPreFilter);
  Imaging.SetOption(ImagingMNGCompressLevel, FCompressLevel);
  Imaging.SetOption(ImagingMNGQuality, FQuality);
  Imaging.SetOption(ImagingMNGProgressive, Ord(FProgressive));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_JNG}

{ TImagingJNG class implementation }

constructor TImagingJNG.Create;
begin
  inherited Create;
  FLossyAlpha := (GetFileFormat as TJNGFileFormat).LossyAlpha;
  FAlphaPreFilter := (GetFileFormat as TJNGFileFormat).PreFilter;
  FAlphaCompressLevel := (GetFileFormat as TJNGFileFormat).CompressLevel;
  FQuality := (GetFileFormat as TJNGFileFormat).Quality;
  FProgressive := (GetFileFormat as TJNGFileFormat).Progressive;
end;

class function TImagingJNG.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TJNGFileFormat);
end;

procedure TImagingJNG.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingJNGLossyALpha, Ord(FLossyAlpha));
  Imaging.SetOption(ImagingJNGAlphaPreFilter, FAlphaPreFilter);
  Imaging.SetOption(ImagingJNGAlphaCompressLevel, FAlphaCompressLevel);
  Imaging.SetOption(ImagingJNGQuality, FQuality);
  Imaging.SetOption(ImagingJNGProgressive, Ord(FProgressive));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

procedure TImagingGraphic.LoadFromClipboardFormat(AFormat: Word;
  AData: Cardinal; APalette: HPALETTE);
begin
//not implemented yet
end;

procedure TImagingGraphic.SaveToClipboardFormat(var AFormat: Word;
  var AData: Cardinal; var APalette: HPALETTE);
begin
//not implemented yet
end;

function TImagingGraphic.ParentControlVisible: Boolean;
begin
  Result := FLastCanvas <> nil;
  if Result then
  try
    FLastCanvas.Refresh;
    Result := (FLastCanvas.Handle <> 0) and IsWindowVisible(WindowFromDC(FLastCanvas.Handle));
  except
    FLastCanvas := nil;
    FLastRect := Rect(0, 0, 0, 0);
    Result := False;
  end;
end;

{ TPainterThread }

var
  AEvent: THandle;

procedure TPainterThread.AddNotifyObject(const AObject: TImagingGraphic);
begin
  if FGraphicList.IndexOf(AObject) < 0 then
  try
    if not AObject.SelfAnimated then Exit;

    //if this is the first object in list then start animaition cycle
    if FGraphicList.Count = 0 then Resume;

    FPause := True;

    //if animation cycle waiting for signal then send the signal manually
    if AEvent <> 0 then
      SetEvent(AEvent);

    Suspend;
    FGraphicList.Add(AObject);
  finally
    FPause := False;
    Resume;
  end;
end;

constructor TPainterThread.Create(CreateSuspended: Boolean);
begin
  FGraphicList := TObjectList.Create(False);

  FPause := False;

  inherited Create(CreateSuspended);
  OnTerminate := Self.DoOnTerminate;
  if HInstance <> MainInstance then
    FSyncWnd := TDllSynchroClass.Create;
end;

destructor TPainterThread.Destroy;
begin
  //terminate the animation cycle
  FPause := True;

  //if animation cycle waiting for signal then send the signal manually
  if AEvent <> 0 then
    SetEvent(AEvent);

  if FGraphicList.Count > 0 then
    FGraphicList.Clear;

  FGraphicList.Free;
  if HInstance <> MainInstance then
    FSyncWnd.Free;
  inherited;
end;

procedure TPainterThread.DoChange;
begin
  //animated drawing should be done in the main thread
  if (FGraphicList.Count > 0) and not FPause then
  try
    TImagingGraphic(FGraphicList[FCurIndex]).PaintTriggered;
  except

  end;
end;

procedure TPainterThread.Execute;
var
  I, MinInterval: Integer;
begin
  while not Terminated do
  if not FPause and (FGraphicList.Count > 0) then
  try
    I := 0;
    //calc the minimum interval for waiting
    MinInterval := TImagingGraphic(FGraphicList[0]).FWaitCounter;
    while I < FGraphicList.Count do
    begin
      if (TImagingGraphic(FGraphicList[I]).FWaitCounter > 0) and
         (TImagingGraphic(FGraphicList[I]).FWaitCounter < MinInterval) then
        MinInterval := TImagingGraphic(FGraphicList[I]).FWaitCounter;
      Inc(I);
    end;

    //create the signal object
    if (FGraphicList.Count > 0) and not FPause then
      AEvent := CreateEvent(nil, True, False, '');

    //wait for signal exactly MinInterval. That trick allow us not to use any kind of timer
    if (FGraphicList.Count > 0) and not FPause then
      WaitForSingleObject(AEvent, MinInterval);

    if AEvent <> 0 then
      CloseHandle(AEvent);
    AEvent := 0;

    if Terminated or FPause then Continue;

    //decrement internal wait counters
    I := 0;
    while I < FGraphicList.Count do
    begin
      if (TImagingGraphic(FGraphicList[I]).FWaitCounter > 0) then
        Dec(TImagingGraphic(FGraphicList[I]).FWaitCounter, MinInterval);
      Inc(I);
    end;

    //if there are items with WiatCounter = 0 then do the animation paint
    I := 0;
    while not Suspended and not FPause and (I < FGraphicList.Count) do
    begin
      if TImagingGraphic(FGraphicList[I]).FWaitCounter < 0 then
      begin
        Inc(I);
        Continue;
      end;

      if (FGraphicList.Count > 0) and (TImagingGraphic(FGraphicList[I]).FWaitCounter = 0) and
         (I < FGraphicList.Count) then
      try
        FCurIndex := I;
        Synchronize(DoChange);
      except
      end;
      Inc(I);
    end;

  except
  end;
end;

procedure TPainterThread.DoOnTerminate(Sender: TObject);
begin
  FPause := True;
end;

procedure TPainterThread.RemoveNotifyObject(const AObject: TImagingGraphic);
begin
  FPause := True;
  //if animation cycle waiting for signal then send the signal manually
  if AEvent <> 0 then
    SetEvent(AEvent);

  Suspend;

  if FGraphicList.Count = 0 then Exit;

  if FGraphicList.IndexOf(AObject) >= 0 then
    FGraphicList.Remove(AObject);

  if FGraphicList.Count <> 0 then
  begin
    FPause := False;
    Resume;
  end;
end;

procedure TPainterThread.Synchronize(AMethod: TThreadMethod);
var
  SyncMsg: TMessage;
begin
  //the code below is based on the fact that hInstance and MainInstance in dll are differs.
  //There are some problems when calling the standard Synchronize method inside dlls.
  //Here we use a window in the main thread and SendMessage functiona as a waitfor replacement
  if HInstance <> MainInstance then
  begin
    SyncMsg.Msg := WM_SynchronizeMe;
    SyncMsg.WParam := LongInt(@TMethod(AMethod));

    with SyncMsg do
      SendMessage(FSyncWnd.Handle, Msg, WParam, LParam);
  end
  else
    inherited Synchronize(AMethod);
end;

{ TRefMultiImage }

constructor TRefMultiImage.Create;
begin
  inherited;
  FRefCount := 0;
end;

function TRefMultiImage.GetCopy: TRefMultiImage;
begin
  Result := nil;
  if Self <> nil then
  begin
    Inc(FRefCount);
    Result := Self;
  end;
end;

procedure TRefMultiImage.ReferredFree(const DeleteFromList: Boolean);
var
  I: Integer;
begin
  if Self = nil then Exit;

  if FRefCount > 0 then
    Dec(FRefCount);

  if (FRefCount = 0) and DeleteFromList then
  with HashedPicList do
  begin
    I := IndexOfObject(Self);
    if I >= 0 then
      Delete(I);
  end;

  if FRefCount = 0 then
    Free;
end;

{$IFNDEF DONT_LINK_GIF}
{ TImagingGifGraphics }

destructor TImagingGifGraphic.Destroy;
begin
  if TestImage(FLastFrame) then
    FreeImage(FLastFrame);
  inherited;
end;

procedure TImagingGifGraphic.AfterLoad(Stream: TStream);
var
  GlobPal: Pointer;
  I: Integer;
begin
  if (FMultImage <> nil) then
  begin
    if (FMultImage.RefCount <= 1) then
    begin
      //to reduce memory usage, Free all palettes, that are referenced to global palette
      GlobPal := nil;
      for I := 0 to FMultImage.ImageCount - 1 do
      begin
        if not GifData(I).IsLocalPalette then
        begin
          if GlobPal = nil then
            GlobPal := FMultImage.DataArray[I].Palette
          else
            FreeMemNil(FMultImage.DataArray[I].Palette);
          GifData(I).Palette := GlobPal;
        end
        else
          with GifData(I) do
          if Transparent then
            FMultImage.DataArray[I].Palette[TransparentIndex].A := 0;
      end;
    end;
    //in case of self animated this will control frames from looping
    FRepeatCount := 0;
    if GifData(0) <> nil then
      FRepeatCount := GifData(0).LoopCount;
  end;
end;

procedure TImagingGifGraphic.DoOverlay(const FrameIndex: Integer; var Frame, BG: TImageData);
var
  Last: Integer;
  First: Integer;
  iGIFData: TGifExtraData;
  J: Integer;
  FrameBufferImg, FrameCopy: TImageData;
  UseCache: Boolean;
  
  procedure CopyFrameTransparent32(const Image, Frame: TImageData; Left, Top: Integer);
  var
    X, Y: Integer;
    Src, Dst: PColor32;
  begin
    Src := Frame.Bits;

    // Copy all pixels from frame to log screen but ignore the transparent ones
    for Y := 0 to Frame.Height - 1 do
    begin
      Dst := @PColor32RecArray(Image.Bits)[(Top + Y) * Image.Width + Left];
      for X := 0 to Frame.Width - 1 do
      begin
        if (TColor32Rec(Src^).A <> 0) then
          Dst^ := Src^;
        Inc(Src);
        Inc(Dst);
      end;
    end;
  end;

begin
  with FMultImage do
  begin
    //frames overlaying algorithm. Overworked from JEDI source (JvGIFCtrl.pas)
    Last := FrameIndex;
    First := Max(0, Last);
    iGIFData := GifData(FrameIndex);

    UseCache := (TestImage(FLastFrame)) and (FCacheIndex = FrameIndex - 1) and (FCacheIndex >= 0) and
                (GifData(FCacheIndex).DisposalMethod <> dmRestorePrevious);

    InitImage(FrameCopy);

    if UseCache then
    begin
      CloneImage(FLastFrame, FrameCopy)
    end
    else
    begin
      FreeImage(FLastFrame);
      NewImage(BG.Width, BG.Height, ifA8R8G8B8, FrameCopy);
    end;

    if not UseCache then
    begin
      while First > 0 do
      begin
        if (iGIFData.GlobalWidth = GifData(First).RealFrameWidth) and
           (iGIFData.GlobalHeight = GifData(First).RealFrameHeight) then
        begin
          if ((GifData(First).DisposalMethod = dmRestoreBackground) and
             (First < Last)) then
            Break;
        end;
       // if TestImage(FLastFrame) and (First = FCacheIndex) then Break;
        Dec(First);
      end;
      
      for J := First to Last - 1 do
      begin
        case GifData(J).DisposalMethod of
          dmNoRemoval, dmLeave:
          begin
            //we copy just a meaning bytes, not all the frame
            InitImage(FrameBufferImg);
            PrepareFrame(J, FrameBufferImg, nil);
            CopyFrameTransparent32(FrameCopy, FrameBufferImg, GifData(J).FrameLeft, GifData(J).FrameTop);
            FreeImage(FrameBufferImg);
          end;
          dmRestoreBackground:
            begin
              if (J > First) then
              begin
                //filling the rect with background is equal to "clearing" it
                CopyRect(BG, GifData(J).FrameLeft, GifData(J).FrameTop,
                         GifData(J).RealFrameWidth, GifData(J).RealFrameHeight,
                         FrameCopy, GifData(J).FrameLeft, GifData(J).FrameTop);
              end;
            end;
          dmRestorePrevious:
            begin { do nothing }

            end;
        end;//case
      end;//for   *)
    end//not UseCache
    else
      with GifData(FCacheIndex) do
      if DisposalMethod = dmRestoreBackground then
        CopyRect(BG, FrameLeft, FrameTop, RealFrameWidth, RealFrameHeight,
                 FrameCopy, FrameLeft, FrameTop);

    with iGifData do
      CopyFrameTransparent32(FrameCopy, Frame, FrameLeft, FrameTop);

    CloneImage(FrameCopy, FLastFrame);
    FCacheIndex := FrameIndex;

    CopyFrameTransparent32(BG, FrameCopy, 0, 0);
    CloneImage(BG, Frame);
    FreeImage(FrameCopy);
  end;
end;

class function TImagingGifGraphic.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TGIFFileFormat);
end;

function TImagingGifGraphic.GetHeight: Integer;
begin
  Result := 0;
  if Empty then Exit;

  if GifData(0) <> nil then
    Result := GifData(0).GlobalHeight;
end;

function TImagingGifGraphic.GetWidth: Integer;
begin
  Result := 0;
  if Empty then Exit;

  if GifData(0) <> nil then
    Result := GifData(0).GlobalWidth;
end;

function TImagingGifGraphic.GifData(const Index: Integer): TGifExtraData;
begin
  Result := nil;
  if GetEmpty or (Index < 0) or (Index > FMultImage.ImageCount - 1) then Exit;
  Result := TGifExtraData(FMultImage.DataArray[Index].Extra);
end;

procedure TImagingGifGraphic.DoPaintTriggered;
begin
  if Empty then Exit;

  if (GifData(0) <> nil) and (GifData(0).LoopCount <> 0) then
  begin
    if FRepeatCount <= 0 then Exit;
    //if gif animation is not coninued looping, then image should be stopped after RepeatCount is 0
    if (FActiveImage = FMultImage.ImageCount - 1) and (FRepeatCount > 0) then
      Dec(FRepeatCount);

    //when reached to the end of repeat, set activeframe to the last - 1, so the PaintTriggered
    //could set it to last frame
    if FRepeatCount <= 0 then
      FActiveImage := Max(FMultImage.ImageCount - 2, 0)
  end;

  inherited DoPaintTriggered;
end;

procedure TImagingGifGraphic.PrepareFrame(const FrameIndex: Integer; var Frame: TImageData; Back: PImageData);
begin
  //if frame referenced to global palette, then we should copy it to the Frame.Palette
  if (GifData(FrameIndex) <> nil) and not GifData(FrameIndex).IsLocalPalette then
  begin
    InitImage(Frame);
    with GifData(FrameIndex) do
      NewImage(RealFrameWidth, RealFrameHeight, ifIndex8, Frame);

    Move(GifData(FrameIndex).Palette^, Frame.Palette^, SizeOf(TPalette32Size256));

    Move(FMultImage.DataArray[FrameIndex].Bits^, Frame.Bits^, FMultImage.Images[FrameIndex].Size);

    Frame.Format := ifIndex8;
    Frame.Size := FMultImage.Images[FrameIndex].Size;
    Frame.Width := FMultImage.Images[FrameIndex].Width;
    Frame.Height := FMultImage.Images[FrameIndex].Height;

    with GifData(FrameIndex) do
    if Transparent then
      Frame.Palette[TransparentIndex].A := 0;

//    if (Back <> nil) or ((Back = nil) and not GifData(FrameIndex).AllGlobal) then
      ConvertImage(Frame, ifA8R8G8B8);
  end
  else
  begin
    //else if local then just convert to ifA8R8G8B8
    if (GifData(FrameIndex) <> nil) then
      inherited PrepareFrame(FrameIndex, Frame, nil)
    else
      inherited;
  end;

  //last step: overlay previous frames to current, according to DisposalMethod
  if (Back <> nil) and (GifData(FrameIndex) <> nil) then
    DoOverlay(FrameIndex, Frame, Back^);
end;

{$ENDIF}

{ TDllSynchroClass }
constructor TDllSynchroClass.Create;
begin
  FSynchronizeWindow := AllocateHWnd(WndProc);
end;

procedure TDllSynchroClass.DefaultHandler(var Message);
begin
  with TMessage(Message) do
    Result := DefWindowProc(FSynchronizeWindow, Msg, WParam, LParam);
end;

destructor TDllSynchroClass.Destroy;
begin
  DestroyWindow(FSynchronizeWindow);
  inherited;
end;

type
  PMethod = ^TMethod;

procedure TDllSynchroClass.SynchronizeMe(var Message: TMessage);
begin
  try
    TThreadMethod(PMethod(Message.WParam)^)();
  except

  end;
end;

procedure TDllSynchroClass.WndProc(var Message: TMessage);
begin
  Dispatch(Message);
end;

initialization
  RegisterTypes;

  HashedPicList := TStringList.Create;
  HashedPicList.Sorted := True;
  PainterThread := TPainterThread.Create(True);
  PainterThread.Priority := tpNormal;

finalization
  UnRegisterTypes;

  HashedPicList.Free;
  PainterThread.FPause := True;
  PainterThread.Terminate;
  PainterThread.Free;
{$IFEND} // {$IF not Defined(COMPONENT_SET_LCL) and not Defined(COMPONENT_SET_VCL)}
{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Added some more IFDEFs for Lazarus widget sets.
    - Removed CLX code.
    - GTK version of Unix DisplayImageData only used with LCL GTK so the
      the rest of the unit can be used with Qt or other LCL interfaces. 
    - Fallback mechanism for DisplayImageDataOnDC, it may fail on occasions.
    - Changed file format conditional compilation to reflect changes
      in LINK symbols.
    - Lazarus 0.9.26 compatibility changes.

  -- 0.24.1 Changes/Bug Fixes ---------------------------------
    - Fixed wrong IFDEF causing that Imaging wouldn't compile in Lazarus
      with GTK2 target.
    - Added commnets with code for Lazarus rev. 11861+ regarding
      RawImage interface. Replace current code with that in comments
      if you use Lazarus from SVN. New RawImage interface will be used by
      default after next Lazarus release.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added TImagingGIF. 

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Uses only high level interface now (except for saving options).
    - Slightly changed class hierarchy. TImagingGraphic is now only for loading
      and base class for savers is new TImagingGraphicForSave. Also
      TImagingGraphic is now registered with all supported file formats
      by TPicture's format support.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added DisplayImage procedures (thanks to Paul Michell, modified)
    - removed RegisterTypes and UnRegisterTypes from interface section,
      they are called automatically
    - added procedures: ConvertImageToBitmap and ConvertBitmapToImage

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - LCL data to bitmap conversion didn´t work in Linux, fixed
    - added MNG file format
    - added JNG file format

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - made it LCL compatible
    - made it CLX compatible
    - added all initial stuff
}

end.

