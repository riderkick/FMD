New Imaging GIF implementation readme.

Since the release 0.26.2 a new TImagingGraphic class appears at Extra directory. The new class is designed for full integration with the VCL and provides an opportunity for self-animation of it's content in TImage-like components. You will need MD5 unit implementation from the ICS(http://www.overbyte.be) to make an extension working. Please read this information carefully!!

-------------============ USAGE ============-------------

To make TImage able to display animated GIF, simply add the ImagingComponents unit to the uses clause and load graphics via the Picture property: 
Image1.Picture.LoadFromFile('test.gif');

-------------============ HOW IT'S WORKS ============-------------

The base class for any kind of self-animated graphics is TImagingGraphic. This class provides the necessary basic properties and methods to deal with some animated graphics formats:

property Frames: TRefMultiImage read FMultImage;

that property provides the access to the frame information - size, palette etc(see the TImageData record) and some Extra data that depends on a specific graphics format (now using only in ImagingGif unit, see TGifExtraData. will be described below). Also, to reduce memory usage, TImagingGraphic uses own reference count so the multiply copies of the same graphics will use one shareable data (see notes below)

property ActiveIndex: Integer

that property controls current animation step to be shown. Set it before drawing the frame to destination.

property SelfAnimated: Boolean

that property provides the control over self-animation of graphics. When displaying the frame to the destination at the first time, the class is placed to the inner animation list a special thread begins to calculate and wait for time period that is given by GetCurrentDelay method.

property ReAnimate: Boolean

that property controls readding the class to the animation list if the class was deleted from. To reduce the process load, thread check the visibility of destination canvas via ParentControlVisible method. If the destination is invisible, the class removed from the animation list. When destinaition becomes visible the parent control will call redrawing and the class might be added again.

property BackGroundBitmap: TBitmap
property BackgroundSharing: Boolean
property BackGroundColor: TColor

Those three properties are intended to set the background for the animation manually. In some rare cases the artefacts might be shown when drawing(if none of these properties were set, when the first draw takes place the class copies the destination background itself. if the withdrawal was overlapped by anything, after it is shown there could be garbage there). The priority of properties is:
1. BackgroundBitmap is set
2. BackgroundBitmap is not set, but BackGroundColor is
3. none of the properties are set, copy the destination backround
The BackgroundSharing property might be useful to use a shareable bitmap for several copies of class. Set to True if you control th lifetime of the bitmap by yourself. Set to False and the class will be responsible for bitmap destroying.

constructor Create(const Referenced: Boolean); overload;
procedure UnRefImage;

By default, TImagingGraphic uses own reference count according to md5 of the stream content. If you don't need memory reducing or you might want to have multiple independent copies, use UnRefImage or create with that constructor.

-------------============ TIMAGEEXTRA FIELD EXPLAINATION ============-------------
The graphics that are non-animated formats will set TImageData.Extra to nil. Animated descendants should create the class that inherits from TExtraData class. The base methods and properties:

procedure AssignFrom(Source: TExtraData); virtual;

That method is similar to TPersistent.AssignFrom. Use it to assign specific properties valies when copy one TImageData to another

procedure ImageResized(var NewWidth, NewHeight: Integer); virtual;

This method is intended for notification that the size of the frame have been altered. Useful if additional data depends on the size of the frame (TImagingGifGraphic extra data are size-dependent)

procedure ContentChanged(const Bits, Palette: Pointer); virtual;

This method is intended for notification that the content of the frame have been altered. Useful if additional data depends on the content of the frame

property Delay: Integer read FDelay write FDelay;

The main property for animated fomats. It is the animation delay in microseconds(ticks)

-------------============ TIMAGINGGIFGRAPHIC AND TGIFEXTRADATA USAGE ============-------------

The most of the properties and useful methods were described above. TImagingGifGraphic (or TGifImage) provides one more additional method for extra data reading:
function GifData(const Index: Integer): TGifExtraData;

The TGifExtraData class contains several useful information about the frame

    property DisposalMethod: TDisposalMethod 
	
  frame disposalmethod. This controls the drawing behavior
  
    property LocalPaletteLength: Word 
	property GlobalPaletteLength: Word 
	
  this property returns local or global palette length
	
    property IsLocalPalette: Boolean 
	
  the property tells if the frames palette local or global
  
    property Transparent: Boolean 
	
  Is the frame transparent?
  
    property TransparentIndex: Integer 
	
  the index of the transparent color in palette
  
    property BackGroundIndex: Integer 
	
  the index of background color of the frame (this may be the same to TransparentIndex)
  
    property RealFrameWidth: Integer 
    property RealFrameHeight: Integer 
	
  Animation width and height are caclulated according to all the frames widths, heights and offsets. These property shows the real frames width and height
  
    property FrameLeft: Integer 
    property FrameTop: Integer 
	
  Frame offset properties
  
    property GlobalWidth: Word 
    property GlobalHeight: Word 
	
  the animation size properries
  
    property Palette: PPalette32 
	
  this property is a pointer to the real palette (will be described below)
  
    property AllLocal: Boolean 
    property AllGlobal: Boolean 
	
  these properties tells that all the frames in animation contains local or global palettes
  
    property LoopCount: Cardinal 
	
  this properies controls the repeat count of self-animation
    
The base data container in Imaging library is TImageData. Each frame has it's own bits or palette copy. To reduce memory usage, TImagingGifGraphic releases the palette memory of all the frames with global palette and points Palette property of their TGifExtraData to the first one. 

DO NOT WORK WITH Frames PROPERTY DIRECTLY! USE PrepareFrame method INSTEAD TO GET FRAME COPY!!!

Most of the gif animations are infinite looped (LoopCount = 0) and they are animated continuesly. If LoopCount is not 0, the self-animation will stop when reached to the end of looping and will show the last frame.

The loading of gif is not perfect. Some of nasty gifs might contain incorrect data and those frames are deleted from the animation.

-------------============ RESTRICTIONS ============-------------
Since the ImagingLib works with ifIndex8 format (which means 8bit color) the result file size when saving through TImagingGifGraphic might be bigger then the original. So, if you want to keep the original file size, save it using TFileStream for example.
TImagingGifGraphic also works only with one type of Gif Application Extension - NETSCAPE2.0. All the other appextensions, comments and plain text will be ignored. They will be lost when saving to file too.
If the result animation width or height was changed, all the frames will be scaled to the new size.



_____________________________
Sergey Galezdinov aka Sega-Zero. 2008.