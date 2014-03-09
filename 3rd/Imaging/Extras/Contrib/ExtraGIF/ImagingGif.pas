{
  $Id: ImagingGif.pas 169 2009-08-22 18:54:21Z galfar $
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

{ This unit contains image format loader/saver for GIF images.

  Note: This modified unit is part of ExtraGIF mod of Imaging library.}
unit ImagingGif;

{$I ImagingOptions.inc}

interface

uses
  windows, SysUtils, Classes, Imaging, ImagingTypes, ImagingIO, ImagingUtility, ImagingCanvases,
  ImagingFormats;

type
  { GIF (Graphics Interchange Format) loader/saver class. GIF was
    (and is still used) popular format for storing images supporting
    multiple images per file and single color transparency.
    Pixel format is 8 bit indexed where each image frame can have
    its own color palette. GIF uses lossless LZW compression
    (patent expired few years ago).
    Imaging can load and save all GIFs with all frames and supports
    transparency.}
  TGIFFileFormat = class(TImageFileFormat)
  private
    function InterlaceStep(Y, Height: Integer; var Pass: Integer): Integer;
    procedure LZWDecompress(Stream: TStream; Handle: TImagingHandle;
      Width, Height: Integer; Interlaced: Boolean; Data: Pointer);
    procedure LZWCompress(const IO: TIOFunctions; Handle: TImagingHandle;
      Width, Height, BitCount: Integer; Interlaced: Boolean; Data: Pointer);
  protected
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    constructor Create; override;
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

type
  TDisposalMethod = (dmNoRemoval, dmLeave, dmRestoreBackground, dmRestorePrevious, dmReserved4,
                     dmReserved5, dmReserved6, dmReserved7);

  TGifExtraData = class(TExtraData)
  private
    FDsipMthd: TDisposalMethod;
    FPalLen: Word;
    FTrans: Boolean;
    FTransInd: Integer;
    FRealWdth: Integer;
    FGlobHgth: Word;
    FRealHgth: Integer;
    FGlobWdth: Word;
    FLocal: Boolean;
    FTop: Integer;
    FLeft: Integer;
    FPal: PPalette32;
    FBGInd: Integer;
    FAllLocal: Boolean;
    FLoop: Cardinal;
    FGPalLen: Word;
    FIntrlcd: Boolean;
    FAllGlobal: Boolean;

  public
    procedure ImageResized(var NewWidth: Integer; var NewHeight: Integer); override;
    procedure AssignFrom(Source: TExtraData); override;

    property DisposalMethod: TDisposalMethod read FDsipMthd write FDsipMthd;
    property LocalPaletteLength: Word read FPalLen write FPalLen;
    property IsLocalPalette: Boolean read FLocal write FLocal;
    property Transparent: Boolean read FTrans write FTrans;
    property TransparentIndex: Integer read FTransInd write FTransInd;
    property BackGroundIndex: Integer read FBGInd write FBGInd;
    property RealFrameWidth: Integer read FRealWdth write FRealWdth;
    property RealFrameHeight: Integer read FRealHgth write FRealHgth;
    property FrameLeft: Integer read FLeft write FLeft;
    property FrameTop: Integer read FTop write FTop;
    property GlobalWidth: Word read FGlobWdth write FGlobWdth;
    property GlobalHeight: Word read FGlobHgth write FGlobHgth;
    property Palette: PPalette32 read FPal write FPal;
    property AllLocal: Boolean read FAllLocal write FAllLocal;
    property AllGlobal: Boolean read FAllGlobal write FAllGlobal;
    property LoopCount: Cardinal read FLoop write FLoop;
    property Interlaced: Boolean read FIntrlcd write FIntrlcd;
    property GlobalPaletteLength: Word read FGPalLen write FGPalLen;
  end;

implementation

//uses
//  DBugIntf;

const
  SGIFFormatName = 'Graphics Interchange Format';
  SGIFMasks      = '*.gif';
  GIFSupportedFormats: TImageFormats = [ifIndex8];

type
  TGIFVersion = (gv87, gv89);

const
  GIFSignature: TChar3 = 'GIF';
  GIFVersions: array[TGIFVersion] of TChar3 = ('87a', '89a');

  // Masks for accessing fields in PackedFields of TGIFHeader
  GIFGlobalColorTable = $80;
  GIFColorResolution  = $70;
  GIFColorTableSorted = $08;
  GIFColorTableSize   = $07;

  // Masks for accessing fields in PackedFields of TImageDescriptor
  GIFLocalColorTable  = $80;
  GIFInterlaced       = $40;
  GIFLocalTableSorted = $20;

  // Block identifiers
  GIFPlainText: Byte               = $01;
  GIFGraphicControlExtension: Byte = $F9;
  GIFCommentExtension: Byte        = $FE;
  GIFApplicationExtension: Byte    = $FF;
  GIFImageDescriptor: Byte         = Ord(',');
  GIFExtensionIntroducer: Byte     = Ord('!');
  GIFTrailer: Byte                 = Ord(';');
  GIFBlockTerminator: Byte         = $00;

  // Masks for accessing fields in PackedFields of TGraphicControlExtension
  GIFTransparent    = $01;
  GIFUserInput      = $02;
  GIFDisposalMethod = $1C;

type
  TGIFHeader = packed record
    // File header part
    Signature: TChar3;  // Header Signature (always "GIF")
    Version: TChar3;    // GIF format version("87a" or "89a")
    // Logical Screen Descriptor part
    ScreenWidth: Word;  // Width of Display Screen in Pixels
    ScreenHeight: Word; // Height of Display Screen in Pixels
    PackedFields: Byte; // Screen and color map information
    BackgroundColorIndex: Byte; // Background color index (in global color table)
    AspectRatio: Byte;  // Pixel aspect ratio, ratio = (AspectRatio + 15) / 64
  end;

  TImageDescriptor = packed record
    //Separator: Byte; // leave that out since we always read one bye ahead
    Left: Word;        // X position of image with respect to logical screen
    Top: Word;         // Y position
    Width: Word;
    Height: Word;
    PackedFields: Byte;
  end;

const
  // GIF extension labels
  GIFExtTypeGraphic     = $F9;
  GIFExtTypePlainText   = $01;
  GIFExtTypeApplication = $FF;
  GIFExtTypeComment     = $FE;

type
  TGraphicControlExtension = packed record
    BlockSize: Byte;
    PackedFields: Byte;
    DelayTime: Word;
    TransparentColorIndex: Byte;
    Terminator: Byte;
  end;

const
  // Netscape sub block types
  GIFAppLoopExtension = 1;
  GIFAppBufferExtension = 2;

type
  TGIFIdentifierCode = array[0..7] of char;
  TGIFAuthenticationCode = array[0..2] of char;
  TGIFApplicationRec = packed record
    Identifier		  : TGIFIdentifierCode;
    Authentication	: TGIFAuthenticationCode;
  end;

const
  CodeTableSize = 4096;
  HashTableSize = 17777;
  
type
  TReadContext = record
    Inx: Integer;
    Size: Integer;
    Buf: array [0..255 + 4] of Byte;
    CodeSize: Integer;
    ReadMask: Integer;
  end;
  PReadContext = ^TReadContext;

  TWriteContext = record
    Inx: Integer;
    CodeSize: Integer;
    Buf: array [0..255 + 4] of Byte;
  end;
  PWriteContext = ^TWriteContext;

  TOutputContext = record
    W: Integer;
    H: Integer;
    X: Integer;
    Y: Integer;
    BitsPerPixel: Integer;
    Pass: Integer;
    Interlace: Boolean;
    LineIdent: Integer;
    Data: Pointer;
    CurrLineData: Pointer;
  end;

  TImageDict = record
    Tail: Word;
    Index: Word;
    Col: Byte;
  end;
  PImageDict = ^TImageDict;

  PIntCodeTable = ^TIntCodeTable;
  TIntCodeTable = array [0..CodeTableSize - 1] of Word;

  TDictTable = array [0..CodeTableSize - 1] of TImageDict;
  PDictTable = ^TDictTable;

resourcestring
  SGIFDecodingError = 'Error when decoding GIF LZW data';

{
  TGIFFileFormat implementation
}

constructor TGIFFileFormat.Create;
begin
  inherited Create;
  FName := SGIFFormatName;
  FCanLoad := True;
  FCanSave := True;
  FIsMultiImageFormat := True;
  FSupportedFormats := GIFSupportedFormats;

  AddMasks(SGIFMasks);
end;

function TGIFFileFormat.InterlaceStep(Y, Height: Integer; var Pass: Integer): Integer;
begin
  Result := Y;
  case Pass of
    0, 1:
      Inc(Result, 8);
    2:
      Inc(Result, 4);
    3:
      Inc(Result, 2);
  end;
  if Result >= Height then
  begin
    if Pass = 0 then
    begin
      Pass := 1;
      Result := 4;
      if Result < Height then
        Exit;
    end;
    if Pass = 1 then
    begin
      Pass := 2;
      Result := 2;
      if Result < Height then
        Exit;
    end;
    if Pass = 2 then
    begin
      Pass := 3;
      Result := 1;
    end;
  end;
end;

{ GIF LZW decompresion code is from JVCL JvGIF.pas unit.}
procedure TGIFFileFormat.LZWDecompress(Stream: TStream; Handle: TImagingHandle; Width, Height: Integer;
  Interlaced: Boolean; Data: Pointer);
var
  MinCodeSize: Byte;
  MaxCode, BitMask, InitCodeSize: Integer;
  ClearCode, EndingCode, FirstFreeCode, FreeCode: Word;
  I, OutCount, Code: Integer;
  CurCode, OldCode, InCode, FinalChar: Word;
  Prefix, Suffix, OutCode: PIntCodeTable;
  ReadCtxt: TReadContext;
  OutCtxt: TOutputContext;
  TableFull: Boolean;

  function ReadCode(var Context: TReadContext): Integer;
  var
    RawCode: Integer;
    ByteIndex: Integer;
    Bytes: Byte;
    BytesToLose: Integer;
  begin
    while (Context.Inx + Context.CodeSize > Context.Size) and
      (Stream.Position < Stream.Size) do
    begin
      // Not enough bits in buffer - refill it - Not very efficient, but infrequently called
      BytesToLose := Context.Inx shr 3;
      // Note biggest Code Size is 12 bits. And this can at worst span 3 Bytes
      Move(Context.Buf[Word(BytesToLose)], Context.Buf[0], 3);
      Context.Inx := Context.Inx and 7;
      Context.Size := Context.Size - (BytesToLose shl 3);
      Stream.Read(Bytes, 1);
      if Bytes > 0 then
        Stream.Read(Context.Buf[Word(Context.Size shr 3)], Bytes);
      Context.Size := Context.Size + (Bytes shl 3);
    end;
    ByteIndex := Context.Inx shr 3;
    RawCode := Context.Buf[Word(ByteIndex)] +
      (Word(Context.Buf[Word(ByteIndex + 1)]) shl 8);
    if Context.CodeSize > 8 then
      RawCode := RawCode + (LongInt(Context.Buf[ByteIndex + 2]) shl 16);
    RawCode := RawCode shr (Context.Inx and 7);
    Context.Inx := Context.Inx + Byte(Context.CodeSize);
    Result := RawCode and Context.ReadMask;
  end;

  procedure Output(Value: Byte; var Context: TOutputContext);
  var
    P: PByte;
  begin
    if Context.Y >= Context.H then
      Exit;

    // Only ifIndex8 supported
    P := @PByteArray(Context.CurrLineData)[Context.X];
    P^ := Value;

    {case Context.BitsPerPixel of
      1:
        begin
          P := @PByteArray(Context.CurrLineData)[Context.X shr 3];
          if (Context.X and $07) <> 0 then
            P^ := P^ or Word(Value shl (7 - (Word(Context.X and 7))))
          else
            P^ := Byte(Value shl 7);
        end;
      4:
        begin
          P := @PByteArray(Context.CurrLineData)[Context.X shr 1];
          if (Context.X and 1) <> 0 then
            P^ := P^ or Value
          else
            P^ := Byte(Value shl 4);
        end;
      8:
        begin
          P := @PByteArray(Context.CurrLineData)[Context.X];
          P^ := Value;
        end;
    end;}
    Inc(Context.X);

    if Context.X < Context.W then
      Exit;
    Context.X := 0;
    if Context.Interlace then
      Context.Y := InterlaceStep(Context.Y, Context.H, Context.Pass)
    else
      Inc(Context.Y);

    Context.CurrLineData := @PByteArray(Context.Data)[Context.Y * Context.LineIdent];
  end;

begin
  OutCount := 0;
  OldCode := 0;
  FinalChar := 0;
  TableFull := False;
  GetMem(Prefix, SizeOf(TIntCodeTable));
  GetMem(Suffix, SizeOf(TIntCodeTable));
  GetMem(OutCode, SizeOf(TIntCodeTable) + SizeOf(Word));
  try
    Stream.Read(MinCodeSize, 1);
    if (MinCodeSize < 2) or (MinCodeSize > 9) then
      RaiseImaging(SGIFDecodingError, []);
    // Initial read context
    ReadCtxt.Inx := 0;
    ReadCtxt.Size := 0;
    ReadCtxt.CodeSize := MinCodeSize + 1;
    ReadCtxt.ReadMask := (1 shl ReadCtxt.CodeSize) - 1;
    // Initialise pixel-output context
    OutCtxt.X := 0;
    OutCtxt.Y := 0;
    OutCtxt.Pass := 0;
    OutCtxt.W := Width;
    OutCtxt.H := Height;
    OutCtxt.BitsPerPixel := MinCodeSize;
    OutCtxt.Interlace := Interlaced;
    OutCtxt.LineIdent := Width;
    OutCtxt.Data := Data;
    OutCtxt.CurrLineData := Data;
    BitMask := (1 shl OutCtxt.BitsPerPixel) - 1;
    // 2 ^ MinCodeSize accounts for all colours in file
    ClearCode := 1 shl MinCodeSize;
    EndingCode := ClearCode + 1;
    FreeCode := ClearCode + 2;
    FirstFreeCode := FreeCode;
    // 2^ (MinCodeSize + 1) includes clear and eoi Code and space too
    InitCodeSize := ReadCtxt.CodeSize;
    MaxCode := 1 shl ReadCtxt.CodeSize;
    Code := ReadCode(ReadCtxt);
    while (Code <> EndingCode) and (Code <> $FFFF) and
      (OutCtxt.Y < OutCtxt.H) do
    begin
      if Code = ClearCode then
      begin
        ReadCtxt.CodeSize := InitCodeSize;
        MaxCode := 1 shl ReadCtxt.CodeSize;
        ReadCtxt.ReadMask := MaxCode - 1;
        FreeCode := FirstFreeCode;
        Code := ReadCode(ReadCtxt);
        CurCode := Code;
        OldCode := Code;
        if Code = $FFFF then
          Break;
        FinalChar := (CurCode and BitMask);
        Output(Byte(FinalChar), OutCtxt);
        TableFull := False;
      end
      else
      begin
        CurCode := Code;
        InCode := Code;
        if CurCode >= FreeCode then
        begin
          CurCode := OldCode;
          OutCode^[OutCount] := FinalChar;
          Inc(OutCount);
        end;
        while CurCode > BitMask do
        begin
          if OutCount > CodeTableSize then
            RaiseImaging(SGIFDecodingError, []);
          OutCode^[OutCount] := Suffix^[CurCode];
          Inc(OutCount);
          CurCode := Prefix^[CurCode];
        end;

        FinalChar := CurCode and BitMask;
        OutCode^[OutCount] := FinalChar;
        Inc(OutCount);
        for I := OutCount - 1 downto 0 do
          Output(Byte(OutCode^[I]), OutCtxt);
        OutCount := 0;
        // Update dictionary
        if not TableFull then
        begin
          Prefix^[FreeCode] := OldCode;
          Suffix^[FreeCode] := FinalChar;
          // Advance to next free slot
          Inc(FreeCode);
          if FreeCode >= MaxCode then
          begin
            if ReadCtxt.CodeSize < 12 then
            begin
              Inc(ReadCtxt.CodeSize);
              MaxCode := MaxCode shl 1;
              ReadCtxt.ReadMask := (1 shl ReadCtxt.CodeSize) - 1;
            end
            else
              TableFull := True;
          end;
        end;
        OldCode := InCode;
      end;
      Code := ReadCode(ReadCtxt);
    end;
    if Code = $FFFF then
      RaiseImaging(SGIFDecodingError, []);
  finally
    FreeMem(Prefix);
    FreeMem(OutCode);
    FreeMem(Suffix);
  end;
end;

{ GIF LZW compresion code is from JVCL JvGIF.pas unit.}
procedure TGIFFileFormat.LZWCompress(const IO: TIOFunctions; Handle: TImagingHandle; Width, Height, BitCount: Integer;
    Interlaced: Boolean; Data: Pointer);
var
  LineIdent: Integer;
  MinCodeSize, Col: Byte;
  InitCodeSize, X, Y: Integer;
  Pass: Integer;
  MaxCode: Integer; { 1 shl CodeSize }
  ClearCode, EndingCode, LastCode, Tail: Integer;
  I, HashValue: Integer;
  LenString: Word;
  Dict: PDictTable;
  HashTable: TList;
  PData: PByte;
  WriteCtxt: TWriteContext;

  function InitHash(P: Integer): Integer;
  begin
    Result := (P + 3) * 301;
  end;

  procedure WriteCode(Code: Integer; var Context: TWriteContext);
  var
    BufIndex: Integer;
    Bytes: Byte;
  begin
    BufIndex := Context.Inx shr 3;
    Code := Code shl (Context.Inx and 7);
    Context.Buf[BufIndex] := Context.Buf[BufIndex] or Byte(Code);
    Context.Buf[BufIndex + 1] := Byte(Code shr 8);
    Context.Buf[BufIndex + 2] := Byte(Code shr 16);
    Context.Inx := Context.Inx + Context.CodeSize;
    if Context.Inx >= 255 * 8 then
    begin
      // Flush out full buffer
      Bytes := 255;
      IO.Write(Handle, @Bytes, 1);
      IO.Write(Handle, @Context.Buf, Bytes);
      Move(Context.Buf[255], Context.Buf[0], 2);
      FillChar(Context.Buf[2], 255, 0);
      Context.Inx := Context.Inx - (255 * 8);
    end;
  end;

  procedure FlushCode(var Context: TWriteContext);
  var
    Bytes: Byte;
  begin
    Bytes := (Context.Inx + 7) shr 3;
    if Bytes > 0 then
    begin
      IO.Write(Handle, @Bytes, 1);
      IO.Write(Handle, @Context.Buf, Bytes);
    end;
    // Data block terminator - a block of zero Size
    Bytes := 0;
    IO.Write(Handle, @Bytes, 1);
  end;

begin
  LineIdent := Width;
  Tail := 0;
  HashValue := 0;
  Col := 0;
  HashTable := TList.Create;
  GetMem(Dict, SizeOf(TDictTable));
  try
    for I := 0 to HashTableSize - 1 do
      HashTable.Add(nil);

    // Initialise encoder variables
    InitCodeSize := BitCount + 1;
    if InitCodeSize = 2 then
      Inc(InitCodeSize);
    MinCodeSize := InitCodeSize - 1;
    IO.Write(Handle, @MinCodeSize, 1);
    ClearCode := 1 shl MinCodeSize;
    EndingCode := ClearCode + 1;
    LastCode := EndingCode;
    MaxCode := 1 shl InitCodeSize;
    LenString := 0;
    // Setup write context
    WriteCtxt.Inx := 0;
    WriteCtxt.CodeSize := InitCodeSize;
    FillChar(WriteCtxt.Buf, SizeOf(WriteCtxt.Buf), 0);
    WriteCode(ClearCode, WriteCtxt);
    Y := 0;
    Pass := 0;

    while Y < Height do
    begin
      PData := @PByteArray(Data)[Y * LineIdent];
      for X := 0 to Width - 1 do
      begin
        // Only ifIndex8 support
        case BitCount of
          8:
            begin
              Col := PData^;
              PData := @PByteArray(PData)[1];
            end;
          {4:
            begin
              if X and 1 <> 0 then
              begin
                Col := PData^ and $0F;
                PData := @PByteArray(PData)[1];
              end
              else
                Col := PData^ shr 4;
            end;
          1:
            begin
              if X and 7 = 7 then
              begin
                Col := PData^ and 1;
                PData := @PByteArray(PData)[1];
              end
              else
                Col := (PData^ shr (7 - (X and $07))) and $01;
            end;}
        end;
        Inc(LenString);
        if LenString = 1 then
        begin
          Tail := Col;
          HashValue := InitHash(Col);
        end
        else
        begin
          HashValue := HashValue * (Col + LenString + 4);
          I := HashValue mod HashTableSize;
          HashValue := HashValue mod HashTableSize;
          while (HashTable[I] <> nil) and
            ((PImageDict(HashTable[I])^.Tail <> Tail) or
            (PImageDict(HashTable[I])^.Col <> Col)) do
          begin
            Inc(I);
            if I >= HashTableSize then
              I := 0;
          end;
          if HashTable[I] <> nil then // Found in the strings table
            Tail := PImageDict(HashTable[I])^.Index
          else
          begin
            // Not found
            WriteCode(Tail, WriteCtxt);
            Inc(LastCode);
            HashTable[I] := @Dict^[LastCode];
            PImageDict(HashTable[I])^.Index := LastCode;
            PImageDict(HashTable[I])^.Tail := Tail;
            PImageDict(HashTable[I])^.Col := Col;
            Tail := Col;
            HashValue := InitHash(Col);
            LenString := 1;
            if LastCode >= MaxCode then
            begin
              // Next Code will be written longer
              MaxCode := MaxCode shl 1;
              Inc(WriteCtxt.CodeSize);
            end
            else
            if LastCode >= CodeTableSize - 2 then
            begin
              // Reset tables
              WriteCode(Tail, WriteCtxt);
              WriteCode(ClearCode, WriteCtxt);
              LenString := 0;
              LastCode := EndingCode;
              WriteCtxt.CodeSize := InitCodeSize;
              MaxCode := 1 shl InitCodeSize;
              for I := 0 to HashTableSize - 1 do
                HashTable[I] := nil;
            end;
          end;
        end;
      end;
      if Interlaced then
        Y := InterlaceStep(Y, Height, Pass)
      else
        Inc(Y);
    end;
    WriteCode(Tail, WriteCtxt);
    WriteCode(EndingCode, WriteCtxt);
    FlushCode(WriteCtxt);
  finally
    HashTable.Free;
    FreeMem(Dict);
  end;
end;


function TGIFFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Header: TGIFHeader;
  HasGlobalPal: Boolean;
  GlobalPalLength: Integer;
  GlobalPal: TPalette32Size256;
  I: Integer;
  BlockID: Byte;
  HasGraphicExt: Boolean;
  GraphicExt: TGraphicControlExtension;

  FrameImages: TDynImageDataArray;
  OldGlobalWidth, OldGlobalHeight: Integer;
  bAllGlobal, bAllLocal: Boolean;

  LoopCount: Word;
  AppRec: TGIFApplicationRec;
  AppRead: Boolean;

  function ReadBlockID: Byte;
  begin
    Result := GIFTrailer;
    if GetIO.Read(Handle, @Result, SizeOf(Result)) < SizeOf(Result) then
      Result := GIFTrailer;
  end;

  procedure ReadExtensions;
  var
    BlockSize, BlockType, ExtType: Byte;

    procedure SkipBytes;
    begin
      with GetIO do
      repeat
        // Read block sizes and skip them
        Read(Handle, @BlockSize, SizeOf(BlockSize));
        Seek(Handle, BlockSize, smFromCurrent);
      until BlockSize = 0;
    end;
  begin
    HasGraphicExt := False;

    // Read extensions until image descriptor is found. Only graphic extension
    // is stored now (for transparency), others are skipped.
    while BlockID = GIFExtensionIntroducer do
    with GetIO do
    begin
      Read(Handle, @ExtType, SizeOf(ExtType));

      while ExtType in [GIFGraphicControlExtension, GIFCommentExtension, GIFApplicationExtension, GIFPlainText] do
      try
        if ExtType = GIFGraphicControlExtension then
        begin
          HasGraphicExt := True;
          Read(Handle, @GraphicExt, SizeOf(GraphicExt));
        end
        else
          if (ExtType = GIFApplicationExtension) and not AppRead then
          begin
            Read(Handle, @BlockSize, SizeOf(BlockSize));
            if BlockSize >= SizeOf(AppRec) then
            begin
              Read(Handle, @AppRec, SizeOf(AppRec));
              if (AppRec.Identifier = 'NETSCAPE') and (AppRec.Authentication = '2.0') then
              begin
                Read(Handle, @BlockSize, SizeOf(BlockSize));
                while BlockSize <> 0 do
                begin
                  BlockType := ReadBlockID;
                  Dec(BlockSize);

                  case BlockType of
                    GIFAppLoopExtension:
                      if (BlockSize >= sizeof(LoopCount)) then
                      begin
                        // Read loop count
                        Read(Handle, @LoopCount, SizeOf(LoopCount));
                        dec(BlockSize, sizeof(LoopCount));
                      end;
                    GIFAppBufferExtension:
                      begin
                        Dec(BlockSize, sizeof(Word));
                        Seek(Handle, sizeof(Word), smFromCurrent);
                      end;
                  end;
                end;//while
                SkipBytes;
                AppRead := True;
              end
              else
                begin
                  //revert all bytes reading
                  Seek(Handle, - SizeOf(AppRec) - SizeOf(BlockSize), smFromCurrent);
                  SkipBytes;
                end;
            end
            else
              begin
                Seek(Handle, - BlockSize - SizeOf(BlockSize), smFromCurrent);
                SkipBytes;
              end;
          end
          else       
          if ExtType in [GIFCommentExtension, GIFApplicationExtension, GIFPlainText] then
          repeat
            // Read block sizes and skip them
            Read(Handle, @BlockSize, SizeOf(BlockSize));
            Seek(Handle, BlockSize, smFromCurrent);
          until BlockSize = 0;

        // Read ID of following block
        BlockID := ReadBlockID;
        ExtType := BlockID;
      except
      end
    end;
  end;

  procedure CopyFrameTransparent(const Image, Frame: TImageData; Left, Top, TransIndex: Integer);
  var
    X, Y: Integer;
    Src, Dst: PByte;
  begin
    Src := Frame.Bits;

    // Copy all pixels from frame to log screen but ignore the transparent ones
    for Y := 0 to Frame.Height - 1 do
    begin
      Dst := @PByteArray(Image.Bits)[(Top + Y) * Image.Width + Left];
      for X := 0 to Frame.Width - 1 do
      begin
        if (Src^ <> TransIndex) then
          Dst^ := Src^;

        Inc(Src);
        Inc(Dst);
      end;
    end;
  end;

  procedure CopyLZWData(Dest: TStream);
  var
    CodeSize, BlockSize: Byte;
    InputSize: Integer;
    Buff: array[Byte] of Byte;
  begin
    InputSize := ImagingIO.GetInputSize(GetIO, Handle);
    // Copy codesize to stream
    GetIO.Read(Handle, @CodeSize, 1);
    Dest.Write(CodeSize, 1);
    repeat
      // Read and write data blocks, last is block term value of 0
      GetIO.Read(Handle, @BlockSize, 1);
      Dest.Write(BlockSize, 1);
      if BlockSize > 0 then
      begin
        GetIO.Read(Handle, @Buff[0], BlockSize);
        Dest.Write(Buff[0], BlockSize);
      end;
    until (BlockSize = 0) or (GetIO.Tell(Handle) >= InputSize);
  end;

  procedure ReadFrame;
  var
    ImageDesc: TImageDescriptor;
    I, Idx, TransIndex: Integer;
    LocalPal: TPalette32Size256;
    Frame: TImageData;
    LZWStream: TMemoryStream;
    FrameExtra: TGifExtraData;
  begin
    Idx := Length(FrameImages);
    SetLength(FrameImages, Idx + 1);
    FrameExtra := TGifExtraData.Create;

    FrameExtra.LoopCount := LoopCount;
    FillChar(LocalPal, SizeOf(LocalPal), 0);
    with GetIO do
    begin
      // Read and parse image descriptor
      Read(Handle, @ImageDesc, SizeOf(ImageDesc));
      FrameExtra.IsLocalPalette := (ImageDesc.PackedFields and GIFLocalColorTable) = GIFLocalColorTable;
      FrameExtra.Interlaced := (ImageDesc.PackedFields and GIFInterlaced) = GIFInterlaced;
      FrameExtra.LocalPaletteLength := ImageDesc.PackedFields and GIFColorTableSize;
      FrameExtra.LocalPaletteLength := 1 shl (FrameExtra.LocalPaletteLength + 1);   // Total pal length is 2^(n+1)

      bAllGlobal := bAllGlobal and not FrameExtra.IsLocalPalette;
      bAllLocal  := bAllLocal and FrameExtra.IsLocalPalette;

      if not FrameExtra.IsLocalPalette then
        FrameExtra.LocalPaletteLength := GlobalPalLength;

      FrameExtra.GlobalPaletteLength := GlobalPalLength;

      //from mozilla source
      if (ImageDesc.Width = 0) or (ImageDesc.Width > Header.ScreenWidth) then
        ImageDesc.Width := Header.ScreenWidth;
      if (ImageDesc.Height = 0) or (ImageDesc.Height > Header.ScreenHeight)  then
        ImageDesc.Height := Header.ScreenHeight;


      FrameExtra.RealFrameWidth  := ImageDesc.Width;
      FrameExtra.RealFrameHeight := ImageDesc.Height;
      FrameExtra.FrameLeft       := ImageDesc.Left;
      FrameExtra.FrameTop        := ImageDesc.Top;

      // Create new logical screen
      NewImage(ImageDesc.Width, ImageDesc.Height, ifIndex8, FrameImages[Idx]);

      // Create new image for this frame which would be later pasted onto logical screen
      InitImage(Frame);
      NewImage(ImageDesc.Width, ImageDesc.Height, ifIndex8, Frame);

      // Load local palette if there is any
      if FrameExtra.IsLocalPalette then
      for I := 0 to FrameExtra.LocalPaletteLength - 1 do
      begin
        LocalPal[I].A := 255;
        Read(Handle, @LocalPal[I].R, SizeOf(LocalPal[I].R));
        Read(Handle, @LocalPal[I].G, SizeOf(LocalPal[I].G));
        Read(Handle, @LocalPal[I].B, SizeOf(LocalPal[I].B));
      end;

      // Use local pal if present or global pal if present or create
      // default pal if neither of them is present
      if FrameExtra.IsLocalPalette then
        Move(LocalPal, FrameImages[Idx].Palette^, SizeOf(LocalPal))
      else if HasGlobalPal then
        Move(GlobalPal, FrameImages[Idx].Palette^, SizeOf(GlobalPal))
      else
        FillCustomPalette(FrameImages[Idx].Palette, GlobalPalLength, 3, 3, 2);

      // Add default disposal method for this frame
      FrameExtra.DisposalMethod := dmNoRemoval;

      //default background color for frame
     // FrameExtra.TransparentIndex := Header.BackgroundColorIndex;

      // If Grahic Control Extension is present make use of it
      if HasGraphicExt then
      begin
        FrameExtra.Transparent := ((GraphicExt.PackedFields and GIFTransparent) = GIFTransparent);
        FrameExtra.DisposalMethod := TDisposalMethod((GraphicExt.PackedFields and GIFDisposalMethod) shr 2);

        if FrameExtra.Transparent then
        begin
          if FrameExtra.IsLocalPalette then
            FrameImages[Idx].Palette[GraphicExt.TransparentColorIndex].A := 0;
          FrameExtra.TransparentIndex := GraphicExt.TransparentColorIndex;
        end;
      end
      else
        FrameExtra.Transparent := False;

      FrameExtra.BackGroundIndex := Header.BackgroundColorIndex;
      if FrameExtra.Transparent {and FrameExtra.IsLocalPalette} then
        FillRect(FrameImages[Idx], 0, 0, FrameImages[Idx].Width, FrameImages[Idx].Height,
                 @FrameExtra.TransparentIndex);

      LZWStream := TMemoryStream.Create;
      try
        // Copy LZW data to temp stream, needed for correct decompression
        try
          CopyLZWData(LZWStream);
          LZWStream.Position := 0;
          // Data decompression finally
          LZWDecompress(LZWStream, Handle, ImageDesc.Width, ImageDesc.Height, FrameExtra.Interlaced, Frame.Bits);
        except
          FreeImage(FrameImages[Idx]);
          if Idx > 0 then
            SetLength(FrameImages, Idx - 1);
          Exit;
        end;

        //Step4
        // Now copy frame to logical screen with skipping of transparent pixels (if enabled)
        if (ImageDesc.Left <= Header.ScreenWidth + 1) and (ImageDesc.Top <= Header.ScreenHeight + 1) then
        begin
          if ImageDesc.Left + ImageDesc.Width > Header.ScreenWidth then
          begin
            Header.ScreenWidth := OldGlobalWidth + (ImageDesc.Left + ImageDesc.Width) - Header.ScreenWidth;
            for I := 0 to Idx - 1 do
            with TGifExtraData(FrameImages[I].Extra) do
            begin
              GlobalWidth  := Header.ScreenWidth;
              GlobalHeight := Header.ScreenHeight;
            end;
          end;
          if ImageDesc.Top + ImageDesc.Height > Header.ScreenHeight then
          begin
            Header.ScreenHeight := OldGlobalHeight + (ImageDesc.Top + ImageDesc.Height) - Header.ScreenHeight;
            for I := 0 to Idx - 1 do
            with TGifExtraData(FrameImages[I].Extra) do
            begin
              GlobalWidth  := Header.ScreenWidth;
              GlobalHeight := Header.ScreenHeight;
            end;
          end;

          with FrameExtra do
          begin
            GlobalWidth  := Header.ScreenWidth;
            GlobalHeight := Header.ScreenHeight;
          end;

          TransIndex := Iff(FrameExtra.Transparent, GraphicExt.TransparentColorIndex, MaxInt);
          if TestImage(Frame) then
          begin
            CopyFrameTransparent(FrameImages[Idx], Frame, 0, 0, TransIndex);
            FrameImages[Idx].Extra := FrameExtra;
          end;
        end
        else
        begin
          FreeImage(Frame);
          FreeImage(FrameImages[Idx]);
          FreeAndNil(FrameExtra);
          if Idx > 0 then
            SetLength(FrameImages, Idx - 1);

          Exit;
        end;
        
        if HasGraphicExt and not OnlyFirstLevel then
        try
          FrameImages[Idx].Extra.Delay := GraphicExt.DelayTime;
        except
        end;
      finally
        LZWStream.Free;
        FreeImage(Frame);
      end;
    end;
  end;

  function Extra(const Index: Integer): TGifExtraData;
  begin
    Result := TGifExtraData(FrameImages[Index].Extra);
  end;

begin
  FreeImagesInArray(Images);
  SetLength(Images, 0);
  FillChar(GlobalPal, SizeOf(GlobalPal), 0);
  with GetIO do
  begin
    // Read GIF header
    Read(Handle, @Header, SizeOf(Header));
    HasGlobalPal := Header.PackedFields and GIFGlobalColorTable = GIFGlobalColorTable; // Bit 7
    GlobalPalLength := Header.PackedFields and GIFColorTableSize; // Bits 0-2
    GlobalPalLength := 1 shl (GlobalPalLength + 1);   // Total pal length is 2^(n+1)

    // Read global palette from file if present
    if HasGlobalPal then
    begin
      for I := 0 to GlobalPalLength - 1 do
      begin
        GlobalPal[I].A := 255;
        Read(Handle, @GlobalPal[I].R, SizeOf(GlobalPal[I].R));
        Read(Handle, @GlobalPal[I].G, SizeOf(GlobalPal[I].G));
        Read(Handle, @GlobalPal[I].B, SizeOf(GlobalPal[I].B));
      end;
    end;

    bAllGlobal       := True;
    bAllLocal        := True;

    // Read ID of the first block
    BlockID := ReadBlockID;

    LoopCount := 0;
    AppRead   := False;

    // Now read all data blocks in the file until file trailer is reached
    while BlockID <> GIFTrailer do
    try
      OldGlobalWidth  := Header.ScreenWidth;
      OldGlobalHeight := Header.ScreenHeight;
      while not (BlockID in [GIFTrailer, GIFExtensionIntroducer, GIFImageDescriptor]) do
        BlockID := ReadBlockID;

      // Read supported and skip unsupported extensions
      ReadExtensions;

      // If image frame is found read it
      if BlockID = GIFImageDescriptor then
        ReadFrame;
      // Read next block's ID
      BlockID := ReadBlockID;

      // If block ID is unknown set it to end-of-GIF marker
      if not (BlockID in [GIFExtensionIntroducer, GIFTrailer, GIFImageDescriptor]) then
        BlockID := GIFTrailer;
    except
      BlockID := GIFTrailer;
    end;

    //now copy frames to destination
    SetLength(Images, Length(FrameImages));
    for I := 0 to Length(FrameImages) - 1 do
    begin
      if (Extra(I) <> nil) then
      with TGifExtraData(FrameImages[I].Extra) do
      begin
        if not AppRead then
          LoopCount := 1;
        AllLocal  := bAllLocal;
        AllGlobal := bAllGlobal;
      end;
      CloneImage(FrameImages[I], Images[I]);
    end;

    FreeImagesInArray(FrameImages);
    Result := Length(Images) <> 0;
  end;//with GetIO do
end;


type
  TLoopExtension = packed record
    Inroducer: Byte;
    ExtLabel: Byte;
    BlockSize: Byte;
    LoopIdent: TGIFApplicationRec;
    AppData: packed record
      BlockSize : Byte; //must be SizeOf(LoopCount) + 1
      BlockType : Byte; //must be GIFAppLoopExtension or $07
      LoopCount : Word;
    end;
    Terminator: Byte;
  end;

function TGIFFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: Integer): Boolean;
var
  Header: TGIFHeader;
  ImageDesc: TImageDescriptor;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  I, J: Integer;
  GraphicExt: TGraphicControlExtension;
  GifExtraData: TGifExtraData;
  LoopExt: TLoopExtension;

const
  LoopIdent: TGIFApplicationRec = (Identifier: 'NETSCAPE'; Authentication: '2.0');

  procedure FindMaxDimensions(var MaxWidth, MaxHeight: Word);
  var
    I: Integer;
  begin
    MaxWidth := Images[FFirstIdx].Width;
    MaxHeight := Images[FFirstIdx].Height;

    for I := FFirstIdx + 1 to FLastIdx do
    begin
      MaxWidth := Iff(Images[I].Width > MaxWidth, Images[I].Width, MaxWidth);
      MaxHeight := Iff(Images[I].Height > MaxWidth, Images[I].Height, MaxHeight);
    end;
  end;

  function FrameExtra(const Index: Integer): TGifExtraData;
  begin
    Result := nil;
    if Images[Index].Extra <> nil then
    begin
      Result := TGifExtraData.Create;
      Result.AssignFrom(Images[Index].Extra);
      if Result.Palette = nil then
        Result.Palette := Images[Index].Palette;
    end;
  end;

  function log2(const Mean: Word): Word;
  var
    iPF: Integer;
  begin
    iPF := (Mean shr 8) or (Mean shr 7) or (Mean shr 6) or (Mean shr 5) or
           (Mean shr 4) or (Mean shr 3) or (Mean shr 2) or (Mean shr 1) or
           (Mean shr 0);

    iPF := iPF and $5555 + ((iPF shr 1) and $5555);
    iPF := iPF and $3333 + ((iPF shr 2) and $3333);
    iPF := iPF and $0f0f + ((iPF shr 4) and $0f0f);
    iPF := iPF and $00ff + ((iPF shr 8) and $00ff);

    Result := Word(iPF) - 2;
  end;

begin
  // Fill header with data, select size of largest image in array as
  // logical screen size
  FillChar(Header, Sizeof(Header), 0);
  Header.Signature := GIFSignature;
  Header.Version := GIFVersions[gv89];


  GifExtraData := FrameExtra(0);
  if GifExtraData = nil then
  begin
    GifExtraData := TGifExtraData.Create;
    FindMaxDimensions(GifExtraData.FGlobWdth, GifExtraData.FGlobWdth);
    with GifExtraData do
    begin
      Delay := 10;
      AllLocal := True;
      if Palette = nil then
        Palette := Images[0].Palette;
      LoopCount := 0;
    end;
  end;

  Header.ScreenWidth  := GifExtraData.GlobalWidth;
  Header.ScreenHeight := GifExtraData.GlobalHeight;
  Header.BackgroundColorIndex := GifExtraData.BackGroundIndex;

  Header.PackedFields := GIFColorResolution; // Color resolution is 256
  if not GifExtraData.AllLocal then
    Header.PackedFields := log2(GifExtraData.GlobalPaletteLength) or GIFGlobalColorTable;

  GetIO.Write(Handle, @Header, SizeOf(Header));

  if not GifExtraData.AllLocal then
  with GifExtraData, GetIO do
  begin
    for J := 0 to GifExtraData.GlobalPaletteLength - 1 do
      begin
        Write(Handle, @Palette[J].R, SizeOf(Palette[J].R));
        Write(Handle, @Palette[J].G, SizeOf(Palette[J].G));
        Write(Handle, @Palette[J].B, SizeOf(Palette[J].B));
      end;
  end;

  // Prepare default GC extension with delay
  FillChar(GraphicExt, Sizeof(GraphicExt), 0);
  GraphicExt.BlockSize := 4;

  //write animation loop count
  //by default 0, means continues loop
  //if = 1, then do not write it, the decoder should set it correctly 
  if GifExtraData.LoopCount <> 1 then
  begin
    LoopExt.Inroducer := GIFExtensionIntroducer;
    LoopExt.ExtLabel  := GIFApplicationExtension;
    LoopExt.BlockSize := SizeOf(LoopIdent);
    LoopExt.LoopIdent := LoopIdent;
    LoopExt.AppData.BlockSize := 1 + SizeOf(Word);
    LoopExt.AppData.BlockType := GIFAppLoopExtension;
    LoopExt.AppData.LoopCount := GifExtraData.LoopCount;
    LoopExt.Terminator        := 0;
    GetIO.Write(Handle, @LoopExt, SizeOf(LoopExt));
  end;
  
  GifExtraData.Free;

  for I := FFirstIdx to FLastIdx do
  begin
    if MakeCompatible(Images[I], ImageToSave, MustBeFreed) then
    with GetIO, ImageToSave do
    try
      GifExtraData := FrameExtra(I);
      if GifExtraData = nil then
      begin
        GifExtraData := TGifExtraData.Create;
        with GifExtraData do
        begin
          Delay := 10;
          AllLocal := True;
          IsLocalPalette := True;
          if Palette = nil then
            Palette := Images[I].Palette;
          RealFrameWidth  := Images[I].Width;
          RealFrameHeight := Images[I].Height;
          GifExtraData.FrameLeft := 0;
          GifExtraData.FrameTop  := 0;
        end;
      end;

      //do not write GraphicExt if Delay=0, not transparent and DisposalMethod = dmNoRemoval
      with GifExtraData do
      if not ((Delay = 0) and (DisposalMethod = dmNoRemoval) and not Transparent) then
      begin
        GraphicExt.DelayTime := GifExtraData.Delay;
        GraphicExt.PackedFields := 0;
        if GifExtraData.Transparent then
        begin
          GraphicExt.PackedFields := GraphicExt.PackedFields or GIFTransparent;
          GraphicExt.TransparentColorIndex := GifExtraData.TransparentIndex;
        end;

        GraphicExt.PackedFields := GraphicExt.PackedFields and not (GIFDisposalMethod)
                                    or ((Ord(GifExtraData.DisposalMethod) shl 2) and GIFDisposalMethod);
        // Write Graphic Control Extension with default delay
        Write(Handle, @GIFExtensionIntroducer, SizeOf(GIFExtensionIntroducer));
        Write(Handle, @GIFGraphicControlExtension, SizeOf(GIFGraphicControlExtension));
        Write(Handle, @GraphicExt, SizeOf(GraphicExt));
      end;

      // Write frame marker and fill and write image descriptor for this frame
      Write(Handle, @GIFImageDescriptor, SizeOf(GIFImageDescriptor));
      FillChar(ImageDesc, Sizeof(ImageDesc), 0);
      ImageDesc.Width  := Iff(GifExtraData.RealFrameWidth <> 0, GifExtraData.RealFrameWidth, Images[I].Width);
      ImageDesc.Height := Iff(GifExtraData.RealFrameHeight <> 0, GifExtraData.RealFrameHeight, Images[I].Height);
      ImageDesc.Left   := GifExtraData.FrameLeft;
      ImageDesc.Top    := GifExtraData.FrameTop;


      //log2 magic trick
      //as far the LocalPaletteLength = 2 ^ (ImageDesc.PackedFields + 1)
      //then ImageDesc.PackedFields = log2(LocalPaletteLength) - 1;
      //thanks to nullbie for algo
      with ImageDesc, GifExtraData do
      if IsLocalPalette then
        ImageDesc.PackedFields := log2(LocalPaletteLength) or GIFLocalColorTable;

      if GifExtraData.Interlaced then
        ImageDesc.PackedFields := ImageDesc.PackedFields or GIFInterlaced;

      Write(Handle, @ImageDesc, SizeOf(ImageDesc));

      if GifExtraData.IsLocalPalette then
      // Write local color table for each frame
      for J := 0 to GifExtraData.LocalPaletteLength - 1 do
      begin
        Write(Handle, @Palette[J].R, SizeOf(Palette[J].R));
        Write(Handle, @Palette[J].G, SizeOf(Palette[J].G));
        Write(Handle, @Palette[J].B, SizeOf(Palette[J].B));
      end;

      // Finally compress image data
      LZWCompress(GetIO, Handle, Width, Height, 8, GifExtraData.Interlaced, Bits);

    finally
      if MustBeFreed then
        FreeImage(ImageToSave);
      GifExtraData.Free;
    end;
  end;

  GetIO.Write(Handle, @GIFTrailer, SizeOf(GIFTrailer));
  Result := True;
end;

procedure TGIFFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  ConvertImage(Image, ifIndex8);
end;

function TGIFFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Header: TGIFHeader;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Header, SizeOf(Header));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount >= SizeOf(Header)) and
      (Header.Signature = GIFSignature) and
      ((Header.Version = GIFVersions[gv87]) or (Header.Version = GIFVersions[gv89]));
  end;
end;

{ TGifExtraData }

procedure TGifExtraData.AssignFrom(Source: TExtraData);
begin
  inherited;
  if Source is TGifExtraData then
  begin
    DisposalMethod := TGifExtraData(Source).DisposalMethod;
    LocalPaletteLength  := TGifExtraData(Source).LocalPaletteLength;
    IsLocalPalette := TGifExtraData(Source).IsLocalPalette;
    Transparent      := TGifExtraData(Source).Transparent;
    TransparentIndex := TGifExtraData(Source).TransparentIndex;
    RealFrameWidth   := TGifExtraData(Source).RealFrameWidth;
    RealFrameHeight  := TGifExtraData(Source).RealFrameHeight;
    FrameLeft        := TGifExtraData(Source).FrameLeft;
    FrameTop         := TGifExtraData(Source).FrameTop;
    GlobalHeight     := TGifExtraData(Source).GlobalHeight;
    GlobalWidth      := TGifExtraData(Source).GlobalWidth;
    AllLocal         := TGifExtraData(Source).AllLocal;
    AllGlobal        := TGifExtraData(Source).AllGlobal;
    LoopCount        := TGifExtraData(Source).LoopCount;
    Interlaced       := TGifExtraData(Source).Interlaced;
    GlobalPaletteLength := TGifExtraData(Source).GlobalPaletteLength;
  end;
end;

procedure TGifExtraData.ImageResized(var NewWidth, NewHeight: Integer);
var
  MaxWidth, MinWidth, MaxHeight, MinHeight: Integer;
begin
  inherited;
  if NewWidth = 0 then
  begin
    GlobalWidth    := 0;
    RealFrameWidth := 0;
    FrameLeft      := 0;
  end;
  if NewWidth = 0 then
  begin
    GlobalWidth    := 0;
    RealFrameWidth := 0;
    FrameLeft      := 0;
  end;

  MaxWidth := Max(NewWidth, GlobalWidth);
  MinWidth := Min(NewWidth, GlobalWidth);
  MaxHeight := Max(NewHeight, GlobalHeight);
  MinHeight := Min(NewHeight, GlobalHeight);

  //scale frame real position and size
  if (NewWidth > 0) and (GlobalWidth > 0) then
  begin
    if FrameLeft > 0 then
    begin
      if GlobalWidth < NewWidth then
        FrameLeft := Round(MaxWidth * FrameLeft / MinWidth)
      else
        FrameLeft := Round(MinWidth * FrameLeft / MaxWidth);
    end;
    if GlobalWidth < NewWidth then
      RealFrameWidth := Max(Round(RealFrameWidth * MaxWidth / MinWidth), 1)
    else
      RealFrameWidth := Max(Round(RealFrameWidth * MinWidth / MaxWidth), 1);
  end;

  if (NewHeight > 0) and (GlobalHeight > 0) then
  begin
    if FrameTop > 0 then
    begin
      if GlobalHeight < NewHeight then
        FrameTop := Round(MaxHeight * FrameTop / MinHeight)
      else
        FrameTop := Round(MinHeight * FrameTop / MaxHeight);
    end;
    if GlobalHeight < NewHeight then
      RealFrameHeight := Max(Round(RealFrameHeight * MaxHeight / MinHeight), 1)
    else
      RealFrameHeight := Max(Round(RealFrameHeight * MinHeight / MaxHeight), 1)
  end;

  GlobalWidth := NewWidth;
  GlobalHeight := NewHeight;
  NewWidth := RealFrameWidth;
  NewHeight := RealFrameHeight;
end;

initialization
  RegisterImageFileFormat(TGIFFileFormat);

{
  File Notes:
  
  -- TODOS ----------------------------------------------------
    - nothing now

   -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Fixed bug - loading of GIF with NETSCAPE app extensions
      failed with Delphi 2009.
   
  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - ExtraGIF fork.

  -- 0.25.0 Changes/Bug Fixes ---------------------------------
    - Fixed loading of some rare GIFs, problems with LZW
      decompression.

  -- 0.24.3 Changes/Bug Fixes ---------------------------------
    - Better solution to transparency for some GIFs. Background not
      transparent by default.

  -- 0.24.1 Changes/Bug Fixes ---------------------------------
    - Made backround color transparent by default (alpha = 0).

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Fixed other loading bugs (local pal size, transparency).
    - Added GIF saving.
    - Fixed bug when loading multiframe GIFs and implemented few animation
      features (disposal methods, ...). 
    - Loading of GIFs working.
    - Unit created with initial stuff!
}

end.

