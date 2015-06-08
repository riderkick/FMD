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

{ This unit contains image format loader for textures and images
  from Redguard and BattleSpire.}
unit ElderImageryBsi;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ElderImagery, ImagingUtility;

type
  { Class for loading of BSI format textures and images found
    in Redguard and BattleSpire (maybe in other games too, Skynet?). This format
    uses chunk structure similar to PNG (HDR/DAT/END). Redguard stores
    multiple images in one file (usually related like textures for various
    parts of single 3d object). Image data is stored as 8bit. Each image
    can have its own embedded palette or it can use external default palette.
    BattleSpire BSI use *.bsi file extension whilst Redguard uses
    texbsi.* mask with number extension (just like Daggerfall).
    Only loading is supported for this format.
    BattleSpire images also contain some sort of 8bit->16bit color mapping data
    which I've not yet figured out (only blue channel known).}
  TBSIFileFormat = class(TElderFileFormat)
  private
    function IsMultiBSI(Handle: TImagingHandle): Boolean;
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

implementation

const
  SBSIFormatName = 'Bethesda Image';
  SBSIMasks      = '*.bsi,texbsi.*';

resourcestring
  SErrorLoadingChunk = 'Error when reading %s chunk data.';

type
  { BSI chunk header.}
  TChunk = packed record
    ChunkID: TChar4;
    DataSize: LongWord; // In Big Endian!
  end;

  { Additional header of BSI textures.}
  TTextureBSIHeader = packed record
    Name: array[0..8] of AnsiChar;
    ImageSize: LongInt;
  end;

  { Main image info header located in BHDR chunk's data.}
  TBHDRChunk = packed record
    OffsetX: Word;
    OffsetY: Word;
    Width: SmallInt;
    Height: SmallInt;
    Unk1, Unk2: Byte;
    Unk3, Unk4: Word;
    Frames: Word;
    Unk6, Unk7, Unk8: Word;
    Unk9, Unk10: Byte;
    Unk11: Word;
  end;

const
  IFHDSignature: TChar4 = 'IFHD';
  BSIFSignature: TChar4 = 'BSIF';
  BHDRSignature: TChar4 = 'BHDR';
  CMAPSignature: TChar4 = 'CMAP';
  HICLSignature: TChar4 = 'HICL';
  HTBLSignature: TChar4 = 'HTBL';
  DATASignature: TChar4 = 'DATA';
  ENDSignature:  TChar4 = 'END ';


{ TBSIFileFormat class implementation }

procedure TBSIFileFormat.Define;
begin
  inherited;
  FName := SBSIFormatName;
  FFeatures := [ffLoad, ffMultiImage];

  AddMasks(SBSIMasks);
  SetPalette(RedguardPalette);
end;

function TBSIFileFormat.IsMultiBSI(Handle: TImagingHandle): Boolean;
var
  ReadCount, StartPos: LongInt;
  Sig: TChar4;
begin
  Result := False;
  if Handle <> nil then
  with GetIO do
  begin
    StartPos := Tell(Handle);
    // Redguard textures have 13 byte tex header and then IFHD or BSIF
    Seek(Handle, SizeOf(TTextureBSIHeader), smFromCurrent);
    ReadCount := Read(Handle, @Sig, SizeOf(Sig));
    Seek(Handle, StartPos, smFromBeginning);
    Result := Result or ((ReadCount = SizeOf(Sig)) and
      ((Sig = IFHDSignature) or (Sig = BSIFSignature)));
  end;
end;

function TBSIFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Chunk: TChunk;
  ChunkData: Pointer;
  DATASize: LongInt;
  BHDR: TBHDRChunk;
  PalLoaded: TPalette24Size256;
  HICL: PByteArray;
  HTBL: PWordArray;
  IsMulti: Boolean;
  TextureHdr: TTextureBSIHeader;
  PaletteFound: Boolean;

  procedure ReadChunk;
  begin
    GetIO.Read(Handle, @Chunk, SizeOf(Chunk));
    Chunk.DataSize := SwapEndianLongWord(Chunk.DataSize);
  end;

  procedure ReadChunkData;
  var
    ReadBytes: LongWord;
  begin
    FreeMemNil(ChunkData);
    GetMem(ChunkData, Chunk.DataSize);
    ReadBytes := GetIO.Read(Handle, ChunkData, Chunk.DataSize);
    if ReadBytes <> Chunk.DataSize then
      RaiseImaging(SErrorLoadingChunk, [Chunk.ChunkID]);
  end;

  procedure SkipChunkData;
  begin
    GetIO.Seek(Handle, Chunk.DataSize, smFromCurrent);
  end;

  procedure GetBHDR;
  begin
    ReadChunkData;
    BHDR := TBHDRChunk(ChunkData^);
  end;

  procedure GetHICL;
  begin
    ReadChunkData;
    GetMem(HICL, Chunk.DataSize);
    Move(ChunkData^, HICL[0], Chunk.DataSize);
  end;

  procedure GetHTBL;
  begin
    ReadChunkData;
    GetMem(HTBL, Chunk.DataSize);
    Move(ChunkData^, HTBL[0], Chunk.DataSize);
  end;

  procedure GetCMAP;
  begin
    ReadChunkData;
    Move(ChunkData^, PalLoaded, Chunk.DataSize);
    PaletteFound := True;
  end;

  procedure GetDATA;
  begin
    ReadChunkData;
    DATASize := Chunk.DataSize;
  end;

  function AddImage(Width, Height: LongInt): LongInt;
  begin
    Result := Length(Images);
    SetLength(Images, Length(Images) + 1);
    NewImage(Width, Height, ifIndex8, Images[Result]);
    if not PaletteFound then
      Move(FARGBPalette[0], Images[Result].Palette[0], Length(FPalette) * SizeOf(TColor32Rec))
    else
      ConvertPalette(PalLoaded, Images[Result].Palette);
  end;

  function AddImageHiColor(Width, Height: LongInt): LongInt;
  begin
    Result := Length(Images);
    SetLength(Images, Length(Images) + 1);
    NewImage(Width, Height, ifA8R8G8B8, Images[Result]);
  end;

  procedure Reconstruct;
  var
    Index, I, J, K: LongInt;
    RowOffsets: PLongIntArray;
    Idx: Byte;
    W: Word;
  begin
    if HICL = nil then
    begin
      if BHDR.Frames = 1 then
      begin
        // Load simple image
        Index := AddImage(BHDR.Width, BHDR.Height);
        Move(ChunkData^, Images[Index].Bits^, Images[Index].Size);
      end
      else
      begin
        // Load animated image:
        // At the beggining of the chunk data there is BHDR.Height * BHDR.Frames
        // 32bit offsets. Each BHDR.Height offsets point to rows of the current frame
        RowOffsets := PLongIntArray(ChunkData);

        for I := 0 to BHDR.Frames - 1 do
        begin
          Index := AddImage(BHDR.Width, BHDR.Height);
          with Images[Index] do
          for J := 0 to BHDR.Height - 1 do
            Move(PByteArray(ChunkData)[RowOffsets[I * BHDR.Height + J]],
              PByteArray(Bits)[J * Width], Width);
        end;
      end;
    end
    else
    begin
      if BHDR.Frames = 1 then
      begin
        // Experimental BattleSpire 16bit image support!
        Index := AddImageHiColor(BHDR.Width, BHDR.Height);
        with Images[Index] do
        for I := 0 to DATASize - 1 do
        with PColor32RecArray(Bits)[I] do
        begin
          // It looks like "HICL[PByteArray(ChunkData)[I]] and 63" gives
          // value of 6bit Blue channel, not other channels are sure yet.
          // So now it looks grayscalish.
          // You can also get interesting results using HTBL as look up table
          // 8->16bit. There are 16 tables for shading (table 0 - darkest colors,
          // table 15 - lightest) each with 256 16bit Words. But their data format
          // is weird (555 is closest). There are some pixels that look
          // as they should (proper color) but some does not.
          // PWordArray(Bits)[I] := HTBL[256 * 15 + PByteArray(ChunkData)[I]]
          Idx := PByteArray(ChunkData)[I];
          A := Iff(Idx <> 0, 255, 0);
          R := MulDiv(HICL[Idx] and 63, 255, 63);
          G := MulDiv(HICL[Idx] and 63, 255, 63);
          B := MulDiv(HICL[Idx] and 63, 255, 63);
        end;
      end
      else
      begin
        // Load animated BattleSpire image, uses offset list just like Redguard
        // animated textures (but high word must be zeroed first to get valid offset)
        RowOffsets := PLongIntArray(ChunkData);

        for I := 0 to BHDR.Frames - 1 do
        begin
          Index := AddImageHiColor(BHDR.Width, BHDR.Height);
          with Images[Index] do
          for J := 0 to BHDR.Height - 1 do
            for K := 0 to BHDR.Width - 1 do
            with PColor32RecArray(Bits)[J * BHDR.Width + K] do
            begin
              Idx := PByteArray(ChunkData)[RowOffsets[I * BHDR.Height + J] and $FFFF + K];
              W := HTBL[256 * 15 + Idx];
              A := Iff(Idx <> 0, 255, 0);
              R := MulDiv(W shr 10 and 31, 255, 31);
              G := MulDiv(W shr 5 and 31, 255, 31);
              B := MulDiv(W and 31, 255, 31);
          {    A := Iff(Idx <> 0, 255, 0);
          R := MulDiv(HICL[Idx] and 63, 255, 63);
          G := MulDiv(HICL[Idx] and 63, 255, 63);
          B := MulDiv(HICL[Idx] and 63, 255, 63);}
            end;
        end;
      end;
    end;
  end;

  procedure ReadTextureHeader;
  begin
    FillChar(TextureHdr, SizeOf(TextureHdr), 0);
    if IsMulti then
      GetIO.Read(Handle, @TextureHdr, SizeOf(TextureHdr))
    else if Length(Images) = 0 then
      // Ensure that while loop that reads chunks is executed for
      // single-image files
      TextureHdr.ImageSize := 1;
  end;

begin
  ChunkData := nil;
  HICL := nil;
  HTBL := nil;
  SetLength(Images, 0);
  IsMulti := IsMultiBSI(Handle);
  with GetIO do
  begin
    // Redguard textures can contain more than one image. Try to read texture
    // header and if ImageSize is >0 there is another image.
    ReadTextureHeader;
    while TextureHdr.ImageSize > 0 do
    try
      PaletteFound := False;
      ReadChunk;
      SkipChunkData;
      // Read data chunks. If they are recognized their data is stored for
      // later image reconstruction
      repeat
        ReadChunk;
        if Chunk.ChunkID = BHDRSignature then
          GetBHDR
        else if Chunk.ChunkID = HICLSignature then
          GetHICL
        else if Chunk.ChunkID = HTBLSignature then
          GetHTBL
        else if Chunk.ChunkID = CMAPSignature then
          GetCMAP
        else if Chunk.ChunkID = DATASignature then
          GetDATA
        else
          SkipChunkData;
      until Eof(Handle) or (Chunk.ChunkID = ENDSignature);
      // Recontruct current image according to data read from chunks
      Reconstruct;
      // Read header for next image
      ReadTextureHeader;
    finally
      FreeMemNil(ChunkData);
      FreeMemNil(HICL);
      FreeMemNil(HTBL);
    end;
    Result := True;
  end;
end;

function TBSIFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  ReadCount: LongInt;
  Sig: TChar4;
begin
  // First check if have multi-image BSI file (Redguard textures)
  Result := IsMultiBSI(Handle);
  if not Result and (Handle <> nil) then
  with GetIO do
  begin
    // Check standard Bettlespire images with IFHD chunk at
    // the beginning of the file
    ReadCount := Read(Handle, @Sig, SizeOf(Sig));
    Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount = SizeOf(Sig)) and (Sig = IFHDSignature);
  end;
end;


{
  Changes/Bug Fixes:

  -- TODOS ----------------------------------------------------
    - crack the BattleSpire format completely

  -- 0.21 -----------------------------------------------------
    - Blue channel of BattleSpire images cracked but others arer still unknown.
    - Added support for animated BattleSpire images.
    - Added support for animated Redguard textures.
    - Added support for Redguard textures (Battlespire images still don't figured out).
    - Updated to current Imaging version.

  -- 0.13 -----------------------------------------------------
    - TBSIFileFormat class added

}

end.
