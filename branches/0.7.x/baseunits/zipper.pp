{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit zipper;

Interface

Uses
  {$IFDEF UNIX}
   BaseUnix,
  {$ENDIF}
   SysUtils,Classes,zstream,FileUtil,lazutf8classes;


Const
  { Signatures }
  END_OF_CENTRAL_DIR_SIGNATURE  = $06054B50;
  LOCAL_FILE_HEADER_SIGNATURE   = $04034B50;
  CENTRAL_FILE_HEADER_SIGNATURE = $02014B50;

Type
   Local_File_Header_Type = Packed Record
     Signature              :  LongInt; //4 bytes
     Extract_Version_Reqd   :  Word;
     Bit_Flag               :  Word;
     Compress_Method        :  Word;
     Last_Mod_Time          :  Word;
     Last_Mod_Date          :  Word;
     Crc32                  :  LongWord;
     Compressed_Size        :  LongWord;
     Uncompressed_Size      :  LongWord;
     Filename_Length        :  Word;
     Extra_Field_Length     :  Word;
   end;

  { Define the Central Directory record types }

  Central_File_Header_Type = Packed Record
    Signature            :  LongInt; //4 bytes
    MadeBy_Version       :  Word;
    Extract_Version_Reqd :  Word;
    Bit_Flag             :  Word;
    Compress_Method      :  Word;
    Last_Mod_Time        :  Word;
    Last_Mod_Date        :  Word;
    Crc32                :  LongWord;
    Compressed_Size      :  LongWord;
    Uncompressed_Size    :  LongWord;
    Filename_Length      :  Word;
    Extra_Field_Length   :  Word;
    File_Comment_Length  :  Word;
    Starting_Disk_Num    :  Word;
    Internal_Attributes  :  Word;
    External_Attributes  :  LongWord;
    Local_Header_Offset  :  LongWord;
  End;

  End_of_Central_Dir_Type =  Packed Record
    Signature               :  LongInt; //4 bytes
    Disk_Number             :  Word;
    Central_Dir_Start_Disk  :  Word;
    Entries_This_Disk       :  Word;
    Total_Entries           :  Word;
    Central_Dir_Size        :  LongWord;
    Start_Disk_Offset       :  LongWord;
    ZipFile_Comment_Length  :  Word;
  end;

Const
  Crc_32_Tab : Array[0..255] of LongWord = (
    $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535, $9e6495a3,
    $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,
    $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
    $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,
    $3b6e20c8, $4c69105e, $d56041e4, $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
    $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,
    $26d930ac, $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
    $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,
    $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
    $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
    $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
    $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,
    $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
    $4369e96a, $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
    $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f,
    $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,
    $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615, $73dc1683,
    $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8, $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,
    $f00f9344, $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7,
    $fed41b76, $89d32be0, $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
    $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
    $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef, $4669be79,
    $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703, $220216b9, $5505262f,
    $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
    $9b64c2b0, $ec63f226, $756aa39c, $026d930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
    $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,
    $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
    $88085ae6, $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
    $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d, $3e6e77db,
    $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
    $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605, $cdd70693, $54de5729, $23d967bf,
    $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
  );

Type

  TProgressEvent = Procedure(Sender : TObject; Const Pct : Double) of object;
  TOnEndOfFileEvent = Procedure(Sender : TObject; Const Ratio : Double) of object;
  TOnStartFileEvent = Procedure(Sender : TObject; Const AFileName : String) of object;

Type

  { TCompressor }
  TCompressor = Class(TObject)
  Protected
    FInFile     : TStream;        { I/O file variables                         }
    FOutFile    : TStream;
    FCrc32Val   : LongWord;       { CRC calculation variable                   }
    FBufferSize : LongWord;
    FOnPercent  : Integer;
    FOnProgress : TProgressEvent;
    Procedure UpdC32(Octet: Byte);
  Public
    Constructor Create(AInFile, AOutFile : TStream; ABufSize : LongWord); virtual;
    Procedure Compress; Virtual; Abstract;
    Class Function ZipID : Word; virtual; Abstract;
    Class Function ZipVersionReqd: Word; virtual; Abstract;
    Function ZipBitFlag: Word; virtual; Abstract;
    Property BufferSize : LongWord read FBufferSize;
    Property OnPercent : Integer Read FOnPercent Write FOnPercent;
    Property OnProgress : TProgressEvent Read FOnProgress Write FOnProgress;
    Property Crc32Val : LongWord Read FCrc32Val Write FCrc32Val;
  end;

  { TDeCompressor }
  TDeCompressor = Class(TObject)
  Protected
    FInFile     : TStream;        { I/O file variables                         }
    FOutFile    : TStream;
    FCrc32Val   : LongWord;       { CRC calculation variable                   }
    FBufferSize : LongWord;
    FOnPercent  : Integer;
    FOnProgress : TProgressEvent;
    Procedure UpdC32(Octet: Byte);
  Public
    Constructor Create(AInFile, AOutFile : TStream; ABufSize : LongWord); virtual;
    Procedure DeCompress; Virtual; Abstract;
    Class Function ZipID : Word; virtual; Abstract;
    Property BufferSize : LongWord read FBufferSize;
    Property OnPercent : Integer Read FOnPercent Write FOnPercent;
    Property OnProgress : TProgressEvent Read FOnProgress Write FOnProgress;
    Property Crc32Val : LongWord Read FCrc32Val Write FCrc32Val;
  end;

  { TShrinker }

Const
   TABLESIZE   =   8191;
   FIRSTENTRY  =    257;

Type
  CodeRec =  Packed Record
    Child   : Smallint;
    Sibling : Smallint;
    Suffix  : Byte;
  end;
  CodeArray   =  Array[0..TABLESIZE] of CodeRec;
  TablePtr    =  ^CodeArray;

  FreeListPtr    =  ^FreeListArray;
  FreeListArray  =  Array[FIRSTENTRY..TABLESIZE] of Word;

  BufPtr      =  PByte;

  TShrinker = Class(TCompressor)
  Private
    FBufSize    : LongWord;
    MaxInBufIdx :  LongWord;      { Count of valid chars in input buffer       }
    InputEof    :  Boolean;       { End of file indicator                      }
    CodeTable   :  TablePtr;      { Points to code table for LZW compression   }
    FreeList    :  FreeListPtr;   { Table of free code table entries           }
    NextFree    :  Word;          { Index into free list table                 }

    ClearList   :  Array[0..1023] of Byte;  { Bit mapped structure used in     }
                                            {    during adaptive resets        }
    CodeSize    :  Byte;     { Size of codes (in bits) currently being written }
    MaxCode     :  Word;   { Largest code that can be written in CodeSize bits }
    InBufIdx,                     { Points to next char in buffer to be read   }
    OutBufIdx   :  LongWord;      { Points to next free space in output buffer }
    InBuf,                        { I/O buffers                                }
    OutBuf      :  BufPtr;
    FirstCh     :  Boolean;  { Flag indicating the START of a shrink operation }
    TableFull   :  Boolean;  { Flag indicating a full symbol table             }
    SaveByte    :  Byte;     { Output code buffer                              }
    BitsUsed    :  Byte;     { Index into output code buffer                   }
    BytesIn     :  LongWord;  { Count of input file bytes processed             }
    BytesOut    :  LongWord;  { Count of output bytes                           }
    FOnBytes    :  LongWord;
    Procedure FillInputBuffer;
    Procedure WriteOutputBuffer;
    Procedure FlushOutput;
    Procedure PutChar(B : Byte);
    procedure PutCode(Code : Smallint);
    Procedure InitializeCodeTable;
    Procedure Prune(Parent : Word);
    Procedure Clear_Table;
    Procedure Table_Add(Prefix : Word; Suffix : Byte);
    function  Table_Lookup(TargetPrefix : Smallint;
                           TargetSuffix : Byte;
                           Out FoundAt  : Smallint) : Boolean;
    Procedure Shrink(Suffix : Smallint);
    Procedure ProcessLine(Const Source : String);
    Procedure DoOnProgress(Const Pct : Double); Virtual;
  Public
    Constructor Create(AInFile, AOutFile : TStream; ABufSize : LongWord); override;
    Destructor Destroy; override;
    Procedure Compress; override;
    Class Function ZipID : Word; override;
    Class Function ZipVersionReqd : Word; override;
    Function ZipBitFlag : Word; override;
  end;

  { TDeflater }

  TDeflater = Class(TCompressor)
  private
    FCompressionLevel: TCompressionlevel;
  Public
    Constructor Create(AInFile, AOutFile : TStream; ABufSize : LongWord);override;
    Procedure Compress; override;
    Class Function ZipID : Word; override;
    Class Function ZipVersionReqd : Word; override;
    Function ZipBitFlag : Word; override;
    Property CompressionLevel : TCompressionlevel Read FCompressionLevel Write FCompressionLevel;
  end;

  { TInflater }

  TInflater = Class(TDeCompressor)
  Public
    Constructor Create(AInFile, AOutFile : TStream; ABufSize : LongWord);override;
    Procedure DeCompress; override;
    Class Function ZipID : Word; override;
  end;

  { TZipFileEntry }

  TZipFileEntry = Class(TCollectionItem)
  private
    FArchiveFileName: String;
    FAttributes: LongInt;
    FDateTime: TDateTime;
    FDiskFileName: String;
    FHeaderPos: int64;
    FOS: Byte;
    FSize: Integer;
    FStream: TStream;
    FCompressionLevel: TCompressionlevel;
    function GetArchiveFileName: String;
  Protected
    Property HdrPos : int64 Read FHeaderPos Write FheaderPos;
  Public
    constructor Create(ACollection: TCollection); override;
    function IsDirectory: Boolean;
    function IsLink: Boolean;
    Procedure Assign(Source : TPersistent); override;
    Property Stream : TStream Read FStream Write FStream;
  Published
    Property ArchiveFileName : String Read GetArchiveFileName Write FArchiveFileName;
    Property DiskFileName : String Read FDiskFileName Write FDiskFileName;
    Property Size : Integer Read FSize Write FSize;
    Property DateTime : TDateTime Read FDateTime Write FDateTime;
    property OS: Byte read FOS write FOS;
    property Attributes: LongInt read FAttributes write FAttributes;
    Property CompressionLevel: TCompressionlevel read FCompressionLevel write FCompressionLevel;
  end;

  { TZipFileEntries }

  TZipFileEntries = Class(TCollection)
  private
    function GetZ(AIndex : Integer): TZipFileEntry;
    procedure SetZ(AIndex : Integer; const AValue: TZipFileEntry);
  Public
    Function AddFileEntry(Const ADiskFileName : String): TZipFileEntry;
    Function AddFileEntry(Const ADiskFileName, AArchiveFileName : String): TZipFileEntry;
    Function AddFileEntry(Const AStream : TSTream; Const AArchiveFileName : String): TZipFileEntry;
    Procedure AddFileEntries(Const List : TStrings);
    Property Entries[AIndex : Integer] : TZipFileEntry Read GetZ Write SetZ; default;
  end;

  { TZipper }

  TZipper = Class(TObject)
  Private
    FEntries: TZipFileEntries;
    FZipping    : Boolean;
    FBufSize    : LongWord;
    FFileName   : String;         { Name of resulting Zip file                 }
    FFileComment: String;
    FFiles      : TStrings;
    FInMemSize  : Integer;
    FOutStream  : TStream;
    FInFile     : TStream;     { I/O file variables                         }
    LocalHdr    : Local_File_Header_Type;
    CentralHdr  : Central_File_Header_Type;
    EndHdr      : End_of_Central_Dir_Type;
    FOnPercent  : LongInt;
    FOnProgress : TProgressEvent;
    FOnEndOfFile : TOnEndOfFileEvent;
    FOnStartFile : TOnStartFileEvent;
    function CheckEntries: Integer;
    procedure SetEntries(const AValue: TZipFileEntries);
  Protected
    Procedure CloseInput(Item : TZipFileEntry);
    Procedure StartZipFile(Item : TZipFileEntry);
    Function  UpdateZipHeader(Item : TZipFileEntry; FZip : TStream; ACRC : LongWord;AMethod : Word; AZipVersionReqd : Word; AZipBitFlag : Word) : Boolean;
    Procedure BuildZipDirectory;
    Procedure DoEndOfFile;
    Procedure ZipOneFile(Item : TZipFileEntry); virtual;
    Function  OpenInput(Item : TZipFileEntry) : Boolean;
    Procedure GetFileInfo;
    Procedure SetBufSize(Value : LongWord);
    Procedure SetFileName(Value : String);
    Function CreateCompressor(Item : TZipFileEntry; AinFile,AZipStream : TStream) : TCompressor; virtual;
  Public
    Constructor Create;
    Destructor Destroy;override;
    Procedure ZipAllFiles; virtual;
    Procedure SaveToFile(AFileName: string);
    Procedure SaveToStream(AStream: TStream);
    Procedure ZipFiles(AFileName : String; FileList : TStrings);
    Procedure ZipFiles(FileList : TStrings);
    Procedure ZipFiles(AFileName : String; Entries : TZipFileEntries);
    Procedure ZipFiles(Entries : TZipFileEntries);
    Procedure Clear;
  Public
    Property BufferSize : LongWord Read FBufSize Write SetBufSize;
    Property OnPercent : Integer Read FOnPercent Write FOnPercent;
    Property OnProgress : TProgressEvent Read FOnProgress Write FOnProgress;
    Property OnStartFile : TOnStartFileEvent Read FOnStartFile Write FOnStartFile;
    Property OnEndFile : TOnEndOfFileEvent Read FOnEndOfFile Write FOnEndOfFile;
    Property FileName : String Read FFileName Write SetFileName;
    Property FileComment: String Read FFileComment Write FFileComment;
    // Deprecated. Use Entries.AddFileEntry(FileName) or Entries.AddFileEntries(List) instead.
    Property Files : TStrings Read FFiles; deprecated;
    Property InMemSize : Integer Read FInMemSize Write FInMemSize;
    Property Entries : TZipFileEntries Read FEntries Write SetEntries;
  end;

  { TFullZipFileEntry }

  TFullZipFileEntry = Class(TZipFileEntry)
  private
    FCompressedSize: LongWord;
    FCompressMethod: Word;
    FCRC32: LongWord;
  Public
    Property CompressMethod : Word Read FCompressMethod;
    Property CompressedSize : LongWord Read FCompressedSize;
    property CRC32: LongWord read FCRC32 write FCRC32;
  end;

  TOnCustomStreamEvent = Procedure(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry) of object;
  TCustomInputStreamEvent = Procedure(Sender: TObject; var AStream: TStream) of object;

  { TFullZipFileEntries }

  TFullZipFileEntries = Class(TZipFileEntries)
  private
    function GetFZ(AIndex : Integer): TFullZipFileEntry;
    procedure SetFZ(AIndex : Integer; const AValue: TFullZipFileEntry);
  Public
    Property FullEntries[AIndex : Integer] : TFullZipFileEntry Read GetFZ Write SetFZ; default;
  end;

  { TUnZipper }

  TUnZipper = Class(TObject)
  Private
    FOnCloseInputStream: TCustomInputStreamEvent;
    FOnCreateStream: TOnCustomStreamEvent;
    FOnDoneStream: TOnCustomStreamEvent;
    FOnOpenInputStream: TCustomInputStreamEvent;
    FUnZipping  : Boolean;
    FBufSize    : LongWord;
    FFileName   : String;         { Name of resulting Zip file                 }
    FOutputPath : String;
    FFileComment: String;
    FEntries    : TFullZipFileEntries;
    FFiles      : TStrings;
    FZipStream  : TStream;     { I/O file variables                         }
    LocalHdr    : Local_File_Header_Type;
    CentralHdr  : Central_File_Header_Type;
    EndHdr      : End_of_Central_Dir_Type;

    FOnPercent  : LongInt;
    FOnProgress : TProgressEvent;
    FOnEndOfFile : TOnEndOfFileEvent;
    FOnStartFile : TOnStartFileEvent;
  Protected
    Procedure OpenInput;
    Procedure CloseOutput(Item : TFullZipFileEntry; var OutStream: TStream);
    Procedure CloseInput;
    Procedure ReadZipDirectory;
    Procedure ReadZipHeader(Item : TFullZipFileEntry; out AMethod : Word);
    Procedure DoEndOfFile;
    Procedure UnZipOneFile(Item : TFullZipFileEntry); virtual;
    Function  OpenOutput(OutFileName : String; var OutStream: TStream; Item : TFullZipFileEntry) : Boolean;
    Procedure SetBufSize(Value : LongWord);
    Procedure SetFileName(Value : String);
    Procedure SetOutputPath(Value:String);
    Function CreateDeCompressor(Item : TZipFileEntry; AMethod : Word;AZipFile,AOutFile : TStream) : TDeCompressor; virtual;
  Public
    Constructor Create;
    Destructor Destroy;override;
    Procedure UnZipAllFiles; virtual;
    Procedure UnZipFiles(AFileName : String; FileList : TStrings);
    Procedure UnZipFiles(FileList : TStrings);
    Procedure UnZipAllFiles(AFileName : String);
    Procedure Clear;
    Procedure Examine;
  Public
    Property BufferSize : LongWord Read FBufSize Write SetBufSize;
    Property OnOpenInputStream: TCustomInputStreamEvent read FOnOpenInputStream write FOnOpenInputStream;
    Property OnCloseInputStream: TCustomInputStreamEvent read FOnCloseInputStream write FOnCloseInputStream;
    Property OnCreateStream : TOnCustomStreamEvent Read FOnCreateStream Write FOnCreateStream;
    Property OnDoneStream : TOnCustomStreamEvent Read FOnDoneStream Write FOnDoneStream;
    Property OnPercent : Integer Read FOnPercent Write FOnPercent;
    Property OnProgress : TProgressEvent Read FOnProgress Write FOnProgress;
    Property OnStartFile : TOnStartFileEvent Read FOnStartFile Write FOnStartFile;
    Property OnEndFile : TOnEndOfFileEvent Read FOnEndOfFile Write FOnEndOfFile;
    Property FileName : String Read FFileName Write SetFileName;
    Property OutputPath : String Read FOutputPath Write SetOutputPath;
    Property FileComment: String Read FFileComment;
    Property Files : TStrings Read FFiles;
    Property Entries : TFullZipFileEntries Read FEntries;
  end;

  EZipError = Class(Exception);

Implementation

ResourceString
  SErrBufsizeChange = 'Changing buffer size is not allowed while (un)zipping';
  SErrFileChange = 'Changing output file name is not allowed while (un)zipping';
  SErrInvalidCRC = 'Invalid CRC checksum while unzipping %s';
  SErrCorruptZIP = 'Corrupt ZIP file %s';
  SErrUnsupportedCompressionFormat = 'Unsupported compression format %d';
  SErrMissingFileName = 'Missing filename in entry %d';
  SErrMissingArchiveName = 'Missing archive filename in streamed entry %d';
  SErrFileDoesNotExist = 'File "%s" does not exist.';
  SErrNoFileName = 'No archive filename for examine operation.';
  SErrNoStream = 'No stream is opened.';

{ ---------------------------------------------------------------------
    Auxiliary
  ---------------------------------------------------------------------}

{$IFDEF FPC_BIG_ENDIAN}
function SwapLFH(const Values: Local_File_Header_Type): Local_File_Header_Type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.Extract_Version_Reqd := SwapEndian(Extract_Version_Reqd);
    Result.Bit_Flag := SwapEndian(Bit_Flag);
    Result.Compress_Method := SwapEndian(Compress_Method);
    Result.Last_Mod_Time := SwapEndian(Last_Mod_Time);
    Result.Last_Mod_Date := SwapEndian(Last_Mod_Date);
    Result.Crc32 := SwapEndian(Crc32);
    Result.Compressed_Size := SwapEndian(Compressed_Size);
    Result.Uncompressed_Size := SwapEndian(Uncompressed_Size);
    Result.Filename_Length := SwapEndian(Filename_Length);
    Result.Extra_Field_Length := SwapEndian(Extra_Field_Length);
  end;
end;

function SwapCFH(const Values: Central_File_Header_Type): Central_File_Header_Type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.MadeBy_Version := SwapEndian(MadeBy_Version);
    Result.Extract_Version_Reqd := SwapEndian(Extract_Version_Reqd);
    Result.Bit_Flag := SwapEndian(Bit_Flag);
    Result.Compress_Method := SwapEndian(Compress_Method);
    Result.Last_Mod_Time := SwapEndian(Last_Mod_Time);
    Result.Last_Mod_Date := SwapEndian(Last_Mod_Date);
    Result.Crc32 := SwapEndian(Crc32);
    Result.Compressed_Size := SwapEndian(Compressed_Size);
    Result.Uncompressed_Size := SwapEndian(Uncompressed_Size);
    Result.Filename_Length := SwapEndian(Filename_Length);
    Result.Extra_Field_Length := SwapEndian(Extra_Field_Length);
    Result.File_Comment_Length := SwapEndian(File_Comment_Length);
    Result.Starting_Disk_Num := SwapEndian(Starting_Disk_Num);
    Result.Internal_Attributes := SwapEndian(Internal_Attributes);
    Result.External_Attributes := SwapEndian(External_Attributes);
    Result.Local_Header_Offset := SwapEndian(Local_Header_Offset);
  end;
end;

function SwapECD(const Values: End_of_Central_Dir_Type): End_of_Central_Dir_Type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.Disk_Number := SwapEndian(Disk_Number);
    Result.Central_Dir_Start_Disk := SwapEndian(Central_Dir_Start_Disk);
    Result.Entries_This_Disk := SwapEndian(Entries_This_Disk);
    Result.Total_Entries := SwapEndian(Total_Entries);
    Result.Central_Dir_Size := SwapEndian(Central_Dir_Size);
    Result.Start_Disk_Offset := SwapEndian(Start_Disk_Offset);
    Result.ZipFile_Comment_Length := SwapEndian(ZipFile_Comment_Length);
  end;
end;
{$ENDIF FPC_BIG_ENDIAN}

Procedure DateTimeToZipDateTime(DT : TDateTime; out ZD,ZT : Word);

Var
  Y,M,D,H,N,S,MS : Word;

begin
  DecodeDate(DT,Y,M,D);
  DecodeTime(DT,H,N,S,MS);
  Y:=Y-1980;
  ZD:=d+(32*M)+(512*Y);
  ZT:=(S div 2)+(32*N)+(2048*h);
end;

Procedure ZipDateTimeToDateTime(ZD,ZT : Word;out DT : TDateTime);

Var
  Y,M,D,H,N,S,MS : Word;

begin
  MS:=0;
  S:=(ZT and 31) shl 1;
  N:=(ZT shr 5) and 63;
  H:=(ZT shr 12) and 31;
  D:=ZD and 31;
  M:=(ZD shr 5) and 15;
  Y:=((ZD shr 9) and 127)+1980;

  if M < 1 then M := 1;
  if D < 1 then D := 1;
  DT:=ComposeDateTime(EncodeDate(Y,M,D),EncodeTime(H,N,S,MS));
end;

const
  OS_FAT = 0;
  OS_UNIX = 3;

  UNIX_MASK = $F000;
  UNIX_FIFO = $1000;
  UNIX_CHAR = $2000;
  UNIX_DIR  = $4000;
  UNIX_BLK  = $6000;
  UNIX_FILE = $8000;
  UNIX_LINK = $A000;
  UNIX_SOCK = $C000;


  UNIX_RUSR = $0100;
  UNIX_WUSR = $0080;
  UNIX_XUSR = $0040;

  UNIX_RGRP = $0020;
  UNIX_WGRP = $0010;
  UNIX_XGRP = $0008;

  UNIX_ROTH = $0004;
  UNIX_WOTH = $0002;
  UNIX_XOTH = $0001;

  UNIX_DEFAULT = UNIX_RUSR or UNIX_WUSR or UNIX_XUSR or UNIX_RGRP or UNIX_ROTH;


function ZipUnixAttrsToFatAttrs(const Name: String; Attrs: Longint): Longint;
begin
  Result := faArchive;

  if (Pos('.', Name) = 1) and (Name <> '.') and (Name <> '..') then
    Result := Result + faHidden;
  case (Attrs and UNIX_MASK) of
    UNIX_DIR:  Result := Result + faDirectory;
    UNIX_LINK: Result := Result + faSymLink;
    UNIX_FIFO, UNIX_CHAR, UNIX_BLK, UNIX_SOCK:
               Result := Result + faSysFile;
  end;

  if (Attrs and UNIX_WUSR) = 0 then
    Result := Result + faReadOnly;
end;

function ZipFatAttrsToUnixAttrs(Attrs: Longint): Longint;
begin
  Result := UNIX_DEFAULT;
  if (faReadOnly and Attrs) > 0 then
    Result := Result and not (UNIX_WUSR);

  if (faSymLink and Attrs) > 0 then
    Result := Result or UNIX_LINK
  else
    if (faDirectory and Attrs) > 0 then
      Result := Result or UNIX_DIR
    else
      Result := Result or UNIX_FILE;
end;

{ ---------------------------------------------------------------------
    TDeCompressor
  ---------------------------------------------------------------------}


Procedure TDeCompressor.UpdC32(Octet: Byte);

Begin
  FCrc32Val := Crc_32_Tab[Byte(FCrc32Val XOR LongInt(Octet))] XOR ((FCrc32Val SHR 8) AND $00FFFFFF);
end;

constructor TDeCompressor.Create(AInFile, AOutFile: TStream; ABufSize: LongWord);
begin
  FinFile:=AInFile;
  FoutFile:=AOutFile;
  FBufferSize:=ABufSize;
  CRC32Val:=$FFFFFFFF;
end;


{ ---------------------------------------------------------------------
    TCompressor
  ---------------------------------------------------------------------}


Procedure TCompressor.UpdC32(Octet: Byte);

Begin
  FCrc32Val := Crc_32_Tab[Byte(FCrc32Val XOR LongInt(Octet))] XOR ((FCrc32Val SHR 8) AND $00FFFFFF);
end;

constructor TCompressor.Create(AInFile, AOutFile: TStream; ABufSize: LongWord);
begin
  FinFile:=AInFile;
  FoutFile:=AOutFile;
  FBufferSize:=ABufSize;
  CRC32Val:=$FFFFFFFF;
end;


{ ---------------------------------------------------------------------
    TDeflater
  ---------------------------------------------------------------------}

constructor TDeflater.Create(AInFile, AOutFile: TStream; ABufSize: LongWord);
begin
  Inherited;
  FCompressionLevel:=clDefault;
end;


procedure TDeflater.Compress;

Var
  Buf : PByte;
  I,Count,NewCount : Integer;
  C : TCompressionStream;
  BytesNow : Integer;
  NextMark : Integer;
  OnBytes : Integer;
  FSize    : Integer;
begin
  CRC32Val:=$FFFFFFFF;
  Buf:=GetMem(FBufferSize);
  if FOnPercent = 0 then
    FOnPercent := 1;
  OnBytes:=Round((FInFile.Size * FOnPercent) / 100);
  BytesNow:=0; NextMark := OnBytes;
  FSize:=FInfile.Size;
  Try
    C:=TCompressionStream.Create(FCompressionLevel,FOutFile,True);
    Try
      if assigned(FOnProgress) then
        fOnProgress(self,0);
      Repeat
        Count:=FInFile.Read(Buf^,FBufferSize);
        For I:=0 to Count-1 do
          UpdC32(Buf[i]);
        NewCount:=Count;
        While (NewCount>0) do
          NewCount:=NewCount-C.Write(Buf^,NewCount);
        inc(BytesNow,Count);
        if BytesNow>NextMark Then
          begin
            if (FSize>0) and assigned(FOnProgress) Then
              FOnProgress(self,100 * ( BytesNow / FSize));
            inc(NextMark,OnBytes);
          end;
      Until (Count=0);
    Finally
      C.Free;
    end;
  Finally
    FreeMem(Buf);
  end;
  if assigned(FOnProgress) then
    fOnProgress(self,100.0);
  Crc32Val:=NOT Crc32Val;
end;

class function TDeflater.ZipID: Word;
begin
  Result:=8;
end;

class function TDeflater.ZipVersionReqd: Word;
begin
  Result:=20;
end;

function TDeflater.ZipBitFlag: Word;
begin
  case CompressionLevel of
    clnone: Result := %110;
    clfastest: Result := %100;
    cldefault: Result := %000;
    clmax: Result := %010;
    else
      Result := 0;
  end;
  Result:=Result or %100000000000;
end;

{ ---------------------------------------------------------------------
    TInflater
  ---------------------------------------------------------------------}

constructor TInflater.Create(AInFile, AOutFile: TStream; ABufSize: LongWord);
begin
  Inherited;
end;


procedure TInflater.DeCompress;

Var
  Buf : PByte;
  I,Count : Integer;
  C : TDeCompressionStream;
  BytesNow : Integer;
  NextMark : Integer;
  OnBytes  : Integer;
  FSize    : Integer;

begin
  CRC32Val:=$FFFFFFFF;
  if FOnPercent = 0 then
    FOnPercent := 1;
  OnBytes:=Round((FInFile.Size * FOnPercent) / 100);
  BytesNow:=0; NextMark := OnBytes;
  FSize:=FInfile.Size;

  If Assigned(FOnProgress) then
    fOnProgress(self,0);

  Buf:=GetMem(FBufferSize);
  Try
    C:=TDeCompressionStream.Create(FInFile,True);
    Try
      Repeat
        Count:=C.Read(Buf^,FBufferSize);
        For I:=0 to Count-1 do
          UpdC32(Buf[i]);
        FOutFile.Write(Buf^,Count);
        inc(BytesNow,Count);
        if BytesNow>NextMark Then
           begin
             if (FSize>0) and assigned(FOnProgress) Then
               FOnProgress(self,100 * ( BytesNow / FSize));
             inc(NextMark,OnBytes);
           end;
      Until (Count=0);
    Finally
      C.Free;
    end;
  Finally
    FreeMem(Buf);
  end;
 if assigned(FOnProgress) then
   fOnProgress(self,100.0);
  Crc32Val:=NOT Crc32Val;
end;

class function TInflater.ZipID: Word;
begin
  Result:=8;
end;


{ ---------------------------------------------------------------------
    TShrinker
  ---------------------------------------------------------------------}

Const
   DefaultInMemSize = 256*1024; { Files larger than 256k are processed on disk   }
   DefaultBufSize =  16384;     { Use 16K file buffers                             }
   MINBITS     =      9;        { Starting code size of 9 bits                     }
   MAXBITS     =     13;        { Maximum code size of 13 bits                     }
   SPECIAL     =    256;        { Special function code                            }
   INCSIZE     =      1;        { Code indicating a jump in code size              }
   CLEARCODE   =      2;        { Code indicating code table has been cleared      }
   STDATTR     =    faAnyFile;  { Standard file attribute for DOS Find First/Next  }

constructor TShrinker.Create(AInFile, AOutFile : TStream; ABufSize : LongWord);
begin
  Inherited;
  FBufSize:=ABufSize;
  InBuf:=GetMem(FBUFSIZE);
  OutBuf:=GetMem(FBUFSIZE);
  CodeTable:=GetMem(SizeOf(CodeTable^));
  FreeList:=GetMem(SizeOf(FreeList^));
end;

destructor TShrinker.Destroy;
begin
  FreeMem(CodeTable);
  FreeMem(FreeList);
  FreeMem(InBuf);
  FreeMem(OutBuf);
  inherited Destroy;
end;

Procedure TShrinker.Compress;

Var
   OneString : String;
   Remaining : Word;

begin
  BytesIn := 1;
  BytesOut := 1;
  InitializeCodeTable;
  FillInputBuffer;
  FirstCh:= TRUE;
  Crc32Val:=$FFFFFFFF;
  FOnBytes:=Round((FInFile.Size * FOnPercent) / 100);
  While NOT InputEof do
    begin
    Remaining:=Succ(MaxInBufIdx - InBufIdx);
    If Remaining>255 then
      Remaining:=255;
    If Remaining=0 then
      FillInputBuffer
    else
      begin
      SetLength(OneString,Remaining);
      Move(InBuf[InBufIdx], OneString[1], Remaining);
      Inc(InBufIdx, Remaining);
      ProcessLine(OneString);
      end;
    end;
   Crc32Val := NOT Crc32Val;
   ProcessLine('');
end;

class function TShrinker.ZipID: Word;
begin
  Result:=1;
end;

class function TShrinker.ZipVersionReqd: Word;
begin
  Result:=10;
end;

function TShrinker.ZipBitFlag: Word;
begin
  Result:=0;
end;


Procedure TShrinker.DoOnProgress(Const Pct: Double);

begin
  If Assigned(FOnProgress) then
    FOnProgress(Self,Pct);
end;


Procedure TShrinker.FillInputBuffer;

Begin
   MaxInbufIDx:=FInfile.Read(InBuf[0], FBufSize);
   If MaxInbufIDx=0 then
      InputEof := TRUE
   else
      InputEOF := FALSE;
   InBufIdx := 0;
end;


Procedure TShrinker.WriteOutputBuffer;
Begin
  FOutFile.WriteBuffer(OutBuf[0], OutBufIdx);
  OutBufIdx := 0;
end;


Procedure TShrinker.PutChar(B : Byte);

Begin
  OutBuf[OutBufIdx] := B;
  Inc(OutBufIdx);
  If OutBufIdx>=FBufSize then
    WriteOutputBuffer;
  Inc(BytesOut);
end;

Procedure TShrinker.FlushOutput;
Begin
  If OutBufIdx>0 then
    WriteOutputBuffer;
End;


procedure TShrinker.PutCode(Code : Smallint);

var
  ACode : LongInt;
  XSize : Smallint;

begin
  if (Code=-1) then
    begin
    if BitsUsed>0 then
      PutChar(SaveByte);
    end
  else
    begin
    ACode := Longint(Code);
    XSize := CodeSize+BitsUsed;
    ACode := (ACode shl BitsUsed) or SaveByte;
    while (XSize div 8) > 0 do
      begin
      PutChar(Lo(ACode));
      ACode := ACode shr 8;
      Dec(XSize,8);
      end;
    BitsUsed := XSize;
    SaveByte := Lo(ACode);
    end;
end;


Procedure TShrinker.InitializeCodeTable;

Var
   I  :  Word;
Begin
   For I := 0 to TableSize do
     begin
     With CodeTable^[I] do
       begin
       Child := -1;
       Sibling := -1;
       If (I<=255) then
         Suffix := I;
       end;
     If (I>=257) then
       FreeList^[I] := I;
     end;
   NextFree  := FIRSTENTRY;
   TableFull := FALSE;
end;


Procedure TShrinker.Prune(Parent : Word);

Var
   CurrChild   : Smallint;
   NextSibling : Smallint;
Begin
  CurrChild := CodeTable^[Parent].Child;
  { Find first Child that has descendants .. clear any that don't }
  While (CurrChild <> -1) AND (CodeTable^[CurrChild].Child = -1) do
    begin
    CodeTable^[Parent].Child := CodeTable^[CurrChild].Sibling;
    CodeTable^[CurrChild].Sibling := -1;
     { Turn on ClearList bit to indicate a cleared entry }
    ClearList[CurrChild DIV 8] := (ClearList[CurrChild DIV 8] OR (1 SHL (CurrChild MOD 8)));
    CurrChild := CodeTable^[Parent].Child;
    end;
  If CurrChild <> -1 then
    begin   { If there are any children left ...}
    Prune(CurrChild);
    NextSibling := CodeTable^[CurrChild].Sibling;
    While NextSibling <> -1 do
      begin
      If CodeTable^[NextSibling].Child = -1 then
        begin
        CodeTable^[CurrChild].Sibling := CodeTable^[NextSibling].Sibling;
        CodeTable^[NextSibling].Sibling := -1;
        { Turn on ClearList bit to indicate a cleared entry }
        ClearList[NextSibling DIV 8] := (ClearList[NextSibling DIV 8] OR (1 SHL (NextSibling MOD 8)));
        NextSibling := CodeTable^[CurrChild].Sibling;
        end
      else
        begin
        CurrChild := NextSibling;
        Prune(CurrChild);
        NextSibling := CodeTable^[CurrChild].Sibling;
        end;
      end;
    end;
end;


Procedure TShrinker.Clear_Table;
Var
   Node : Word;
Begin
   FillChar(ClearList, SizeOf(ClearList), $00);
   For Node := 0 to 255 do
     Prune(Node);
   NextFree := Succ(TABLESIZE);
   For Node := TABLESIZE downto FIRSTENTRY do
     begin
     If (ClearList[Node DIV 8] AND (1 SHL (Node MOD 8))) <> 0 then
       begin
       Dec(NextFree);
       FreeList^[NextFree] := Node;
       end;
     end;
   If NextFree <= TABLESIZE then
     TableFull := FALSE;
end;


Procedure TShrinker.Table_Add(Prefix : Word; Suffix : Byte);
Var
   FreeNode : Word;
Begin
  If NextFree <= TABLESIZE then
    begin
    FreeNode := FreeList^[NextFree];
    Inc(NextFree);
    CodeTable^[FreeNode].Child := -1;
    CodeTable^[FreeNode].Sibling := -1;
    CodeTable^[FreeNode].Suffix := Suffix;
    If CodeTable^[Prefix].Child  = -1 then
      CodeTable^[Prefix].Child := FreeNode
    else
      begin
      Prefix := CodeTable^[Prefix].Child;
      While CodeTable^[Prefix].Sibling <> -1 do
        Prefix := CodeTable^[Prefix].Sibling;
      CodeTable^[Prefix].Sibling := FreeNode;
      end;
    end;
  if NextFree > TABLESIZE then
    TableFull := TRUE;
end;

function TShrinker.Table_Lookup(    TargetPrefix : Smallint;
                          TargetSuffix : Byte;
                      Out FoundAt      : Smallint   ) : Boolean;

var TempPrefix : Smallint;

begin
  TempPrefix := TargetPrefix;
  Table_lookup := False;
  if CodeTable^[TempPrefix].Child <> -1 then
    begin
    TempPrefix := CodeTable^[TempPrefix].Child;
    repeat
      if CodeTable^[TempPrefix].Suffix = TargetSuffix then
        begin
        Table_lookup := True;
        break;
        end;
      if CodeTable^[TempPrefix].Sibling = -1 then
        break;
      TempPrefix := CodeTable^[TempPrefix].Sibling;
    until False;
  end;
  if Table_Lookup then
    FoundAt := TempPrefix
  else
    FoundAt := -1;
end;

Procedure TShrinker.Shrink(Suffix : Smallint);

Const
  LastCode : Smallint = 0;

Var
  WhereFound : Smallint;

Begin
  If FirstCh then
    begin
    SaveByte := $00;
    BitsUsed := 0;
    CodeSize := MINBITS;
    MaxCode  := (1 SHL CodeSize) - 1;
    LastCode := Suffix;
    FirstCh  := FALSE;
    end
  else
    begin
    If Suffix <> -1 then
      begin
      If TableFull then
        begin
        Putcode(LastCode);
        PutCode(SPECIAL);
        Putcode(CLEARCODE);
        Clear_Table;
        Table_Add(LastCode, Suffix);
        LastCode := Suffix;
        end
      else
        begin
        If Table_Lookup(LastCode, Suffix, WhereFound) then
          begin
          LastCode  := WhereFound;
          end
        else
          begin
          PutCode(LastCode);
          Table_Add(LastCode, Suffix);
          LastCode := Suffix;
          If (FreeList^[NextFree] > MaxCode) and (CodeSize < MaxBits) then
            begin
            PutCode(SPECIAL);
            PutCode(INCSIZE);
            Inc(CodeSize);
            MaxCode := (1 SHL CodeSize) -1;
            end;
          end;
        end;
      end
    else
      begin
      PutCode(LastCode);
      PutCode(-1);
      FlushOutput;
      end;
    end;
end;

Procedure TShrinker.ProcessLine(Const Source : String);

Var
  I : Word;

Begin
  If Source = '' then
    Shrink(-1)
  else
    For I := 1 to Length(Source) do
      begin
      Inc(BytesIn);
      If (Pred(BytesIn) MOD FOnBytes) = 0 then
        DoOnProgress(100 * ( BytesIn / FInFile.Size));
      UpdC32(Ord(Source[I]));
      Shrink(Ord(Source[I]));
      end;
end;

{ ---------------------------------------------------------------------
    TZipper
  ---------------------------------------------------------------------}


Procedure TZipper.GetFileInfo;

Var
  F    : TZipFileEntry;
  Info : TSearchRec;
  I    : LongWord;
{$IFDEF UNIX}
  UnixInfo: Stat;
{$ENDIF}
Begin
  For I := 0 to FEntries.Count-1 do
    begin
    F:=FEntries[i];
    If F.Stream=Nil then
      begin
      If (F.DiskFileName='') then
        Raise EZipError.CreateFmt(SErrMissingFileName,[I]);
      If FindFirstUTF8(F.DiskFileName, STDATTR, Info)=0 then
        try
          F.Size:=Info.Size;
          F.DateTime:=FileDateToDateTime(Info.Time);
        {$IFDEF UNIX}
          if fplstat(F.DiskFileName, @UnixInfo) = 0 then
            F.Attributes := UnixInfo.st_mode;
        {$ELSE}
          F.Attributes := Info.Attr;
        {$ENDIF}
        finally
          FindClose(Info);
        end
      else
        Raise EZipError.CreateFmt(SErrFileDoesNotExist,[F.DiskFileName]);
      end
    else
      begin
      If (F.ArchiveFileName='') then
        Raise EZipError.CreateFmt(SErrMissingArchiveName,[I]);
      F.Size:=F.Stream.Size;
    {$IFDEF UNIX}
      F.Attributes := UNIX_FILE or UNIX_DEFAULT;
    {$ELSE}
      F.Attributes := faArchive;
    {$ENDIF}
      end;
    end;
end;


procedure TZipper.SetEntries(const AValue: TZipFileEntries);
begin
  if FEntries=AValue then exit;
  FEntries.Assign(AValue);
end;

Function TZipper.OpenInput(Item : TZipFileEntry) : Boolean;

Begin
  If (Item.Stream<>nil) then
    FInFile:=Item.Stream
  else
    if Item.IsDirectory then
      FInFile := TStringStream.Create('')
    else
      FInFile:=TFileStreamUTF8.Create(Item.DiskFileName,fmOpenRead);
  Result:=True;
  If Assigned(FOnStartFile) then
    FOnStartFile(Self,Item.ArchiveFileName);
End;


Procedure TZipper.CloseInput(Item : TZipFileEntry);

Begin
  If (FInFile<>Item.Stream) then
    FreeAndNil(FInFile)
  else
    FinFile:=Nil;
  DoEndOfFile;
end;


Procedure TZipper.StartZipFile(Item : TZipFileEntry);

Begin
  FillChar(LocalHdr,SizeOf(LocalHdr),0);
  With LocalHdr do
    begin
    Signature := LOCAL_FILE_HEADER_SIGNATURE;
    Extract_Version_Reqd := 10;
    Bit_Flag := 0;
    Compress_Method := 1;
    DateTimeToZipDateTime(Item.DateTime,Last_Mod_Date,Last_Mod_Time);
    Crc32 := 0;
    Compressed_Size := 0;
    Uncompressed_Size := Item.Size;
    FileName_Length := 0;
    Extra_Field_Length := 0;
  end ;
End;


function TZipper.UpdateZipHeader(Item: TZipFileEntry; FZip: TStream;
  ACRC: LongWord; AMethod: Word; AZipVersionReqd: Word; AZipBitFlag: Word
  ): Boolean;
var
  ZFileName  : String;
Begin
  ZFileName:=Item.ArchiveFileName;
  With LocalHdr do
    begin
    FileName_Length := Length(ZFileName);
    Crc32 := ACRC;
    Result:=Not (Compressed_Size >= Uncompressed_Size);
    If Not Result then
      begin                     { No...                          }
      Compress_Method := 0;                  { ...change stowage type      }
      Compressed_Size := Uncompressed_Size;  { ...update compressed size   }
      end
    else
      begin
      Compress_method:=AMethod;
      Compressed_Size := FZip.Size;
      Bit_Flag := Bit_Flag or AZipBitFlag;
      if AZipVersionReqd > Extract_Version_Reqd then
        Extract_Version_Reqd := AZipVersionReqd;
      end;
    end;
  FOutStream.WriteBuffer({$IFDEF ENDIAN_BIG}SwapLFH{$ENDIF}(LocalHdr),SizeOf(LocalHdr));
  FOutStream.WriteBuffer(ZFileName[1],Length(ZFileName));
End;


Procedure TZipper.BuildZipDirectory;

Var
   SavePos   : int64;
   HdrPos    : int64;
   CenDirPos : int64;
   ACount    : Word;
   ZFileName  : String;

Begin
   ACount := 0;
   CenDirPos := FOutStream.Position;
   FOutStream.Seek(0,soBeginning);             { Rewind output file }
   HdrPos := FOutStream.Position;
   FOutStream.ReadBuffer(LocalHdr, SizeOf(LocalHdr));
{$IFDEF FPC_BIG_ENDIAN}
   LocalHdr := SwapLFH(LocalHdr);
{$ENDIF}
   Repeat
     SetLength(ZFileName,LocalHdr.FileName_Length);
     FOutStream.ReadBuffer(ZFileName[1], LocalHdr.FileName_Length);
     SavePos := FOutStream.Position;
     FillChar(CentralHdr,SizeOf(CentralHdr),0);
     With CentralHdr do
       begin
       Signature := CENTRAL_FILE_HEADER_SIGNATURE;
       MadeBy_Version := LocalHdr.Extract_Version_Reqd;
     {$IFDEF UNIX}
       MadeBy_Version := MadeBy_Version or (OS_UNIX shl 8);
     {$ENDIF}
       Move(LocalHdr.Extract_Version_Reqd, Extract_Version_Reqd, 26);
       Last_Mod_Time:=localHdr.Last_Mod_Time;
       Last_Mod_Date:=localHdr.Last_Mod_Date;
       File_Comment_Length := 0;
       Starting_Disk_Num := 0;
       Internal_Attributes := 0;
     {$IFDEF UNIX}
       External_Attributes := Entries[ACount].Attributes shl 16;
     {$ELSE}
       External_Attributes := Entries[ACount].Attributes;
     {$ENDIF}
       Local_Header_Offset := HdrPos;
       end;
     FOutStream.Seek(0,soEnd);
     FOutStream.WriteBuffer({$IFDEF FPC_BIG_ENDIAN}SwapCFH{$ENDIF}(CentralHdr),SizeOf(CentralHdr));
     FOutStream.WriteBuffer(ZFileName[1],Length(ZFileName));
     Inc(ACount);
     FOutStream.Seek(SavePos + LocalHdr.Compressed_Size,soBeginning);
     HdrPos:=FOutStream.Position;
     FOutStream.ReadBuffer(LocalHdr, SizeOf(LocalHdr));
{$IFDEF FPC_BIG_ENDIAN}
     LocalHdr := SwapLFH(LocalHdr);
{$ENDIF}
   Until LocalHdr.Signature = CENTRAL_FILE_HEADER_SIGNATURE;
   FOutStream.Seek(0,soEnd);
   FillChar(EndHdr,SizeOf(EndHdr),0);
   With EndHdr do
     begin
     Signature := END_OF_CENTRAL_DIR_SIGNATURE;
     Disk_Number := 0;
     Central_Dir_Start_Disk := 0;
     Entries_This_Disk := ACount;
     Total_Entries := ACount;
     Central_Dir_Size := FOutStream.Size-CenDirPos;
     Start_Disk_Offset := CenDirPos;
     ZipFile_Comment_Length := Length(FFileComment);
     FOutStream.WriteBuffer({$IFDEF FPC_BIG_ENDIAN}SwapECD{$ENDIF}(EndHdr), SizeOf(EndHdr));
     if Length(FFileComment) > 0 then
       FOutStream.WriteBuffer(FFileComment[1],Length(FFileComment));
     end;
end;

Function TZipper.CreateCompressor(Item : TZipFileEntry; AInFile,AZipStream : TStream) : TCompressor;

begin
  Result:=TDeflater.Create(AinFile,AZipStream,FBufSize);
  (Result as TDeflater).CompressionLevel:=Item.CompressionLevel;
end;

Procedure TZipper.ZipOneFile(Item : TZipFileEntry);

Var
  CRC : LongWord;
  ZMethod : Word;
  ZVersionReqd : Word;
  ZBitFlag : Word;
  ZipStream : TStream;
  TmpFileName : String;

Begin
  OpenInput(Item);
  Try
    StartZipFile(Item);
    If (FInfile.Size<=FInMemSize) then
      ZipStream:=TMemoryStream.Create
    else
      begin
      TmpFileName:=ChangeFileExt(FFileName,'.tmp');
      ZipStream:=TFileStreamUTF8.Create(TmpFileName,fmCreate);
      end;
    Try
      With CreateCompressor(Item, FinFile,ZipStream) do
        Try
          OnProgress:=Self.OnProgress;
          OnPercent:=Self.OnPercent;
          Compress;
          CRC:=Crc32Val;
          ZMethod:=ZipID;
          ZVersionReqd:=ZipVersionReqd;
          ZBitFlag:=ZipBitFlag;
        Finally
          Free;
        end;
      If UpdateZipHeader(Item,ZipStream,CRC,ZMethod,ZVersionReqd,ZBitFlag) then
        // Compressed file smaller than original file.
        FOutStream.CopyFrom(ZipStream,0)
      else
        begin
        // Original file smaller than compressed file.
        FInfile.Seek(0,soBeginning);
        FOutStream.CopyFrom(FInFile,0);
        end;
    finally
      ZipStream.Free;
      If (TmpFileName<>'') then
        DeleteFileUTF8(TmpFileName);
    end;
  Finally
    CloseInput(Item);
  end;
end;

// Just like SaveToFile, but uses the FileName property
Procedure TZipper.ZipAllFiles;

Begin
  SaveToFile(FileName);
end;

procedure TZipper.SaveToFile(AFileName: string);
var
  lStream: TFileStreamUTF8;
begin
  lStream:=TFileStreamUTF8.Create(FFileName,fmCreate);
  try
    SaveToStream(lStream);
  finally
    FreeAndNil(lStream);
  end;
end;

procedure TZipper.SaveToStream(AStream: TStream);
Var
   I : Integer;
   filecnt : integer;
begin
  FOutStream := AStream;

  If CheckEntries=0 then
    Exit;
  FZipping:=True;
  Try
    GetFileInfo;

    filecnt:=0;
    for I:=0 to FEntries.Count-1 do
    begin
      ZipOneFile(FEntries[i]);
      inc(filecnt);
    end;
    if filecnt>0 then
      BuildZipDirectory;
  finally
    FZipping:=False;
    // Remove entries that have been added by CheckEntries from Files.
    For I:=0 to FFiles.Count-1 do
      FEntries.Delete(FEntries.Count-1);
  end;
end;


Procedure TZipper.SetBufSize(Value : LongWord);

begin
  If FZipping then
    Raise EZipError.Create(SErrBufsizeChange);
  If Value>=DefaultBufSize then
    FBufSize:=Value;
end;

Procedure TZipper.SetFileName(Value : String);

begin
  If FZipping then
    Raise EZipError.Create(SErrFileChange);
  FFileName:=Value;
end;

Procedure TZipper.ZipFiles(AFileName : String; FileList : TStrings);

begin
  FFileName:=AFileName;
  ZipFiles(FileList);
end;

procedure TZipper.ZipFiles(FileList: TStrings);
begin
  FFiles.Assign(FileList);
  ZipAllFiles;
end;

procedure TZipper.ZipFiles(AFileName: String; Entries: TZipFileEntries);
begin
  FFileName:=AFileName;
  ZipFiles(Entries);
end;

procedure TZipper.ZipFiles(Entries: TZipFileEntries);
begin
  FEntries.Assign(Entries);
  ZipAllFiles;
end;

Procedure TZipper.DoEndOfFile;

Var
  ComprPct : Double;

begin
  If (LocalHdr.Uncompressed_Size>0) then
    ComprPct := (100.0 * (LocalHdr.Uncompressed_Size - LocalHdr.Compressed_Size)) / LocalHdr.Uncompressed_Size
  else
    ComprPct := 0;
  If Assigned(FOnEndOfFile) then
    FOnEndOfFile(Self,ComprPct);
end;

Constructor TZipper.Create;

begin
  FBufSize:=DefaultBufSize;
  FInMemSize:=DefaultInMemSize;
  FFiles:=TStringList.Create;
  FEntries:=TZipFileEntries.Create(TZipFileEntry);
  FOnPercent:=1;
end;

Function TZipper.CheckEntries : Integer;

Var
  I : Integer;

begin
  For I:=0 to FFiles.Count-1 do
    FEntries.AddFileEntry(FFiles[i]);
  Result:=FEntries.Count;
end;


Procedure TZipper.Clear;

begin
  FEntries.Clear;
  FFiles.Clear;
end;

Destructor TZipper.Destroy;

begin
  Clear;
  FreeAndNil(FEntries);
  FreeAndNil(FFiles);
  Inherited;
end;


{ ---------------------------------------------------------------------
    TUnZipper
  ---------------------------------------------------------------------}

Procedure TUnZipper.OpenInput;

Begin
  if Assigned(FOnOpenInputStream) then
    FOnOpenInputStream(Self, FZipStream);
  if FZipStream = nil then
    FZipStream:=TFileStreamUTF8.Create(FFileName,fmOpenRead);
End;


Function TUnZipper.OpenOutput(OutFileName : String; var OutStream: TStream; Item : TFullZipFileEntry) : Boolean;
Var
  Path: String;
  OldDirectorySeparators: set of char;
Begin
  { the default RTL behaviour is broken on Unix platforms
    for Windows compatibility: it allows both '/' and '\'
    as directory separator. We don't want that behaviour
    here, since 'abc\' is a valid file name under Unix.

	(mantis 15836) On the other hand, many archives on
	 windows have '/' as pathseparator, even Windows
	 generated .odt files. So we disable this for windows.
  }
  OldDirectorySeparators:=AllowDirectorySeparators;
  {$ifndef Windows}
  AllowDirectorySeparators:=[DirectorySeparator];
  {$endif}
  Path:=ExtractFilePath(OutFileName);
  OutStream:=Nil;
  If Assigned(FOnCreateStream) then
    FOnCreateStream(Self, OutStream, Item);
  // If FOnCreateStream didn't create one, we create one now.
  If (OutStream=Nil) then
    Begin
    if (Path<>'') then
      ForceDirectories(Path);
    AllowDirectorySeparators:=OldDirectorySeparators;
    OutStream:=TFileStreamUTF8.Create(OutFileName,fmCreate);
    end;

  AllowDirectorySeparators:=OldDirectorySeparators;
  Result:=True;
  If Assigned(FOnStartFile) then
    FOnStartFile(Self,OutFileName);

End;


Procedure TUnZipper.CloseOutput(Item : TFullZipFileEntry; var OutStream: TStream);

Begin
  if Assigned(FOnDoneStream) then
  begin
    FOnDoneStream(Self, OutStream, Item);
    OutStream := nil;
  end
  else
    FreeAndNil(OutStream);
  DoEndOfFile;
end;


Procedure TUnZipper.CloseInput;

Begin
  if Assigned(FOnCloseInputStream) then
    FOnCloseInputStream(Self, FZipStream);
  FreeAndNil(FZipStream);
end;


Procedure TUnZipper.ReadZipHeader(Item : TFullZipFileEntry; out AMethod : Word);
Var
  S : String;
  D : TDateTime;
Begin
  FZipStream.Seek(Item.HdrPos,soBeginning);
  FZipStream.ReadBuffer(LocalHdr,SizeOf(LocalHdr));
{$IFDEF FPC_BIG_ENDIAN}
  LocalHdr := SwapLFH(LocalHdr);
{$ENDIF}
  With LocalHdr do
    begin
      SetLength(S,Filename_Length);
      FZipStream.ReadBuffer(S[1],Filename_Length);
      //SetLength(E,Extra_Field_Length);
      //FZipStream.ReadBuffer(E[1],Extra_Field_Length);
      FZipStream.Seek(Extra_Field_Length,soCurrent);
      Item.ArchiveFileName:=S;
      Item.DiskFileName:=S;
      Item.Size:=Uncompressed_Size;
      ZipDateTimeToDateTime(Last_Mod_Date,Last_Mod_Time,D);
      Item.DateTime:=D;
      if Crc32 <> 0 then
        Item.CRC32 := Crc32;
      AMethod:=Compress_method;
    end;
End;

procedure FindEndHeader(AZip: TStream; out AEndHdr: End_of_Central_Dir_Type; out AEndHdrPos: Int64; out AZipFileComment: string);
var
  Buf: PByte;
  BufSize: Integer;
  I: Integer;
begin
  AZipFileComment := '';
  AEndHdrPos := AZip.Size - SizeOf(AEndHdr);
  if AEndHdrPos < 0 then
  begin
    AEndHdrPos := -1;
    FillChar(AEndHdr, SizeOf(AEndHdr), 0);
    exit;
  end;
  AZip.Seek(AEndHdrPos, soBeginning);
  AZip.ReadBuffer(AEndHdr, SizeOf(AEndHdr));
  {$IFDEF FPC_BIG_ENDIAN}
  AEndHdr := SwapECD(AEndHdr);
  {$ENDIF}
  if (AEndHdr.Signature = END_OF_CENTRAL_DIR_SIGNATURE) and
     (AEndHdr.ZipFile_Comment_Length = 0) then
    exit;

  // scan the last (64k + something) bytes for the END_OF_CENTRAL_DIR_SIGNATURE
  // (zip file comments are 64k max)
  BufSize := 65536 + SizeOf(AEndHdr) + 128;
  if AZip.Size < BufSize then
    BufSize := AZip.Size;

  Buf := GetMem(BufSize);
  try
    AZip.Seek(AZip.Size - BufSize, soBeginning);
    AZip.ReadBuffer(Buf^, BufSize);

    for I := BufSize - SizeOf(AEndHdr) downto 0 do
    begin
      if (Buf[I] or (Buf[I + 1] shl 8) or (Buf[I + 2] shl 16) or (Buf[I + 3] shl 24)) = END_OF_CENTRAL_DIR_SIGNATURE then
      begin
        Move(Buf[I], AEndHdr, SizeOf(AEndHdr));
        {$IFDEF FPC_BIG_ENDIAN}
        AEndHdr := SwapECD(AEndHdr);
        {$ENDIF}
        if (AEndHdr.Signature = END_OF_CENTRAL_DIR_SIGNATURE) and
           (I + SizeOf(AEndHdr) + AEndHdr.ZipFile_Comment_Length = BufSize) then
        begin
          AEndHdrPos := AZip.Size - BufSize + I;
          AZip.Seek(AEndHdrPos + SizeOf(AEndHdr), soBeginning);
          SetLength(AZipFileComment, AEndHdr.ZipFile_Comment_Length);
          AZip.ReadBuffer(AZipFileComment[1], Length(AZipFileComment));
          exit;
        end;
      end;
    end;

    AEndHdrPos := -1;
    FillChar(AEndHdr, SizeOf(AEndHdr), 0);
  finally
    FreeMem(Buf);
  end;
end;

Procedure TUnZipper.ReadZipDirectory;

Var
  i : LongWord; //todo: expand to 8 bytes when introducing zip64 format
  EndHdrPos,
  CenDirPos : Int64;
  NewNode   : TFullZipFileEntry;
  D : TDateTime;
  S : String;
Begin
  FindEndHeader(FZipStream, EndHdr, EndHdrPos, FFileComment);
  if EndHdrPos < 0 then
    raise EZipError.CreateFmt(SErrCorruptZIP,[FileName]);
  CenDirPos := EndHdr.Start_Disk_Offset;
  FZipStream.Seek(CenDirPos,soBeginning);
  FEntries.Clear;
  for i:=0 to EndHdr.Entries_This_Disk-1 do
    begin
    FZipStream.ReadBuffer(CentralHdr, SizeOf(CentralHdr));
{$IFDEF FPC_BIG_ENDIAN}
    CentralHdr := SwapCFH(CentralHdr);
{$ENDIF}
    With CentralHdr do
      begin
      if Signature<>CENTRAL_FILE_HEADER_SIGNATURE then
        raise EZipError.CreateFmt(SErrCorruptZIP,[FileName]);
      NewNode:=FEntries.Add as TFullZipFileEntry;
      NewNode.HdrPos := Local_Header_Offset;
      SetLength(S,Filename_Length);
      FZipStream.ReadBuffer(S[1],Filename_Length);
      NewNode.ArchiveFileName:=S;
      NewNode.Size:=Uncompressed_Size;
      NewNode.FCompressedSize:=Compressed_Size;
      NewNode.CRC32:=CRC32;
      NewNode.OS := MadeBy_Version shr 8;

      if NewNode.OS = OS_UNIX then
        NewNode.Attributes := External_Attributes shr 16
      else
        NewNode.Attributes := External_Attributes;
      ZipDateTimeToDateTime(Last_Mod_Date,Last_Mod_Time,D);
      NewNode.DateTime:=D;
      FZipStream.Seek(Extra_Field_Length+File_Comment_Length,soCurrent);
      end;
   end;
end;

Function TUnZipper.CreateDeCompressor(Item : TZipFileEntry; AMethod : Word;AZipFile,AOutFile : TStream) : TDeCompressor;
begin
  case AMethod of
    8 :
      Result:=TInflater.Create(AZipFile,AOutFile,FBufSize);
  else
    raise EZipError.CreateFmt(SErrUnsupportedCompressionFormat,[AMethod]);
  end;
end;

Procedure TUnZipper.UnZipOneFile(Item : TFullZipFileEntry);

Var
  Count: int64;
  Attrs: Longint;
  ZMethod : Word;
  LinkTargetStream: TStringStream;
  OutputFileName: string;
  FOutStream: TStream;
  IsLink: Boolean;
  IsCustomStream: Boolean;


  procedure DoUnzip(const Dest: TStream);
  begin
    if ZMethod=0 then
    begin
      if (LocalHdr.Compressed_Size<>0) then
        begin
          Count:=Dest.CopyFrom(FZipStream,LocalHdr.Compressed_Size)
         {$warning TODO: Implement CRC Check}
        end
      else
        Count:=0;
    end
    else
    With CreateDecompressor(Item, ZMethod, FZipStream, Dest) do
      Try
        OnProgress:=Self.OnProgress;
        OnPercent:=Self.OnPercent;
        DeCompress;
        if Item.CRC32 <> Crc32Val then
          raise EZipError.CreateFmt(SErrInvalidCRC,[Item.ArchiveFileName]);
      Finally
        Free;
      end;
  end;
Begin
  ReadZipHeader(Item, ZMethod);
  OutputFileName:=Item.DiskFileName;

  IsCustomStream := Assigned(FOnCreateStream);


  if (IsCustomStream = False) and (FOutputPath<>'') then
    OutputFileName:=IncludeTrailingPathDelimiter(FOutputPath)+OutputFileName;

  IsLink := Item.IsLink;

{$IFNDEF UNIX}
  if IsLink and Not IsCustomStream then
  begin
    {$warning TODO: Implement symbolic link creation for non-unix}
    IsLink := False;
  end;
{$ENDIF}


  if IsCustomStream then
  begin
    try
      OpenOutput(OutputFileName, FOutStream, Item);
      if (IsLink = False) and (Item.IsDirectory = False) then
        DoUnzip(FOutStream);
    Finally
      CloseOutput(Item, FOutStream);
    end;
  end
  else
  begin
    if IsLink then
    begin
    {$IFDEF UNIX}
      LinkTargetStream := TStringStream.Create('');
      try
        DoUnzip(LinkTargetStream);
        fpSymlink(PChar(LinkTargetStream.DataString), PChar(OutputFileName));
      finally
        LinkTargetStream.Free;
      end;
    {$ENDIF}
    end
    else
    begin
      if Item.IsDirectory then
        CreateDir(OutputFileName)
      else
      begin
        try
          OpenOutput(OutputFileName, FOutStream, Item);
          DoUnzip(FOutStream);
        Finally
          CloseOutput(Item, FOutStream);
        end;
      end;
    end;
  end;


  if Not IsCustomStream then
  begin
    // set attributes
    FileSetDate(OutputFileName, DateTimeToFileDate(Item.DateTime));

    if (Item.Attributes <> 0) then
    begin
      Attrs := 0;
    {$IFDEF UNIX}
      if Item.OS = OS_UNIX then Attrs := Item.Attributes;
      if Item.OS = OS_FAT then
        Attrs := ZipFatAttrsToUnixAttrs(Item.Attributes);
    {$ELSE}
      if Item.OS = OS_FAT then Attrs := Item.Attributes;
      if Item.OS = OS_UNIX then
        Attrs := ZipUnixAttrsToFatAttrs(ExtractFileName(Item.ArchiveFileName), Item.Attributes);
    {$ENDIF}

      if Attrs <> 0 then
      begin
    {$IFDEF UNIX}
      FpChmod(OutputFileName, Attrs);
    {$ELSE}
      FileSetAttr(OutputFileName, Attrs);
    {$ENDIF}
      end;
    end;
  end;
end;


Procedure TUnZipper.UnZipAllFiles;
Var
   Item : TFullZipFileEntry;
   I : Integer;
   AllFiles : Boolean;

Begin
  FUnZipping:=True;
  Try
    AllFiles:=(FFiles.Count=0);
    OpenInput;
    Try
      ReadZipDirectory;
      For I:=0 to FEntries.Count-1 do
        begin
        Item:=FEntries[i];
        if AllFiles or (FFiles.IndexOf(Item.ArchiveFileName)<>-1) then
          UnZipOneFile(Item);
        end;
    Finally
      CloseInput;
    end;
  finally
    FUnZipping:=False;
  end;
end;


Procedure TUnZipper.SetBufSize(Value : LongWord);

begin
  If FUnZipping then
    Raise EZipError.Create(SErrBufsizeChange);
  If Value>=DefaultBufSize then
    FBufSize:=Value;
end;

Procedure TUnZipper.SetFileName(Value : String);

begin
  If FUnZipping then
    Raise EZipError.Create(SErrFileChange);
  FFileName:=Value;
end;

Procedure TUnZipper.SetOutputPath(Value:String);
begin
  If FUnZipping then
    Raise EZipError.Create(SErrFileChange);
  FOutputPath:=Value;
end;

Procedure TUnZipper.UnZipFiles(AFileName : String; FileList : TStrings);

begin
  FFileName:=AFileName;
  UNzipFiles(FileList);
end;

procedure TUnZipper.UnZipFiles(FileList: TStrings);
begin
  FFiles.Assign(FileList);
  UnZipAllFiles;
end;

Procedure TUnZipper.UnZipAllFiles(AFileName : String);

begin
  FFileName:=AFileName;
  UnZipAllFiles;
end;

Procedure TUnZipper.DoEndOfFile;

Var
  ComprPct : Double;

begin
  If (LocalHdr.Uncompressed_Size>0) then
    ComprPct := (100.0 * (LocalHdr.Uncompressed_Size - LocalHdr.Compressed_Size)) / LocalHdr.Uncompressed_Size
  else
    ComprPct := 0;
  If Assigned(FOnEndOfFile) then
    FOnEndOfFile(Self,ComprPct);
end;

Constructor TUnZipper.Create;

begin
  FBufSize:=DefaultBufSize;
  FFiles:=TStringList.Create;
  TStringlist(FFiles).Sorted:=True;
  FEntries:=TFullZipFileEntries.Create(TFullZipFileEntry);
  FOnPercent:=1;
end;

Procedure TUnZipper.Clear;

begin
  FFiles.Clear;
  FEntries.Clear;
end;

procedure TUnZipper.Examine;
begin
  if (FOnOpenInputStream = nil) and (FFileName='') then
    Raise EZipError.Create(SErrNoFileName);
  OpenInput;
  If (FZipStream=nil) then
    Raise EZipError.Create(SErrNoStream);
  Try
    ReadZipDirectory;
  Finally
    CloseInput;
  end;
end;

Destructor TUnZipper.Destroy;

begin
  Clear;
  FreeAndNil(FFiles);
  FreeAndNil(FEntries);
  Inherited;
end;

{ TZipFileEntry }

function TZipFileEntry.GetArchiveFileName: String;
begin
  Result:=FArchiveFileName;
  If (Result='') then
    Result:=FDiskFileName;
end;

constructor TZipFileEntry.Create(ACollection: TCollection);

begin
{$IFDEF UNIX}
  FOS := OS_UNIX;
{$ELSE}
  FOS := OS_FAT;
{$ENDIF}
  FCompressionLevel:=cldefault;
  inherited create(ACollection);
end;

function TZipFileEntry.IsDirectory: Boolean;
begin
  Result := (DiskFileName <> '') and (DiskFileName[Length(DiskFileName)] in ['/', '\']);
  if Attributes <> 0 then
  begin
    case OS of
      OS_FAT: Result := (faDirectory and Attributes) > 0;
      OS_UNIX: Result := (Attributes and UNIX_MASK) = UNIX_DIR;
    end;
  end;
end;

function TZipFileEntry.IsLink: Boolean;
begin
  Result := False;
  if Attributes <> 0 then
  begin
    case OS of
      OS_FAT: Result := (faSymLink and Attributes) > 0;
      OS_UNIX: Result := (Attributes and UNIX_MASK) = UNIX_LINK;
    end;
  end;
end;

procedure TZipFileEntry.Assign(Source: TPersistent);

Var
  Z : TZipFileEntry;

begin
  if Source is TZipFileEntry then
    begin
    Z:=Source as TZipFileEntry;
    FArchiveFileName:=Z.FArchiveFileName;
    FDiskFileName:=Z.FDiskFileName;
    FSize:=Z.FSize;
    FDateTime:=Z.FDateTime;
    FStream:=Z.FStream;
    FOS:=Z.OS;
    FAttributes:=Z.Attributes;
    end
  else
    inherited Assign(Source);
end;

{ TZipFileEntries }

function TZipFileEntries.GetZ(AIndex : Integer): TZipFileEntry;
begin
  Result:=TZipFileEntry(Items[AIndex]);
end;

procedure TZipFileEntries.SetZ(AIndex : Integer; const AValue: TZipFileEntry);
begin
  Items[AIndex]:=AValue;
end;

function TZipFileEntries.AddFileEntry(const ADiskFileName: String): TZipFileEntry;
begin
  Result:=Add as TZipFileEntry;
  Result.DiskFileName:=ADiskFileName;
end;

function TZipFileEntries.AddFileEntry(const ADiskFileName,
  AArchiveFileName: String): TZipFileEntry;
begin
  Result:=AddFileEntry(ADiskFileName);
  Result.ArchiveFileName:=AArchiveFileName;
end;

function TZipFileEntries.AddFileEntry(const AStream: TSTream;
  const AArchiveFileName: String): TZipFileEntry;
begin
  Result:=Add as TZipFileEntry;
  Result.Stream:=AStream;
  Result.ArchiveFileName:=AArchiveFileName;
end;

Procedure TZipFileEntries.AddFileEntries(Const List : TStrings);

Var
  I : integer;

begin
  For I:=0 to List.Count-1 do
    AddFileEntry(List[i]);
end;
{ TFullZipFileEntries }

function TFullZipFileEntries.GetFZ(AIndex : Integer): TFullZipFileEntry;
begin
  Result:=TFullZipFileEntry(Items[AIndex]);
end;

procedure TFullZipFileEntries.SetFZ(AIndex : Integer;
  const AValue: TFullZipFileEntry);
begin
  Items[AIndex]:=AValue;
end;

End.

