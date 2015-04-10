// *****************************************************************************
//  Title.............. :  GZIP / deflate / inflate Streams with PasZLib
//
//  Modulname ......... :  gziputils.pas
//  Type .............. :  Unit
//  Author ............ :  Udo Schmal
//  Development Status  :  15.12.2014
//  Operating System .. :  Win32/Win64
//  IDE ............... :  Delphi & Lazarus
// *****************************************************************************
unit GZIPUtils;
{$ifdef fpc}
  {$mode objfpc}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils, PasZLib, zbase;

type
  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);

const
  RAW_WBITS = -MAX_WBITS; // deflate raw stream (without any header)

  ZLevels: array[TZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,
    Z_BEST_SPEED,
    Z_DEFAULT_COMPRESSION,
    Z_BEST_COMPRESSION
  );

function deflate(inStream, outStream: TMemoryStream; level: TZCompressionLevel = zcDefault): boolean;
function GZip(inStream, outStream: TMemoryStream; level: TZCompressionLevel = zcDefault): boolean;
function ZLib(inStream, outStream: TMemoryStream; level: TZCompressionLevel = zcDefault): boolean;
function inflate(inStream, OutStream: TMemoryStream): boolean;
function ZUncompressStream(inStream, outStream: TMemoryStream): boolean;

implementation

// deflate raw stream
function deflate(inStream, outStream: TMemoryStream; level: TZCompressionLevel = zcDefault): boolean;
var
  zstream: z_stream;
  headerSize: longword;
begin
  result := false;
  headerSize := outStream.Position;
  zstream.next_in := inStream.Memory;
  zstream.avail_in := inStream.Size;
  outStream.SetSize(headerSize + ((inStream.Size + (inStream.Size div 10) + 12) + 255) and not 255);
  zstream.next_out := outStream.Memory + headerSize;
  zstream.avail_out := outStream.Size - headerSize;
  if paszlib.deflateInit2(zstream, ZLevels[level], Z_DEFLATED, RAW_WBITS, 8, Z_DEFAULT_STRATEGY) < Z_OK then Exit;
  paszlib.deflate(zstream, Z_FINISH);
  result := not (deflateEnd(zstream) < 0);
  outStream.SetSize(zstream.total_out + headerSize);
  outStream.Position := zstream.total_out + headerSize;
end;

function GZip(inStream, outStream: TMemoryStream; level: TZCompressionLevel = zcDefault): boolean;
var crc, size: longword;
begin
  inStream.Position := 0; // goto start of input stream
  outStream.Position := 0; // goto start of output stream
  size := inStream.Size;
  crc := crc32(0, Pointer(inStream.Memory), size);
  outStream.WriteWord($8b1f); //GZip IDentification
  outStream.WriteByte($08); //Compression Method = deflate
  outStream.WriteByte($00); //FLags
  // bit 0   FTEXT - indicates file is ASCII text (can be safely ignored)
  // bit 1   FHCRC - there is a CRC16 for the header immediately following the header
  // bit 2   FEXTRA - extra fields are present
  // bit 3   FNAME - the zero-terminated filename is present. encoding; ISO-8859-1.
  // bit 4   FCOMMENT  - a zero-terminated file comment is present. encoding: ISO-8859-1
  // bit 5   reserved
  // bit 6   reserved
  // bit 7   reserved
  outStream.WriteDWord($00000000); //Modification TIME = no time stamp is available (UNIX time format only make problems)
  outStream.WriteByte($00); //eXtra FLags
  // 00 - default compression
  // 02 - compressor used maximum compression, slowest algorithm
  // 04 - compressor used fastest algorithm
  outStream.WriteByte({$ifdef win32}$0b{$else}$03{$endif}); //Operating System = NTFS filesystem (NT)
  // 00 - FAT filesystem (MS-DOS, OS/2, NT/Win32)
  // 01 - Amiga
  // 02 - VMS (or OpenVMS)
  // 03 - Unix
  // 04 - VM/CMS
  // 05 - Atari TOS
  // 06 - HPFS filesystem (OS/2, NT)
  // 07 - Macintosh
  // 08 - Z-System
  // 09 - CP/M
  // 0A - TOPS-20
  // 0B - NTFS filesystem (NT)
  // 0C - QDOS
  // 0D - Acorn RISCOS
  // FF - unknown
  result := deflate(inStream, outStream, level);
  if result then
  begin
    outStream.WriteDWord(crc); // CRC32 (CRC-32)
    outStream.WriteDWord(size); // ISIZE (Input SIZE)
  end;
  outStream.Position := 0; // goto start of result stream
//  WriteLog('deflateed:'#13#10 + Hexdump(Pointer(outStream.Memory)^, outStream.Size)) ;
//  outStream.Seek(0, soFromBeginning);
end;

function ZLib(inStream, outStream: TMemoryStream; level: TZCompressionLevel = zcDefault): boolean;
var adler: longword;
begin
  inStream.Position := 0; // goto start of input stream
  outStream.Position := 0; // goto start of output stream
  outStream.WriteWord($9c78); //ZLib Header
  adler := adler32(0, Z_NULL, 0);
  adler := adler32(adler, Pointer(inStream.Memory), inStream.Size);
  result := deflate(inStream, outStream, level);
  if result then // add adler32 checksum
    outStream.WriteDWord(SwapEndian(adler)); // adler32 checksum
  outStream.Position := 0; // goto start of result stream
//  WriteLog('deflateed:'#13#10 + Hexdump(Pointer(outStream.Memory)^, outStream.Size)) ;
//  outStream.Seek(0, soFromBeginning);
end;

// inflate raw stream
function inflate(inStream, OutStream: TMemoryStream): boolean;
var
  zstream: z_stream;
  headerSize, delta: longword;
begin
  headerSize := inStream.Position;
  zstream.next_in := inStream.Memory + headerSize;
  zstream.avail_in := inStream.Size - headerSize;
  delta := (inStream.Size + 255) and not 255;
  if outStream.Size = 0 then
    outStream.SetSize(delta);
  zstream.next_out := outStream.Memory;
  zstream.avail_out := outStream.Size;
  if paszlib.inflateInit2(zstream, RAW_WBITS) < 0 then Exit;
  while paszlib.inflate(zstream, Z_NO_FLUSH) = Z_OK do
  begin
    outStream.SetSize(outStream.Size + delta);
    zstream.next_out := outStream.Memory + zstream.total_out;
    zstream.avail_out := delta;
  end;
  result := not (inflateEnd(zstream) < 0);
  outStream.SetSize(zstream.total_out);
end;

function ZUncompressStream(inStream, outStream: TMemoryStream): boolean;
type
  TZStreamType = (
    zsZLib, // standard zlib stream (deflate header)
    zsGZip, // gzip stream (with gzip header)
    zsRaw,  // raw stream (without any header)
    zsNo    // no compression
  );
var
  streamType: TZStreamType;
  hdr, crc, adler, adler32in, crcGZin, sizeGZin: longword;
begin
  result := false;
  inStream.Position := 0; // goto start of input stream
  outStream.Position := 0; // goto start of output stream
  hdr := inStream.ReadDWord;
  if (hdr and $00088B1F) = $00088B1F then // gzip header (deflate method)
  begin
    streamType := zsGZip; // GZIP format
    inStream.Seek(-8, soFromEnd);
    crcGZin := inStream.ReadDWord; // CRC32 (CRC-32)
    sizeGZin := inStream.ReadDWord; // ISIZE (Input SIZE)
    inStream.Size := inStream.Size-8; // cut the 4 byte crc32 and 4 byte input size
    outStream.SetSize(sizeGZin);
    inStream.Position := 10; // jump over header
  end
  else if (hdr and $00009C78) = $00009C78 then // zlib header
  begin
    streamType := zsZLib; // deflate format (with header)
    inStream.Seek(-4, soFromEnd); // first byte is start of deflate header
    adler32in := SwapEndian(inStream.ReadDWord);
    inStream.Size := inStream.Size-4; // cut the 4 byte adler32 code
    outStream.SetSize(0);
    inStream.Position := 2; // jump over header
  end
  else
  begin
    streamType := zsRaw; // deflate format (is without header)
    outStream.SetSize(0);
  end;
  result := inflate(inStream, outStream);
  if result and (streamType = zsGZip) then // can check crc32 and size
  begin
    crc := crc32(0, Pointer(outStream.Memory), outStream.Size); // get result crc32 checksum
    result := (crc = crcGZin) and (outStream.Size = sizeGZin); // compare with input checksum and size
  end
  else if result and (streamType = zsZLib) then // can check adler32 checksum
  begin
    adler := adler32(0, Z_NULL, 0);
    adler := adler32(adler, Pointer(outStream.Memory), outStream.Size);
    result := (adler = adler32in);
  end;
  inStream.Position := 0; // goto start of source stream
  outStream.Position := 0; // goto start of result stream
//  WriteLog(Hexdump(Pointer(outStream.Memory)^, outStream.Size));
//  outStream.Position := 0; // goto start of result stream
end;

end.
