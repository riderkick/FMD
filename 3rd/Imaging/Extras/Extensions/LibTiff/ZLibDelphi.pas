unit ZLibDelphi;

(*
{$IFOPT D+}
{$DEFINE ZLIB_DEBUG}
{$ENDIF}
*)

interface

uses
  Windows, SysUtils;

const

  ZLIB_VERSION = '1.2.1';

  Z_NO_FLUSH = 0;
  Z_FINISH = 4;

  Z_OK = 0;
  Z_STREAM_END = 1;

type

  PRZStream = ^RZStream;

  RZStream = record
    NextIn: PByte;
    AvailIn: Cardinal;
    TotalIn: Cardinal;
    NextOut: PByte;
    AvailOut: Cardinal;
    TotalOut: Cardinal;
    Msg: PChar;
    State: Pointer;
    AllocFunc: Pointer;
    FreeFunc: Pointer;
    Opaque: Cardinal;
    DataType: Integer;
    Adler: Cardinal;
    Reserved: Cardinal;
  end;

function  inflateInit_(strm: Pointer; version: Pointer; stream_size: Integer): Integer; cdecl; external;
function  inflateReset(strm: Pointer): Integer; cdecl; external;
function  inflate(strm: Pointer; flush: Integer): Integer; cdecl; external;
function  inflateSync(strm: Pointer): Integer; cdecl; external;
function  deflateInit(strm: Pointer; level: Integer): Integer;
function  deflateInit_(strm: Pointer; level: Integer; version: Pointer; stream_size: Integer): Integer; cdecl; external;
function  deflateReset(strm: Pointer): Integer; cdecl; external;
function  deflate(strm: Pointer; flush: Integer): Integer; cdecl; external;
function  deflateEnd(strm: Pointer): Integer; cdecl; external;
function  inflateEnd(strm: Pointer): Integer; cdecl; external;
function  deflateParams(strm: Pointer; level: Integer; strategy: Integer): Integer; cdecl; external;

implementation

uses
  LibDelphi;

function deflateInit(strm: Pointer; level: Integer): Integer;
begin
  Result:=deflateInit_(strm,level,PChar(ZLIB_VERSION),SizeOf(RZStream));
end;

{inflate}

{$IFDEF ZLIB_DEBUG}
{$L ZLibDelphi\debug\inflate.obj}
{$ELSE}
{$L ZLibDelphi\release\inflate.obj}
{$ENDIF}

{crc32}

{$IFDEF ZLIB_DEBUG}
{$L ZLibDelphi\debug\crc32.obj}
{$ELSE}
{$L ZLibDelphi\release\crc32.obj}
{$ENDIF}

{adler32}

{$IFDEF ZLIB_DEBUG}
{$L ZLibDelphi\debug\adler32.obj}
{$ELSE}
{$L ZLibDelphi\release\adler32.obj}
{$ENDIF}

{inftrees}

{$IFDEF ZLIB_DEBUG}
{$L ZLibDelphi\debug\inftrees.obj}
{$ELSE}
{$L ZLibDelphi\release\inftrees.obj}
{$ENDIF}

{inffast}

{$IFDEF ZLIB_DEBUG}
{$L ZLibDelphi\debug\inffast.obj}
{$ELSE}
{$L ZLibDelphi\release\inffast.obj}
{$ENDIF}

{deflate}

{$IFDEF ZLIB_DEBUG}
{$L ZLibDelphi\debug\deflate.obj}
{$ELSE}
{$L ZLibDelphi\release\deflate.obj}
{$ENDIF}

{zutil}

{$IFDEF ZLIB_DEBUG}
{$L ZLibDelphi\debug\zutil.obj}
{$ELSE}
{$L ZLibDelphi\release\zutil.obj}
{$ENDIF}

{trees}

{$IFDEF ZLIB_DEBUG}
{$L ZLibDelphi\debug\trees.obj}
{$ELSE}
{$L ZLibDelphi\release\trees.obj}
{$ENDIF}

{compress}

{$IFDEF ZLIB_DEBUG}
{$L ZLibDelphi\debug\compress.obj}
{$ELSE}
{$L ZLibDelphi\release\compress.obj}
{$ENDIF}

{decompress}

{$IFDEF ZLIB_DEBUG}
{$L ZLibDelphi\debug\uncompr.obj}
{$ELSE}
{$L ZLibDelphi\release\uncompr.obj}
{$ENDIF}

end.








