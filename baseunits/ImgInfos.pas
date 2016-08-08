{ ImgInfos.pas

  Copyright (C) 2016

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit ImgInfos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8Classes, LazFileUtils, FPimage,
  FPReadJPEG, FPReadPNG, FPReadGif, FPReadBMP, FPReadTiff,
  FPWriteJPEG, FPWritePNG, FPWriteBMP, FPWriteTiff;

type
  PImageHandlerRec = ^TImageHandlerRec;
  TImageHandlerRec = record
    Ext: String;
    WExt: String;
    ReaderClass: TFPCustomImageReaderClass;
    WriterClass: TFPCustomImageWriterClass;
    Reader: TFPCustomImageReader
  end;

  { TimageHandlerMgr }

  TimageHandlerMgr = class
  private
    FList: array of TImageHandlerRec;
    FEmptyHandlerRec: TImageHandlerRec;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ReaderClass: TFPCustomImageReaderClass; const WriterClass: TFPCustomImageWriterClass;
      const Ext: String; WExt: String = '');
    function GetImageHandlerByStream(const Stream: TStream): PImageHandlerRec;
    function GetImageHandlerByFile(const FileName: String): PImageHandlerRec;
    function GetImageHandlerByExt(const Ext: String): PImageHandlerRec;
    function GetImageStreamExt(const Stream: TStream): String; inline;
    function GetImageFileExt(const FileName: String): String; inline;
    function GetImageStreamSize(const Stream: TStream; out Width, Height: Integer): String;
    function GetImageFileSize(const FileName: String; out Width, Height: Integer): String;
    function GetImageStreamReaderClass(const Stream: TStream): TFPCustomImageReaderClass; inline;
    function GetImageFileReaderClass(const FileName: String): TFPCustomImageReaderClass; inline;
    function GetImageExtReaderClass(const Ext: String): TFPCustomImageReaderClass; inline;
    function GetImageStreamWriterClass(const Stream: TStream): TFPCustomImageWriterClass; inline;
    function GetImageFileWriterClass(const FileName: String): TFPCustomImageWriterClass; inline;
    function GetImageExtWriterClass(const Ext: String): TFPCustomImageWriterClass; inline;
    function GetImageWriterExt(const Ext: String): String; inline;
  public
    property Count: Integer read GetCount;
  end;

function GetImageHandlerByStream(const Stream: TStream): TImageHandlerRec; inline;
function GetImageHandlerByFile(const FileName: String): TImageHandlerRec; inline;
function GetImageHandlerByExt(const Ext: String): TImageHandlerRec; inline;
function GetImageStreamExt(const Stream: TStream): String; inline;
function GetImageFileExt(const FileName: String): String; inline;
function GetImageStreamSize(const Stream: TStream; out Width, Height: Integer): String; inline;
function GetImageFileSize(const FileName: String; out Width, Height: Integer): String; inline;
function GetImageStreamReaderClass(const Stream: TStream): TFPCustomImageReaderClass; inline;
function GetImageFileReaderClass(const FileName: String): TFPCustomImageReaderClass; inline;
function GetImageExtReaderClass(const Ext: String): TFPCustomImageReaderClass; inline;
function GetImageStreamWriterClass(const Stream: TStream): TFPCustomImageWriterClass; inline;
function GetImageFileWriterClass(const FileName: String): TFPCustomImageWriterClass; inline;
function GetImageExtWriterClass(const Ext: String): TFPCustomImageWriterClass; inline;
function GetImageWriterExt(const Ext: String): String; inline;

var
  ImageHandlerMgr: TimageHandlerMgr;

implementation

function GetImageHandlerByStream(const Stream: TStream): TImageHandlerRec;
begin
  Result := ImageHandlerMgr.GetImageHandlerByStream(Stream)^;
end;

function GetImageHandlerByFile(const FileName: String): TImageHandlerRec;
begin
  Result := ImageHandlerMgr.GetImageHandlerByFile(FileName)^;
end;

function GetImageHandlerByExt(const Ext: String): TImageHandlerRec;
begin
  Result := ImageHandlerMgr.GetImageHandlerByExt(Ext)^;
end;

function GetImageStreamExt(const Stream: TStream): String;
begin
  Result := ImageHandlerMgr.GetImageStreamExt(Stream);
end;

function GetImageFileExt(const FileName: String): String;
begin
  Result := ImageHandlerMgr.GetImageFileExt(FileName);
end;

function GetImageStreamSize(const Stream: TStream; out Width, Height: Integer): String;
begin
  Result := ImageHandlerMgr.GetImageStreamSize(Stream, Width, Height);
end;

function GetImageFileSize(const FileName: String; out Width, Height: Integer): String;
begin
  Result := ImageHandlerMgr.GetImageFileSize(FileName, Width, Height);
end;

function GetImageStreamReaderClass(const Stream: TStream): TFPCustomImageReaderClass;
begin
  Result := ImageHandlerMgr.GetImageStreamReaderClass(Stream);
end;

function GetImageFileReaderClass(const FileName: String): TFPCustomImageReaderClass;
begin
  Result := ImageHandlerMgr.GetImageFileReaderClass(FileName);
end;

function GetImageExtReaderClass(const Ext: String): TFPCustomImageReaderClass;
begin
  Result := ImageHandlerMgr.GetImageExtReaderClass(Ext);
end;

function GetImageStreamWriterClass(const Stream: TStream): TFPCustomImageWriterClass;
begin
  Result := ImageHandlerMgr.GetImageStreamWriterClass(Stream);
end;

function GetImageFileWriterClass(const FileName: String): TFPCustomImageWriterClass;
begin
  Result := GetImageFileWriterClass(FileName);
end;

function GetImageExtWriterClass(const Ext: String): TFPCustomImageWriterClass;
begin
  Result := GetImageExtWriterClass(Ext);
end;

function GetImageWriterExt(const Ext: String): String;
begin
  Result := GetImageWriterExt(Ext);
end;

{ TimageHandlerMgr }

function TimageHandlerMgr.GetCount: Integer;
begin
  Result := Length(FList);
end;

constructor TimageHandlerMgr.Create;
begin
  FillChar(FEmptyHandlerRec, SizeOf(FEmptyHandlerRec), 0);
end;

destructor TimageHandlerMgr.Destroy;
var
  i: Integer;
begin
  if Length(FList) <> 0 then
    for i := High(FList) downto Low(FList) do
      Flist[i].Reader.Free;
  SetLength(Flist, 0);
  inherited Destroy;
end;

procedure TimageHandlerMgr.Add(const ReaderClass: TFPCustomImageReaderClass;
  const WriterClass: TFPCustomImageWriterClass; const Ext: String; WExt: String);
var
  i: Integer;
begin
  i := Length(FList);
  SetLength(FList, i + 1);
  FList[i].ReaderClass := ReaderClass;
  FList[i].WriterClass := WriterClass;
  FList[i].Reader := ReaderClass.Create;
  FList[i].Ext := Ext;
  if WExt <> '' then
    FList[i].WExt := WExt
  else
    FList[i].WExt := Ext;
end;

function TimageHandlerMgr.GetImageHandlerByStream(const Stream: TStream): PImageHandlerRec;
var
  P: Int64;
  i: Integer;
begin
  Result := @FEmptyHandlerRec;
  if Stream = nil then Exit;
  if Stream.Size = 0 then Exit;
  P := Stream.Position;
  try
    for i := 0 to Length(Flist) - 1 do
    begin
      Stream.Position := 0;
      if FList[i].Reader.CheckContents(Stream) then
      begin
        Result := @FList[i];
        Break;
      end;
    end;
  finally
    Stream.Position := P;
  end;
end;

function TimageHandlerMgr.GetImageHandlerByFile(const FileName: String): PImageHandlerRec;
var
  FS: TFileStreamUTF8;
begin
  Result := @FEmptyHandlerRec;
  if not FileExistsUTF8(FileName) then Exit;
  try
    FS := TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyWrite);
    Result := GetImageHandlerByStream(FS);
  finally
    FS.Free;
  end;
end;

function TimageHandlerMgr.GetImageHandlerByExt(const Ext: String): PImageHandlerRec;
var
  i: Integer;
begin
  Result := @FEmptyHandlerRec;
  for i := Low(FList) to High(Flist) do
    if Ext = FList[i].Ext then
    begin
      Result := @FList[i];
      Break;
    end;
end;

function TimageHandlerMgr.GetImageStreamExt(const Stream: TStream): String;
begin
  Result := GetImageHandlerByStream(Stream)^.Ext;
end;

function TimageHandlerMgr.GetImageFileExt(const FileName: String): String;
begin
  Result := GetImageHandlerByFile(FileName)^.Ext;
end;

function TimageHandlerMgr.GetImageStreamSize(const Stream: TStream; out Width,
  Height: Integer): String;
var
  H: PImageHandlerRec;
  S: TPoint;
begin
  Width := 0;
  Height := 0;
  H := GetImageHandlerByStream(Stream);
  Result := H^.Ext;
  if Assigned(H^.Reader) then
  begin
    S := H^.Reader.ImageSize(Stream);
    Width := S.x;
    Height := S.y;
  end;
end;

function TimageHandlerMgr.GetImageFileSize(const FileName: String; out Width,
  Height: Integer): String;
var
  FS: TFileStreamUTF8;
begin
  Result := '';
  Width := 0;
  Height := 0;
  if not FileExistsUTF8(FileName) then Exit;
  try
    FS := TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyWrite);
    Result := GetImageStreamSize(FS, Width, Height);
  finally
    FS.Free;
  end;
end;

function TimageHandlerMgr.GetImageStreamReaderClass(const Stream: TStream
  ): TFPCustomImageReaderClass;
begin
  Result := GetImageHandlerByStream(Stream)^.ReaderClass;
end;

function TimageHandlerMgr.GetImageFileReaderClass(const FileName: String
  ): TFPCustomImageReaderClass;
begin
  Result := GetImageHandlerByFile(FileName)^.ReaderClass;
end;

function TimageHandlerMgr.GetImageExtReaderClass(const Ext: String
  ): TFPCustomImageReaderClass;
begin
  Result := GetImageHandlerByExt(Ext)^.ReaderClass;
end;

function TimageHandlerMgr.GetImageStreamWriterClass(const Stream: TStream
  ): TFPCustomImageWriterClass;
begin
  Result := GetImageHandlerByStream(Stream)^.WriterClass;
end;

function TimageHandlerMgr.GetImageFileWriterClass(const FileName: String
  ): TFPCustomImageWriterClass;
begin
  Result := GetImageHandlerByFile(FileName)^.WriterClass;
end;

function TimageHandlerMgr.GetImageExtWriterClass(const Ext: String
  ): TFPCustomImageWriterClass;
begin
  Result := GetImageHandlerByExt(Ext)^.WriterClass;
end;

function TimageHandlerMgr.GetImageWriterExt(const Ext: String): String;
begin
  Result := GetImageHandlerByExt(Ext)^.WExt;
end;

initialization
  ImageHandlerMgr := TimageHandlerMgr.Create;
  ImageHandlerMgr.Add(TFPReaderJPEG, TFPWriterJPEG, 'jpg');
  ImageHandlerMgr.Add(TFPReaderPNG, TFPWriterPNG, 'png');
  ImageHandlerMgr.Add(TFPReaderGif, TFPWriterPNG, 'gif', 'png');
  ImageHandlerMgr.Add(TFPReaderBMP, TFPWriterBMP, 'bmp');
  ImageHandlerMgr.Add(TFPReaderTiff, TFPWriterTiff, 'tif');

finalization
  ImageHandlerMgr.Free;

end.
