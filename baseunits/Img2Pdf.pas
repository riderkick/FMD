{ Img2Pdf

  Copyright (C) 2016

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit Img2Pdf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8Classes,
  Imaging, ImagingTypes, ImagingExtras, zstream;

type
  TPDFFloat = Single;
  TCompressionQuality = 0..100;

  TImg2PDF = class;

  { TPageInfo }

  TPageInfo = class
  private
    FOwner: TImg2PDF;
  public
    constructor Create(const AOwner: TImg2PDF);
    destructor Destroy; override;
  public
    FileName: String;
    Extension: String;
    Width: Integer;
    Height: Integer;
    ColorSpace: String;
    Filter: String;
    Stream: TMemoryStreamUTF8;
  public
    procedure GetImageSize;
    procedure LoadImageData;
    procedure FlushImageData;
  end;

  TPDFInfos = record
    Title,
    Subject,
    Author,
    Creator,
    Producer,
    Keywords: String;
    CreationDate: TDateTime;
    ModDate: TDateTime;
  end;

  { TImg2PDF }

  TImg2PDF = class
  private
    FPageInfos: TFPList;
    function GetPageInfo(const Index: Integer): TPageInfo;
  public
    constructor Create;
    destructor Destroy; override;
  public
    Infos: TPDFInfos;
    CompressionQuality: TCompressionQuality;
    function AddImage(const AFileName: String): Integer;
    procedure DeleteImage(const Index: Integer);
    procedure SaveToStream(const Stream: TStream);
    procedure SaveToFile(const AFileName: String);
  public
    property PageInfo[const Index: Integer]: TPageInfo read GetPageInfo;
  end;

implementation

const
  CRLF = #13#10;
  PDF_VERSION = '%PDF-1.3';
  PDF_FILE_END = '%%EOF';
  PDF_MAX_GEN_NUM = 65535;

function PDFString(const S: String): String;
begin
  Result := S;
  if Pos(Result, '\') <> -1 then
    Result := StringReplace(Result, '\', '\\', [rfReplaceAll]);
  if Pos(Result, ')') <> -1 then
    Result := StringReplace(Result, ')', '\)', [rfReplaceAll]);
  if Pos(Result, '(') <> -1 then
    Result := StringReplace(Result, '(', '\(', [rfReplaceAll]);
end;

function PDFInt(const I: Integer; PadLength: Integer): String;
begin
  Result := IntToStr(I);
  PadLength := PadLength - Length(Result);
  if PadLength > 0 then
    Result := StringOfChar('0', PadLength) + Result;
end;

{ TPageInfo }

constructor TPageInfo.Create(const AOwner: TImg2PDF);
begin
  FOwner := AOwner;
  FileName := '';
  Extension := '';
  Width := 0;
  Height := 0;
  ColorSpace := '';
  Filter := '';
end;

destructor TPageInfo.Destroy;
begin
  FlushImageData;
  inherited Destroy;
end;

function GetColorSpace(const AImageData: TImageData): String;
begin
  case AImageData.Format of
    ifIndex8: Result := 'Indexed';

    ifGray8,
    ifA8Gray8,
    ifGray16,
    ifGray32,
    ifGray64,
    ifA16Gray16: Result := 'DeviceGray';

    ifX5R1G1B1,
    ifR3G3B2,
    ifR5G6B5,
    ifA1R5G5B5,
    ifA4R4G4B4,
    ifX1R5G5B5,
    ifX4R4G4B4,
    ifR8G8B8,
    ifA8R8G8B8,
    ifX8R8G8B8,
    ifR16G16B16,
    ifA16R16G16B16,
    ifB16G16R16,
    ifA16B16G16R16,
    ifR32F,
    ifA32R32G32B32F,
    ifA32B32G32R32F,
    ifR16F,
    ifA16R16G16B16F,
    ifA16B16G16R16F,
    ifR32G32B32F,
    ifB32G32R32F: Result := 'DeviceRGB';
    else
      Result := '';
  end;
end;

procedure TPageInfo.GetImageSize;
var
  AFS: TFileStreamUTF8;
  AImg: TImageData;
begin
  AFS := TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Extension := LowerCase(DetermineStreamFormat(AFS));
    if Extension = '' then Exit;
    InitImage(AImg);
    try
      if LoadImageFromStream(AFS, AImg) then
      begin
        Width := AImg.Width;
        Height := AImg.Height;
      end;
    finally
      FreeImage(AImg);
    end;
  finally
    AFS.Free;
  end;
end;

procedure TPageInfo.LoadImageData;
var
  AImg: TImageData;
  ADefaultJpegQuality: LongInt;
  AImgInfo: TImageFormatInfo;
  i: Integer;
begin
  if Assigned(Stream) then Exit;
  Stream := TMemoryStreamUTF8.Create;
  try
    Stream.LoadFromFile(FileName);
    InitImage(AImg);
    LoadImageFromStream(Stream, AImg);
    ColorSpace := GetColorSpace(AImg);
    if Extension = 'jpg' then
      Filter := 'DCTDecode'
    else
    begin
      Stream.Clear;
      if FOwner.CompressionQuality < 100 then
      begin
        //DCTDecode for jpg, convert to jpg for non jpg
        Filter := 'DCTDecode';
        ADefaultJpegQuality := Imaging.GetOption(ImagingJpegQuality);
        try
          Imaging.SetOption(ImagingJpegQuality, FOwner.CompressionQuality);
          SaveImageToStream('jpg', Stream, AImg);
        finally
          Imaging.SetOption(ImagingJpegQuality, ADefaultJpegQuality);
        end;
      end
      else
      begin
        //FlateDecode
        Filter := 'FlateDecode';
        GetImageFormatInfo(AImg.Format, AImgInfo);
        if (AImgInfo.IsIndexed) and (AImgInfo.PaletteEntries > 0) then begin
          ColorSpace := 'Indexed /DeviceRGB ' + IntToStr(AImgInfo.PaletteEntries - 1) + ' <';
          for i := 0 to AImgInfo.PaletteEntries - 1 do
            with AImg.Palette^[i] do
              ColorSpace += IntToHex(R, 2) + IntToHex(G, 2) + IntToHex(B, 2);
          ColorSpace += '>';
        end;
        if ColorSpace = 'DeviceRGB' then
          try
            SwapChannels(AImg, ChannelRed, ChannelBlue);
          except
          end;
        with Tcompressionstream.Create(clmax, Stream, False) do
          try
            Write(AImg.Bits^, AImg.Size);
          finally
            Free;
          end;
      end;
    end;
  finally
    FreeImage(AImg);
  end;
end;

procedure TPageInfo.FlushImageData;
begin
  if Assigned(Stream) then
    FreeAndNil(Stream);
end;

{ TImg2PDF }

function TImg2PDF.GetPageInfo(const Index: Integer): TPageInfo;
begin
  Result := TPageInfo(FPageInfos[Index]);
end;

constructor TImg2PDF.Create;
begin
  FPageInfos := TFPList.Create;
  Infos.Title := '';
  Infos.Subject := '';
  Infos.Author := '';
  Infos.Creator := '';
  Infos.Producer := 'Img2Pdf';
  Infos.Keywords := '';
  Infos.CreationDate := Now;
  Infos.ModDate := Now;
  CompressionQuality := 100;
end;

destructor TImg2PDF.Destroy;
begin
  while FPageInfos.Count <> 0 do
    DeleteImage(FPageInfos.Count - 1);
  FPageInfos.Free;
  inherited Destroy;
end;

function TImg2PDF.AddImage(const AFileName: String): Integer;
var
  P: TPageInfo;
begin
  Result := -1;
  if not FileExistsUTF8(AFileName) then Exit;
  P := TPageInfo.Create(Self);
  P.FileName := AFileName;
  P.GetImageSize;
  if P.Extension <> '' then
    Result := FPageInfos.Add(P)
  else
    p.Free;
end;

procedure TImg2PDF.DeleteImage(const Index: Integer);
begin
  TPageInfo(FPageInfos[Index]).Free;
  FPageInfos.Delete(Index);
end;

procedure TImg2PDF.SaveToStream(const Stream: TStream);
var
  PDFBuffer: String;
  AData, vkids: String;
  AObjs: array of Integer;
  AObjCount, i, vnbpal, vni, vo: Integer;

  procedure PDFFlush;
  begin
    if PDFBuffer = '' then Exit;
    Stream.Write(Pointer(PDFBuffer)^, Length(PDFBuffer));
    PDFBuffer := '';
  end;

  procedure PDFWrite(const S: String);
  begin
    PDFBuffer := PDFBuffer + S + CRLF;
  end;

  procedure CreateNewObj;
  begin
    Inc(AObjCount);
    SetLength(AObjs, Length(AObjs) + 1);
    AObjs[AObjCount] := Stream.Size;
    PDFWrite(IntToStr(AObjCount) + ' 0 obj');
  end;

begin
  if FPageInfos.Count = 0 then Exit;

  PDFBuffer := '';
  AObjCount := 2;
  SetLength(AObjs, 3);

  try
    PDFWrite(PDF_VERSION);

    for i := 0 to FPageInfos.Count - 1 do
    begin
      CreateNewObj;
      PDFWrite('<<');
      PDFWrite('/Type /Page');
      PDFWrite('/Parent 1 0 R');
      PDFWrite('/MediaBox [0 0 ' + IntToStr(PageInfo[i].Width) + ' ' + IntToStr(PageInfo[i].Height) + ']');
      PDFWrite('/Resources 2 0 R');
      PDFWrite('/Contents ' + IntToStr(AObjCount + 1) + ' 0 R');
      PDFWrite('>>');
      PDFWrite('endobj');

      CreateNewObj;
      AData := '1 0 0 1 0 ' + IntToStr(PageInfo[i].Height) + ' cm' + CRLF +
        Format('q %d 0 0 %d 0 -%d cm /I%d Do Q', [PageInfo[i].Width, PageInfo[i].Height,
        PageInfo[i].Height, i + 1]);
      PDFWrite('<<');
      PDFWrite('/Length ' + IntToStr(Length(AData)));
      PDFWrite('>>');
      PDFWrite('stream');
      PDFWrite(AData + CRLF + 'endstream');
      PDFWrite('endobj');
    end;
    PDFFlush;

    vni := AObjCount;
    for i := 0 to FPageInfos.Count - 1 do
      try
        PageInfo[i].LoadImageData;
        CreateNewObj;
        PDFWrite('<<');
        PDFWrite('/Type /XObject');
        PDFWrite('/Subtype /Image');
        PDFWrite('/Width ' + IntToStr(PageInfo[i].Width));
        PDFWrite('/Height ' + IntToStr(PageInfo[i].Height));
        PDFWrite('/ColorSpace [/' + PageInfo[i].ColorSpace + ']');
        PDFWrite('/BitsPerComponent 8');
        PDFWrite('/Filter /' + PageInfo[i].Filter);
        PDFWrite('/Length ' + IntToStr(PageInfo[i].Stream.Size));
        PDFWrite('>>');
        PDFWrite('stream');
        PDFFlush;
        PageInfo[i].Stream.Position := 0;
        Stream.CopyFrom(PageInfo[i].Stream, PageInfo[i].Stream.Size);
        PDFWrite(CRLF + 'endstream');
        PDFWrite('endobj');
      finally
        PageInfo[i].FlushImageData;
      end;
    PDFFlush;

    AObjs[1] := Stream.Size;
    PDFWrite('1 0 obj');
    PDFWrite('<<');
    PDFWrite('/Type /Pages');
    vkids := '/Kids [';
    for i := 0 to FPageInfos.Count - 1 do
      vkids := vkids + IntToStr(3 + 2 * i) + ' 0 R ';
    PDFWrite(vkids + ']');
    PDFWrite('/Count ' + IntToStr(FPageInfos.Count));
    PDFWrite('/MediaBox [0 0 800 600]');
    PDFWrite('>>');
    PDFWrite('endobj');

    AObjs[2] := Stream.Size;
    PDFWrite('2 0 obj');
    PDFWrite('<<');
    PDFWrite('/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]');
    PDFWrite('/Font');
    PDFWrite('<<');
    PDFWrite('>>');
    PDFWrite('/XObject');
    PDFWrite('<<');
    vnbpal := 0;
    for i := 1 to FPageInfos.Count do
    begin
      PDFWrite('/I' + IntToStr(i) + ' ' + IntToStr(vni + (i) + vnbpal) + ' 0 R');
      if (PageInfo[i - 1].ColorSpace = 'Indexed') then
        vnbpal := vnbpal + 1;
    end;
    PDFWrite('>>');
    PDFWrite('>>');
    PDFWrite('endobj');

    CreateNewObj;
    PDFWrite('<<');
    PDFWrite('/CreationDate (D:' + FormatDateTime('yyyymmddhhnnss', Infos.CreationDate) + ')');
    PDFWrite('/ModDate (D:' + FormatDateTime('yyyymmddhhnnss', Infos.ModDate) + ')');
    if Infos.Title <> '' then
      PDFWrite('/Title (' + PDFString(Infos.Title) + ')');
    if Infos.Subject <> '' then
      PDFWrite('/Subject (' + PDFString(Infos.Subject) + ')');
    if Infos.Author <> '' then
      PDFWrite('/Author (' + PDFString(Infos.Author) + ')');
    if Infos.Creator <> '' then
      PDFWrite('/Creator (' + PDFString(Infos.Creator) + ')');
    if Infos.Producer <> '' then
      PDFWrite('/Producer (' + PDFString(Infos.Producer) + ')');
    if Infos.Keywords <> '' then
      PDFWrite('/Keywords (' + PDFString(Infos.Keywords) + ')');
    PDFWrite('>>');
    PDFWrite('endobj');

    CreateNewObj;
    PDFWrite('<<');
    PDFWrite('/Type /Catalog');
    PDFWrite('/OpenAction [3 0 R /FitH null]');
    PDFWrite('/Pages 1 0 R');
    PDFWrite('>>');
    PDFWrite('endobj');

    vo := Stream.Size;
    PDFWrite('xref');
    PDFWrite('0 ' + IntToStr(AObjCount + 1));
    PDFWrite('0000000000 ' + PDFInt(PDF_MAX_GEN_NUM, 5) + ' f');
    for i := 1 to AObjCount do
      PDFWrite(PDFInt(AObjs[i], 10) + ' 00000 n');
    PDFWrite('trailer');
    PDFWrite('<<');
    PDFWrite('/Size ' + IntToStr(AObjCount + 1));
    PDFWrite('/Root ' + IntToStr(AObjCount) + ' 0 R');
    PDFWrite('/Info ' + IntToStr(AObjCount - 1) + ' 0 R');
    PDFWrite('>>');
    PDFWrite('startxref');
    PDFWrite(IntToStr(vo));

    PDFWrite(PDF_FILE_END);
    PDFFlush;
  finally
    SetLength(AObjs, 0);
  end;
end;

procedure TImg2PDF.SaveToFile(const AFileName: String);
var
  fs: TFileStreamUTF8;
begin
  if FPageInfos.Count = 0 then Exit;
  fs := TFileStreamUTF8.Create(AFileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

end.
