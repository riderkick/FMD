{
        File: img2pdf.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uImg2Pdf;

{$mode delphi}

interface

uses
  Classes, SysUtils, ZStream, FPImage, FPReadJPEG, FPWriteJPEG, Graphics, GraphType,
  ImagingTypes, Imaging, ImagingUtility, lazutf8classes;

const
  TPDFFormatSetings: TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: #0;
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
    LongMonthNames: ('January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December');
    ShortDayNames: ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    LongDayNames: ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday',
    'Friday', 'Saturday');
    TwoDigitYearCenturyWindow: 50;
    );

type
  TPageInfo = record
    fWidth, fHeight: Single;
    imgStream      : TMemoryStream;
    bpc            : Byte;
    cs, f          : String;
  end;

  TImg2Pdf = class
  private
    FBuffer     : TMemoryStream;
    FState      : Integer;
    FCompressionQuality,
    FObjCount,
    FCurrentPage: Cardinal;
    FTitle      : String;
    FPages      : array of String;
    FOffsets    : array of Cardinal;
    FPageInfos  : array of TPageInfo;

    procedure   CreateNewObj;
    procedure   BeginPDF;
    procedure   EndPDF;
    procedure   BeginPDFPage(const AFWidth, AFHeight: Single);
    procedure   EndPDFPage;
    procedure   PDFWrite(AText: String);
    function    PDFString(const AText: String): String;
    procedure   Error(AMsg: String);
    procedure   AddFlateImage(const AName: String);
    procedure   AddDCTImage(const AName: String);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   AddImage(const AName: String);
    procedure   SaveToStream(const AStream: TStream);
    procedure   SaveToFile(const AFile: String);

    property    Title: String read FTitle write FTitle;
    property    CompressionQuality: Cardinal read FCompressionQuality write FCompressionQuality;
  end;

implementation

// private

procedure   TImg2Pdf.CreateNewObj;
begin
  Inc(FObjCount);
  SetLength(FOffsets, Length(FOffsets) + 1);
  FOffsets[FObjCount]:= FBuffer.Size;
  PDFWrite(IntToStr(FObjCount) + ' 0 obj');
end;

procedure   TImg2Pdf.BeginPDF;
begin
  FState:= 0;
  SetLength(FPages, 1);
  SetLength(FPageInfos, 1);
  PDFWrite('%PDF-1.7');
end;

procedure   TImg2Pdf.EndPDF;
var
  vni, vo,
  vnbpal,
  i   : Cardinal;
  vkids,
  data: String;
begin
  for i:= 1 to FCurrentPage do
  begin
    CreateNewObj;

    PDFWrite('<</Type /Page');
    PDFWrite('/Parent 1 0 R');
    PDFWrite('/MediaBox [0 0 ' + FloatToStr(FPageInfos[i].fWidth) + ' ' + FloatToStr(FPageInfos[i].fHeight) + ']');
    PDFWrite('/Resources 2 0 R');
    PDFWrite('/Contents ' + IntToStr(FObjCount + 1) + ' 0 R>>');
    PDFWrite('endobj');

    data:= FPages[i];
    CreateNewObj;
    PDFWrite('<</Length ' + IntToStr(Length(data)) + '>>');
    PDFWrite('stream');
    PDFWrite(data + 'endstream');
    PDFWrite('endobj');
  end;

  vni:= FObjCount;
  for i:= 1 to FCurrentPage do
  begin
    CreateNewObj;
    PDFWrite('<</Type /XObject');
    PDFWrite('/Subtype /Image');
    PDFWrite('/Width ' + FloatToStr(FPageInfos[i].fWidth));
    PDFWrite('/Height ' + FloatToStr(FPageInfos[i].fHeight));
    PDFWrite('/ColorSpace /' + FPageInfos[i].cs);
    PDFWrite('/BitsPerComponent ' + IntToStr(FPageInfos[i].bpc));
    PDFWrite('/Filter /' + FPageInfos[i].f);
    PDFWrite('/Length ' + IntToStr(FPageInfos[i].imgStream.Size) + '>>');
    PDFWrite('stream');
    FPageInfos[i].imgStream.Position := 0;
    FBuffer.CopyFrom(FPageInfos[i].imgStream, FPageInfos[i].imgStream.Size);
    PDFWrite(#10 + 'endstream');
    PDFWrite('endobj');
  end;

  FOffsets[1]:= FBuffer.Size;
  PDFWrite('1 0 obj');
  PDFWrite('<</Type /Pages');
  vkids:= '/Kids [';
  for i:= 0 to FCurrentPage - 1 do
    vkids:= vkids + IntToStr(3 + 2 * i) + ' 0 R ';
  PDFWrite(vkids + ']');
  PDFWrite('/Count ' + IntToStr(FCurrentPage));
  PDFWrite('/MediaBox [0 0 ' + FloatToStr(800) + ' ' + FloatToStr(600) + ']');
  PDFWrite('>>');
  PDFWrite('endobj');

  FOffsets[2]:= FBuffer.Size;
  PDFWrite('2 0 obj');
  PDFWrite('<</ProcSet [/PDF /Text /ImageB /ImageC /ImageI]');
  PDFWrite('/Font <<');
  PDFWrite('>>');
  PDFWrite('/XObject <<');
  vnbpal:= 0;
  for i:= 1 to FCurrentPage do
  begin
    PDFWrite('/I' + IntToStr(i) + ' ' +
      IntToStr(vni + (i) + vnbpal) + ' 0 R');
    if (FPageInfos[i].cs = 'Indexed') then
      vnbpal:= vnbpal + 1;
  end;
  PDFWrite('>>');
  PDFWrite('>>');
  PDFWrite('endobj');

  CreateNewObj;
  PDFWrite('<</Producer (FMD - IMG2PDF)');
  if FTitle <> '' then
    PDFWrite('/Title (' + PDFString(FTitle) + ')');

  PDFWrite('/ModDate (D:' + FormatDateTime('yyyymmddhhnnss', now) +')');
  PDFWrite('/CreationDate (D:' + FormatDateTime('yyyymmddhhnnss', now) +')>>');
  PDFWrite('endobj');

  CreateNewObj;
  PDFWrite('<</Type /Catalog');
  PDFWrite('/OpenAction [3 0 R /FitH null]');
  PDFWrite('/Pages 1 0 R>>');
  PDFWrite('endobj');

  vo:= FBuffer.Size;
  PDFWrite('xref');
  PDFWrite('0 ' + IntToStr(FObjCount + 1));
  PDFWrite('0000000000 65535 f ');
  for i:= 1 to FObjCount do
    PDFWrite(Format('%.10d 00000 n ', [FOffsets[i]], TPDFFormatSetings));
  PDFWrite('trailer');
  PDFWrite('<</Size ' + IntToStr(FObjCount + 1));
  PDFWrite('/Root ' + IntToStr(FObjCount) + ' 0 R');
  PDFWrite('/Info ' + IntToStr(FObjCount - 1) + ' 0 R>>');
  PDFWrite('startxref');
  PDFWrite(IntToStr(vo));

  // TODO: add compress method

  PDFWrite('%%EOF');
end;

procedure   TImg2Pdf.BeginPDFPage(const AFWidth, AFHeight: Single);
begin
  Inc(FCurrentPage);
  SetLength(FPages, Length(FPages) + 1);
  SetLength(FPageInfos, Length(FPageInfos) + 1);

  FPages[FCurrentPage]:= '';
  FState:= 1;

  PDFWrite(FloatToStrF(1, ffNumber, 14, 6, TPDFFormatSetings) +
    ' 0 0 ' + FloatToStrF(1, ffNumber, 14, 6, TPDFFormatSetings) +
    ' 0 ' + FloatToStr(AFHeight) + ' cm');
end;

procedure   TImg2Pdf.EndPDFPage;
begin
  FState:= 0;
end;

procedure   TImg2Pdf.PDFWrite(AText: String);
begin
  if FState = 1 then
    FPages[FCurrentPage]:= FPages[FCurrentPage] + AText + #10
  else
  begin
    AText := AText + #10;
    FBuffer.Write(Pointer(AText)^, Length(AText));
  end;
end;

function    TImg2Pdf.PDFString(const AText: String): String;
begin
  Result:= StringReplace(StringReplace(StringReplace(AText, '\', '\\', [rfReplaceAll]),
    ')', '\)', [rfReplaceAll]), '(', '\(', [rfReplaceAll]);
end;

procedure   TImg2Pdf.Error(AMsg: String);
begin
  raise Exception.Create('IMG2PDF error: ' + AMsg);
end;

// public

constructor TImg2Pdf.Create;
begin
  inherited;

  FTitle      := '';
  FState      := 0;
  FObjCount   := 2;
  FCurrentPage:= 0;
  FCompressionQuality:= 100;
  SetLength(FPages, 0);
  SetLength(FOffsets, 3);
  SetLength(FPageInfos, 0);
  FBuffer:= TMemoryStream.Create;

  BeginPDF;
end;

destructor  TImg2Pdf.Destroy;
var
  i: Cardinal;
begin
  if FCurrentPage > 0 then
    for i:= 1 to FCurrentPage do
      FPageInfos[i].imgStream.Free;
  SetLength(FPageInfos, 0);
  SetLength(FOffsets, 0);
  SetLength(FPages, 0);
  FBuffer.Free;
  inherited;
end;

procedure   TImg2Pdf.AddFlateImage(const AName: String);
var
  stream: TFileStreamUTF8;
  cs    : TCompressionStream;
  ss    : TMemoryStream;
  ext   : String;
  im    : TImageData;

  {$ifdef CPU32}
  procedure Swap(buf: Pointer; size: Cardinal);
  begin
    asm
      mov  esi,buf
      mov  ecx,size
    @loop:
      mov  al,[esi]
      xchg al,[esi+2]
      mov  [esi],al
      add  esi,3
      loop @loop
    end;
  end;
  {$endif}

begin
  ext:= StringReplace(UpperCase(ExtractFileExt(AName)), '.', '', [rfReplaceAll]);
  if (ext = '') then
    Error('File without an extension!');
  try
    stream:= TFileStreamUTF8.Create(AName, fmOpenRead);
    LoadImageFromStream(stream, im);
    if (ext <> 'JPG') AND (ext <> 'JPEG') then
      ConvertImage(im, ifR8G8B8);
    ss:= TMemoryStream.Create;
    BeginPDFPage(im.Width, im.Height);

    {$ifdef CPU32}
    Swap(im.Bits, im.Width*im.Height);
    {$endif}

    cs:= TCompressionStream.Create(TCompressionLevel.clMax, ss);
    cs.Write(im.Bits^, im.Width*im.Height*3);
    cs.Free;

    FPageInfos[FCurrentPage].imgStream:= TMemoryStream.Create;
    ss.SaveToStream(FPageInfos[FCurrentPage].imgStream);

    FPageInfos[FCurrentPage].cs     := 'DeviceRGB';
    FPageInfos[FCurrentPage].fWidth := im.Width;
    FPageInfos[FCurrentPage].fHeight:= im.Height;
    FPageInfos[FCurrentPage].bpc    := 8;
    FPageInfos[FCurrentPage].f      := 'FlateDecode';

    PDFWrite('q ' + FloatToStr(im.Width) + ' 0 0 ' + FloatToStr(im.Height) +
      ' 0 -' + FloatToStr(im.Height) + ' cm /I' +
      IntToStr(FCurrentPage) + ' Do Q');
  finally
    stream.Free;
    ss.Free;
    FreeImage(im);
    EndPDFPage;
  end;
end;

procedure   TImg2Pdf.AddDCTImage(const AName: String);
var
  cr    : TFPCustomImageReader;
  stream: TFileStreamUTF8;
  jw    : TFPWriterJPEG;
  im    : TFPMemoryImage;
  ms    : TMemoryStream;
  imd   : TImageData;
  ext   : string;
begin
  ext:= StringReplace(UpperCase(ExtractFileExt(AName)), '.', '', [rfReplaceAll]);
  if (ext = '') then
    Error('File without an extension!');
  try
    im:= TFPMemoryImage.Create(1, 1);
    jw:= TFPWriterJPEG.Create;
    jw.CompressionQuality:= FCompressionQuality;
    stream:= TFileStreamUTF8.Create(AName, fmOpenRead);

    LoadImageFromStream(stream, imd);
    ConvertImage(imd, ifR8G8B8);
    ms:= TMemoryStream.Create;
    SaveImageToStream('jpg', ms, imd);
    FreeImage(imd);
    cr:= TFPReaderJPEG.Create;
    cr.ImageRead(ms, im);

    BeginPDFPage(im.Width, im.Height);
    FPageInfos[FCurrentPage].imgStream:= TMemoryStream.Create;

    im.SaveToStream(FPageInfos[FCurrentPage].imgStream, jw);

    if (jw.GrayScale) then
      FPageInfos[FCurrentPage].cs   := 'DeviceGray'
    else
      FPageInfos[FCurrentPage].cs   := 'DeviceRGB';
    FPageInfos[FCurrentPage].fWidth := im.Width;
    FPageInfos[FCurrentPage].fHeight:= im.Height;
    FPageInfos[FCurrentPage].bpc    := 8;
    FPageInfos[FCurrentPage].f      := 'DCTDecode';

    PDFWrite('q ' + FloatToStr(im.Width) + ' 0 0 ' + FloatToStr(im.Height) +
      ' 0 -' + FloatToStr(im.Height) + ' cm /I' +
      IntToStr(FCurrentPage) + ' Do Q');
  finally
    ms.Free;
    cr.Free;
    stream.Free;
    im.Free;
    jw.Free;
    EndPDFPage;
  end;
end;

procedure   TImg2Pdf.AddImage(const AName: String);
begin
  if FCompressionQuality = 100 then
    AddFlateImage(AName)
  else
    AddDCTImage(AName);
end;

procedure   TImg2Pdf.SaveToStream(const AStream: TStream);
begin
  // close PDF
  if FCurrentPage = 0 then exit;
  EndPDFPage;
  EndPDF;

  // save to stream
  try
    FBuffer.Position:= 0;
    AStream.CopyFrom(FBuffer, FBuffer.Size);
  finally
  end;
end;

procedure   TImg2Pdf.SaveToFile(const AFile: String);
var
  fstream: TFileStream;
begin
  if FCurrentPage = 0 then exit;
  EndPDFPage;
  EndPDF;

  // save to file
  try
    FBuffer.Position:= 0;
    fstream:= TFileStream.Create(AFile, fmCreate);
    fstream.CopyFrom(FBuffer, FBuffer.Size);
  finally
    fstream.Free;
  end;
end;

end.

