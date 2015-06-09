{
        File: img2pdf.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uImg2Pdf;

{$mode delphi}

interface

uses
  Classes, SysUtils, ZStream, FPImage, FPReadJPEG, FPWriteJPEG,
  ImagingTypes, Imaging, lazutf8classes, USimpleLogger, USimpleException;

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

  { TImg2Pdf }

  TImg2Pdf = class(TObject)
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
    function    GetImageFormat(imData: TImageData): string;
    procedure   SetCompressionQuality(Quality: Cardinal);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   AddImage(const AName: String);
    procedure   SaveToStream(const AStream: TStream);
    procedure   SaveToFile(const AFile: String);

    property    Title: String read FTitle write FTitle;
    property    CompressionQuality: Cardinal read FCompressionQuality write SetCompressionQuality;
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
  i   : Integer;
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
    PDFWrite('/Contents ' + IntToStr(QWord(FObjCount) + 1) + ' 0 R>>');
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
  PDFWrite('0 ' + IntToStr(QWord(FObjCount) + 1));
  PDFWrite('0000000000 65535 f ');
  for i:= 1 to FObjCount do
    PDFWrite(Format('%.10d 00000 n ', [FOffsets[i]], TPDFFormatSetings));
  PDFWrite('trailer');
  PDFWrite('<</Size ' + IntToStr(QWord(FObjCount) + 1));
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
  Imaging.SetOption(ImagingJpegProgressive, 1);
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
  fs  : TFileStreamUTF8;
  ext : String;
  im  : TImageData;
begin
  ext:= StringReplace(UpperCase(ExtractFileExt(AName)), '.', '', [rfReplaceAll]);
  if (ext = '') then
    Error('File without an extension!');

  Initialize(im);
  fs := TFileStreamUTF8.Create(AName, fmOpenRead);
  try
    LoadImageFromStream(fs, im);
    if not Assigned(im.Bits) then Exit;

    BeginPDFPage(im.Width, im.Height);
    FPageInfos[FCurrentPage].imgStream:= TMemoryStream.Create;
    try
      FPageInfos[FCurrentPage].cs     := GetImageFormat(im);
      FPageInfos[FCurrentPage].fWidth := im.Width;
      FPageInfos[FCurrentPage].fHeight:= im.Height;
      FPageInfos[FCurrentPage].bpc    := 8;
      PDFWrite('q ' + FloatToStr(im.Width) + ' 0 0 ' + FloatToStr(im.Height) +
        ' 0 -' + FloatToStr(im.Height) + ' cm /I' +
        IntToStr(FCurrentPage) + ' Do Q');

      if (ext = 'JPG') or (ext = 'JPEG') then
      begin
        FPageInfos[FCurrentPage].f := 'DCTDecode';
        FPageInfos[FCurrentPage].imgStream.CopyFrom(fs, 0);
      end
      else
      begin
        FPageInfos[FCurrentPage].f := 'FlateDecode';
        if GetImageFormat(im) = 'DeviceRGB' then try
          SwapChannels(im, ChannelRed, ChannelBlue);
        except
        end;
        FPageInfos[FCurrentPage].imgStream.Position := 0;
        with Tcompressionstream.create(clmax, FPageInfos[FCurrentPage].imgStream) do try
          write(im.Bits^, im.Size);
        finally
          Free;
        end;
      end;
    finally
      EndPDFPage;
    end;
  except
    on E :Exception do begin
      WriteLog_E('TImg2Pdf.AddFlateImage.Error, '+E.Message);
      USimpleException.ExceptionHandleSaveLogOnly(Self, E);
    end;
  end;
  fs.Free;
  FreeImage(im);
end;

procedure   TImg2Pdf.AddDCTImage(const AName: String);
var
  fs  : TFileStreamUTF8;
  im  : TImageData;
  ext : string;
begin
  ext := StringReplace(UpperCase(ExtractFileExt(AName)), '.', '', [rfReplaceAll]);
  if (ext = '') then
    Error('File without an extension!');

  Initialize(im);
  fs:= TFileStreamUTF8.Create(AName, fmOpenRead);
  try
    LoadImageFromStream(fs, im);
  finally
    fs.Free;
  end;
  if not Assigned(im.Bits) then Exit;

  BeginPDFPage(im.Width, im.Height);
  FPageInfos[FCurrentPage].imgStream := TMemoryStream.Create;
  try
    FPageInfos[FCurrentPage].cs     := GetImageFormat(im);
    FPageInfos[FCurrentPage].fWidth := im.Width;
    FPageInfos[FCurrentPage].fHeight:= im.Height;
    FPageInfos[FCurrentPage].bpc    := 8;
    FPageInfos[FCurrentPage].f      := 'DCTDecode';
    PDFWrite('q ' + FloatToStr(im.Width) + ' 0 0 ' + FloatToStr(im.Height) +
      ' 0 -' + FloatToStr(im.Height) + ' cm /I' +
      IntToStr(FCurrentPage) + ' Do Q');

    SaveImageToStream('jpg', FPageInfos[FCurrentPage].imgStream, im);
  except
    on E :Exception do begin
      WriteLog_E('TImg2Pdf.AddCDTImage.Error, '+E.Message);
      USimpleException.ExceptionHandleSaveLogOnly(Self, E);
    end;
  end;
  EndPDFPage;
  FreeImage(im);
end;

function TImg2Pdf.GetImageFormat(imData: TImageData): string;
begin
  case imData.Format of
    ifGray8,
    ifA8Gray8,
    ifGray16,
    ifGray32,
    ifGray64,
    ifA16Gray16: Result := 'DeviceGray';

    ifUnknown,
    ifDefault,
    ifIndex8,
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
      Result := 'DeviceCMYK';
  end;
end;

procedure TImg2Pdf.SetCompressionQuality(Quality: Cardinal);
begin
  FCompressionQuality := Quality;
  Imaging.SetOption(ImagingJpegQuality, FCompressionQuality);
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
  fstream: TFileStreamUTF8;
begin
  if FCurrentPage = 0 then exit;
  EndPDFPage;
  EndPDF;

  // save to file
  try
    FBuffer.Position:= 0;
    fstream:= TFileStreamUTF8.Create(AFile, fmCreate);
    fstream.CopyFrom(FBuffer, FBuffer.Size);
  finally
    fstream.Free;
  end;
end;

end.

