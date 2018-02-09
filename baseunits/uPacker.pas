{
        File: uPacker.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uPacker;

{$mode delphi}

interface

uses
  Classes, Zipper, zstream, SysUtils, uBaseUnit, Img2Pdf, FileUtil, lazutf8classes,
  LazFileUtils, SimpleException;

type
  TPackerFormat = (pfZIP, pfCBZ, pfPDF);

  { TPacker }

  TPacker = class
  protected
    FSavedFileName, FExt: String;
    FFileList: TStringList;
    procedure FileFound(FileIterator: TFileIterator);
    procedure DoZipCbz;
    procedure DoPdf;
  public
    Path,
    FileName: String;
    Format: TPackerFormat;
    CompressionQuality: Cardinal;
    function Execute: Boolean;
    property FileList: TStringList read FFileList;
    property SavedFileName: String read FSavedFileName;
    property Ext: String read FExt;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

procedure TPacker.FileFound(FileIterator: TFileIterator);
begin
  FFileList.Add(FileIterator.Filename);
end;

procedure TPacker.DoZipCbz;
var
  i: Integer;
begin
  with TZipper.Create do
    try
      try
        FileName := FSavedFileName;
        for i := 0 to FFileList.Count - 1 do
          with Entries.AddFileEntry(FFileList[i]) do
          begin
            CompressionLevel := clnone;
            ArchiveFileName := ExtractFileName(FFileList[i]);
          end;
        ZipAllFiles;
      except
        on E: Exception do
        begin
          E.Message := 'DoZipCbz.Exception'#13#10 + E.Message;
          SimpleException.ExceptionHandleSaveLogOnly(Self, E);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TPacker.DoPdf;
var
  pdf: TImg2Pdf;
  i: Cardinal;
  fstream: TFileStreamUTF8;
begin
  try
    pdf := TImg2Pdf.Create;
    try
      pdf.CompressionQuality := CompressionQuality;
      pdf.Infos.Title := GetLastDir(Path);
      pdf.Infos.Creator := ApplicationName;
      for i := 0 to FFileList.Count - 1 do
      begin
        try
          pdf.AddImage(FFileList[i]);
        except
        end;
      end;

      fstream := TFileStreamUTF8.Create(FSavedFileName, fmCreate);
      try
        pdf.SaveToStream(fstream);
      finally
        fstream.Free;
      end;
    finally
      pdf.Free;
    end;
  except
    on E: Exception do
    begin
      E.Message := 'DoPdf.Exception'#13#10 + E.Message;
      SimpleException.ExceptionHandleSaveLogOnly(Self, E);
    end;
  end;
end;

function TPacker.Execute: Boolean;
var
  i: Integer;
begin
  Result := False;
  Path := CorrectPathSys(Path);

  if FFileList.Count = 0 then
  begin
    if DirectoryExistsUTF8(Path) = False then Exit;
    with TFileSearcher.Create do
      try
        OnFileFound := FileFound;
        Search(Self.Path, '*.jpg;*.png;*.gif;*.webp', False, False);
      finally
        Free;
      end;
  end;

  if FFileList.Count = 0 then Exit;

  FFileList.CustomSort(NaturalCustomSort);
  case Format of
    pfZIP: FExt := '.zip';
    pfCBZ: FExt := '.cbz';
    pfPDF: FExt := '.pdf';
  end;
  if FileName <> '' then
    FSavedFileName := FileName + FExt
  else
    FSavedFileName := TrimAndExpandFilename(Path) + FExt;
  if FileExistsUTF8(FSavedFileName) then
    if DeleteFileUTF8(FSavedFileName) = False then
      Exit;
  case Format of
    pfZIP, pfCBZ: DoZipCbz;
    pfPDF: DoPdf;
  end;
  Result := FileExistsUTF8(FSavedFileName);
  if Result then
  begin
    for i := 0 to FFileList.Count - 1 do
      DeleteFileUTF8(FFileList[i]);
    if IsDirectoryEmpty(Path) then
      RemoveDirUTF8(Path);
  end;
end;

constructor TPacker.Create;
begin
  FFileList := TStringList.Create;
  FSavedFileName := '';
  FExt := '';
  Path := '';
  FileName := '';
  Format := pfZIP;
end;

destructor TPacker.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

end.
