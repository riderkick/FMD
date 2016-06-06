{
        File: uPacker.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uPacker;

{$mode delphi}

interface

uses
  Classes, Zipper, zstream, SysUtils, uBaseUnit, uImg2Pdf, FileUtil, lazutf8classes,
  LazFileUtils, SimpleException, uMisc;

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
      pdf.Title := GetLastDir(Path);
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
begin
  Result:=False;
  Path:=CleanAndExpandDirectory(Path);
  if DirectoryExistsUTF8(Path)=False then Exit;
  FFileList:=TStringList.Create;
  try
    with TFileSearcher.Create do
      try
        OnFileFound:=FileFound;
        Search(Self.Path,'*.jpg;*.jpeg;*.png;*.gif',False,False);
      finally
        Free;
      end;
    if FFileList.Count>0 then begin
      FFileList.CustomSort(NaturalCustomSort);
      case Format of
        pfZIP: FExt:='.zip';
        pfCBZ: FExt:='.cbz';
        pfPDF: FExt:='.pdf';
      end;
      if FileName<>'' then
        FSavedFileName:=FileName+FExt
      else
        FSavedFileName:=TrimAndExpandFilename(Path)+FExt;
      if FileExistsUTF8(FSavedFileName) then
        if DeleteFileUTF8(FSavedFileName)=False then
          Exit;
      case Format of
        pfZIP,pfCBZ: DoZipCbz;
        pfPDF: DoPdf;
      end;
      Result := FileExistsUTF8(FSavedFileName);
      if Result then
        if DeleteDirectory(Path,False) then
          RemoveDirUTF8(Path);
    end;
  finally
    FFileList.Free;
  end;
end;

end.
