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

  TPacker = class
  protected
    FSavedFilename, FExt: String;
    FFileList: TStringList;
    procedure FileFound(FileIterator: TFileIterator);
    procedure DoZipCbz;
    procedure DoPdf;
  public
    Path: String;
    Format: TPackerFormat;
    CompressionQuality: Cardinal;
    procedure Execute;
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
        FileName := FSavedFilename;
        Entries.AddFileEntries(FFileList);
        if Entries.Count > 0 then
          for i := 0 to Entries.Count - 1 do
            Entries[i].CompressionLevel := clnone;
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

      fstream := TFileStreamUTF8.Create(FSavedFilename, fmCreate);
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

procedure TPacker.Execute;
begin
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
      FSavedFilename:=TrimAndExpandFilename(Path)+FExt;
      if FileExistsUTF8(FSavedFilename) then
        if DeleteFileUTF8(FSavedFilename)=False then
          Exit;
      case Format of
        pfZIP,pfCBZ: DoZipCbz;
        pfPDF: DoPdf;
      end;
      if FileExistsUTF8(FSavedFilename) then
        if DeleteDirectory(Path,False) then
          RemoveDirUTF8(Path);
    end;
  finally
    FFileList.Free;
  end;
end;

end.
