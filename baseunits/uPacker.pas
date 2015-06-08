{
        File: uPacker.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uPacker;

{$mode delphi}

interface

uses
  Classes, FileUtil, Zipper, SysUtils, uBaseUnit, uImg2Pdf, USimpleException,
  USimpleLogger;

type
  TPacker = class
  protected
    list: TStringList;
    procedure OnFileFound(FileIterator: TFileIterator);
  public
    ext, Path: String;
    CompressionQuality: Cardinal;
    procedure DoZipCbz;
    procedure DoPdf;
    procedure Execute;
  end;

implementation

uses
  lazutf8classes;

procedure TPacker.OnFileFound(FileIterator: TFileIterator);
begin
  list.Add(FileIterator.Filename);
end;

procedure TPacker.DoZipCbz;
var
  s, fPath: String;
  searcher: TFileSearcher;
  Zip: TZipper;
  i: Cardinal;
  fstream: TFileStreamUTF8;
begin
  try
    fPath := Trim(Path);
    RenameFileUTF8(Path, fPath);
    list := TStringList.Create;
    searcher := TFileSearcher.Create;
    searcher.OnFileFound := OnFileFound;
    searcher.Search(fPath, '*.jpg;*.jpeg;*.png;*.gif;*.db', False, False);

    if list.Count > 0 then
    begin
      Zip := TZipper.Create;
      try
        Zip.FileName := fPath + ext;
        for i := 0 to list.Count - 1 do
        begin
          s := list[i];
          {$IFDEF WINDOWS}
          s := StringReplace(s, '/', '\', [rfReplaceAll]);
          {$ENDIF}
          Zip.Entries.AddFileEntry(TFileStreamUTF8.Create(s, fmOpenRead),
            ExtractFileName(s));
        end;
        fstream := TFileStreamUTF8.Create(fPath + ext, fmCreate);
        try
          Zip.SaveToStream(fstream);
        finally
          fstream.Free;
        end;

        for i := 0 to list.Count - 1 do
        begin
          Zip.Entries[i].Stream.Free;
        end;
      finally
        Zip.Free;
      end;

      //for i := 0 to list.Count - 1 do
      //begin
      //  DeleteFileUTF8(list[i]);
      //end;
      if DeleteDirectory(fPath, False) then
        RemoveDirUTF8(fPath);
      RenameFileUTF8(fPath + ext, Path + ext);
    end;
    searcher.Free;
    list.Free;
  except
    on E: Exception do
    begin
      E.Message := 'DoZipCbz.Exception'#13#10 + E.Message;
      USimpleException.ExceptionHandleSaveLogOnly(Self, E);
    end;
  end;
end;

procedure TPacker.DoPdf;
var
  s, fPath: String;
  searcher: TFileSearcher;
  pdf: TImg2Pdf;
  i: Cardinal;
  fstream: TFileStreamUTF8;
begin
  try
    // Path:= FixPath(Path);
    fPath := Trim(Path);
    RenameFileUTF8(Path, fPath);
    list := TStringList.Create;
    searcher := TFileSearcher.Create;
    searcher.OnFileFound := OnFileFound;
    searcher.Search(fPath, '*.jpg;*.jpeg;*.png;*.gif', False, False);

    if list.Count <> 0 then
    begin
      pdf := TImg2Pdf.Create;
      pdf.CompressionQuality := CompressionQuality;
      pdf.Title := GetLastDir(Path);
      // pdf.FileName:= fPath+ext;
      for i := 0 to list.Count - 1 do
      begin
        s := list[i];
        {$IFDEF WINDOWS}
        s := StringReplace(s, '/', '\', [rfReplaceAll]);
        {$ENDIF}
        // add image to PDF
        try
          pdf.AddImage(s);
        except
        end;
      end;

      fstream := TFileStreamUTF8.Create(fPath + ext, fmCreate);
      pdf.SaveToStream(fstream);
      fstream.Free;
      pdf.Free;
      //searcher.Search(fPath, '*.db', False, False);
      //for i := 0 to list.Count - 1 do
      //  DeleteFileUTF8(list.Strings[i]);
      if DeleteDirectory(fPath, False) then
        RemoveDirUTF8(fPath);
      RenameFileUTF8(fPath + ext, Path + ext);
    end;
    searcher.Free;
    list.Free;
  except
    on E: Exception do
    begin
      E.Message := 'DoPdf.Exception'#13#10 + E.Message;
      USimpleException.ExceptionHandleSaveLogOnly(Self, E);
    end;
  end;
end;

procedure TPacker.Execute;
begin
  if (ext = '.zip') or (ext = '.cbz') then
  begin
    DoZipCbz;
  end
  else
    DoPdf;
end;

end.
