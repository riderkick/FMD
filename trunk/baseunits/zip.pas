{
        File: zip.pas
        License: GPLv2
        This unit is part of Free Manga Downloader
}

unit zip;

{$mode delphi}

interface

uses
  Classes, FileUtil, Zipper, baseunit, SysUtils;

type
  TCompress = class
  protected
    list: TStringList;
    procedure   OnFileFound(FileIterator: TFileIterator);
  public
    ext,
    Path: String;
    procedure   Execute;
  end;

implementation

uses
  lazutf8classes;

procedure   TCompress.OnFileFound(FileIterator: TFileIterator);
begin
  list.Add(FileIterator.Filename);
end;

procedure   TCompress.Execute;
var
  s,
  fPath   : String;
  searcher: TFileSearcher;
  Zip     : TZipper;
  i       : Cardinal;
  fstream : TFileStreamUTF8;
begin
  try
   // Path:= FixPath(Path);
    fPath:= FixLastDir(Path);
    RenameFileUTF8(Path, fPath);
    list:= TStringList.Create;
    searcher:= TFileSearcher.Create;
    searcher.OnFileFound:= OnFileFound;
    searcher.Search(fPath, '*.jpg;*.jpeg;*.png;*.gif', FALSE, FALSE);

    if list.Count <> 0 then
    begin
      // norm the list
      //for i:= 0 to list.Count-1 do
      //  list.Strings[i]:= ExtractFileName(list.Strings[i]);
      Zip:= TZipper.Create;
      Zip.FileName:= fPath+ext;
      //SetCurrentDirUTF8(ExtractFileDir(Path+'.zip'));
      for i:= 0 to list.Count-1 do
      begin
        {$IFDEF WINDOWS}
        s:= StringReplace(list.Strings[i], '/', '\', [rfReplaceAll]);
        {$ELSE}
        s:= list.Strings[i];
        {$ENDIF}
        Zip.Entries.AddFileEntry(s, Format('%.3d%s', [i, ExtractFileExt(list.Strings[i])]));
      end;

      fstream:= TFileStreamUTF8.Create(fPath+ext, fmCreate);
     // Zip.ZipAllFiles;
      Zip.SaveToStream(fstream);
      fstream.Free;
      Zip.Free;
      //SetCurrentDirUTF8(oldDir);
      for i:= 0 to list.Count-1 do
        DeleteFileUTF8(list.Strings[i]);
      RemoveDirUTF8(fPath);
      if fPath[Length(fPath)] = '/' then
        Delete(fPath, Length(fPath), 1);
      RenameFileUTF8(fPath+ext, Path+ext);
    end;
  finally
    searcher.Free;
    list.Free;
  end;
end;

end.

