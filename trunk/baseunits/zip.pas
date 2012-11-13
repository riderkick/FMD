{
        File: zip.pas
        License: GPLv3
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
    Path: String;
    procedure   Execute;
  end;

implementation

procedure   TCompress.OnFileFound(FileIterator: TFileIterator);
begin
  list.Add(FileIterator.Filename);
end;

procedure   TCompress.Execute;
var
  fPath   : String;
  searcher: TFileSearcher;
  Zip     : TZipper;
  i       : Cardinal;
begin
  try
   // Path:= FixPath(Path);
    fPath:= FixLastDir(Path);
    RenameFileUTF8(Path, fPath);
    list:= TStringList.Create;
    searcher:= TFileSearcher.Create;
    searcher.OnFileFound:= OnFileFound;
    searcher.Search(fPath, '*.jpg;*.jpeg;*.png;*.gif', FALSE, ';');

    if list.Count <> 0 then
    begin
      // norm the list
      //for i:= 0 to list.Count-1 do
      //  list.Strings[i]:= ExtractFileName(list.Strings[i]);
      Zip:= TZipper.Create;
      Zip.FileName:= fPath+'.zip';
      //SetCurrentDirUTF8(ExtractFileDir(Path+'.zip'));
      for i:= 0 to list.Count-1 do
        Zip.Entries.AddFileEntry(list.Strings[i], Format('%.3d%s', [i, ExtractFileExt(list.Strings[i])]));
      Zip.ZipAllFiles;
      Zip.Free;
      //SetCurrentDirUTF8(oldDir);
      for i:= 0 to list.Count-1 do
        DeleteFileUTF8(list.Strings[i]);
      RemoveDirUTF8(fPath);
      RenameFileUTF8(fPath+'.zip', Path+'.zip');
    end;
  finally
    searcher.Free;
    list.Free;
  end;
end;

end.

