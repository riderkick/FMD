{
        File: zip.pas
        License: GPL/LGPL
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
  searcher: TFileSearcher;
  Zip     : TZipper;
  i       : Cardinal;
begin
  try
    list:= TStringList.Create;
    searcher:= TFileSearcher.Create;
    searcher.OnFileFound:= OnFileFound;
    searcher.Search(Path, '*.jpg;*.jpeg;*.png;*.gif', FALSE, ';');
    if list.Count <> 0 then
    begin
      Zip:= TZipper.Create;
      Zip.FileName:= Path+'.zip';
      for i:= 0 to list.Count-1 do
        Zip.Entries.AddFileEntry(list.Strings[i], Format('%.3d%s', [i, ExtractFileExt(list.Strings[i])]));
      Zip.ZipAllFiles;
      Zip.Free;
      for i:= 0 to list.Count-1 do
        DeleteFile(list.Strings[i]);
      RemoveDir(Path);
    end;
  finally
    searcher.Free;
    list.Free;
  end;
end;

end.

