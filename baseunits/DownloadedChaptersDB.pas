unit DownloadedChaptersDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLiteData, LazFileUtils;

type

  { TDownloadedChaptersDB }

  TDownloadedChaptersDB = class(TSQliteData)
  private
    locklocate: TRTLCriticalSection;
    function GetChapters(const AWebsiteLink: String): String;
    procedure SetChapters(const AWebsiteLink: String; AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Delete(const AWebsiteLink: String);
    property Chapters[const AWebsiteLink: String]: String read GetChapters write SetChapters;
    function ImportFromIni(const AFilename: String): Boolean;
  end;

implementation

function CleanStr(const S: String): String;
begin
  Result := LowerCase(Trim(S));
  if Pos(' ', Result) > 0 then
    Result := StringReplace(Result, ' ', '', [rfReplaceAll]);
  while Pos(LineEnding + LineEnding, Result) > 0 do
    Result := StringReplace(Result, LineEnding + LineEnding, LineEnding, [rfReplaceAll]);
end;

{ TDownloadedChaptersDB }

function TDownloadedChaptersDB.GetChapters(const AWebsiteLink: String): String;
begin
  Result := '';
  if AWebsiteLink = '' then Exit;
  if not Connected then Exit;
  EnterCriticalsection(locklocate);
  with Table do
    try
      if Locate('websitelink', LowerCase(AWebsiteLink), []) then
        Result := Fields[1].AsString;
    finally
      LeaveCriticalsection(locklocate);
    end;
end;

procedure TDownloadedChaptersDB.SetChapters(const AWebsiteLink: String; AValue: String);
var
  dc, ds: TStringList;
  c, s: String;
  i: Integer;
begin
  if AWebsiteLink = '' then Exit;
  if AValue = '' then Exit;
  if not Connected then Exit;
  EnterCriticalsection(locklocate);
  with Table do
    try
      if Locate('websitelink', LowerCase(AWebsiteLink), []) then
      begin
        c := CleanStr(AValue);
        s := Fields[1].AsString;
        if c = s then Exit;
        dc := TStringList.Create;
        ds := TStringList.Create;
        try
          dc.AddText(c);
          ds.Sorted := True;
          ds.Duplicates := dupIgnore;
          ds.AddText(s);
          for i := 0 to dc.Count - 1 do
          begin
            dc[i] := Trim(dc[i]);
            if dc[i] <> '' then
              ds.Add(dc[i]);
          end;
          Edit;
          try
            Fields[1].AsString := ds.Text;
            Post;
          except
            CancelUpdates;
          end;
        finally
          dc.Free;
          ds.Free;
        end;
      end
      else
      begin
        Append;
        try
          Fields[0].AsString := LowerCase(AWebsiteLink);
          Fields[1].AsString := CleanStr(AValue);
          Post;
          IncRecordCount;
        except
          CancelUpdates;
        end;
      end;
    finally
      LeaveCriticalsection(locklocate);
    end;
end;

constructor TDownloadedChaptersDB.Create;
begin
  inherited Create;
  InitCriticalSection(locklocate);
  AutoApplyUpdates := True;
  TableName := 'downloadedchapters';
  CreateParams :=
    'websitelink VARCHAR(3000) NOT NULL PRIMARY KEY,' +
    'chapters TEXT';
end;

destructor TDownloadedChaptersDB.Destroy;
begin
  inherited Destroy;
  DoneCriticalsection(locklocate);
end;

procedure TDownloadedChaptersDB.Delete(const AWebsiteLink: String);
begin
  if not Connected then Exit;
  EnterCriticalsection(locklocate);
  with Table do
    try
      if Locate('websitelink', LowerCase(AWebsiteLink), []) then
        Delete;
    finally
      LeaveCriticalsection(locklocate);
    end;
end;

function TDownloadedChaptersDB.ImportFromIni(const AFilename: String): Boolean;
var
  dc: TStringList;
  i: Integer;
begin
  Result := False;
  if not Connected then Exit;
  if not FileExistsUTF8(AFilename) then Exit;
  dc := TStringList.Create;
  try
    dc.LoadFromFile(AFilename);
    if dc.Count > 0 then
      i := 0;
    while i < dc.Count - 2 do
    begin
      Chapters[dc[i]] := StringReplace(dc[i + 1], '!%~', LineEnding, [rfReplaceAll]);
      Inc(i, 2);
    end;
    Result := True;
  finally
    dc.Free;
  end;
end;

end.

