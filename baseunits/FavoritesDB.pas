unit FavoritesDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLiteData;

type

  { TFavoritesDB }

  TFavoritesDB = class(TSQliteData)
  private
    FCommitCount: Integer;
    FAutoCommitCount: Integer;
    procedure SetAutoCommitCount(AValue: Integer);
  public
    constructor Create(const AFilename: String);
    function Add(const AOrder: Integer; const AWebsite, ALink,
      ATitle, ACurrentChapter, ADownloadedChapterList, ASaveTo: String): Boolean;
    procedure Delete(const AWebsite, ALink: String);
    procedure Commit; override;
    property AutoCommitCount: Integer read FAutoCommitCount write SetAutoCommitCount;
  end;

implementation

{ TFavoritesDB }

procedure TFavoritesDB.SetAutoCommitCount(AValue: Integer);
begin
  if FAutoCommitCount = AValue then Exit;
  FAutoCommitCount := AValue;
end;

constructor TFavoritesDB.Create(const AFilename: String);
begin
  inherited Create;
  FCommitCount := 0;
  FAutoCommitCount := 500;
  Filename := AFilename;
  TableName := 'favorites';
  CreateParams :=
    '"websitelink" VARCHAR(3000) NOT NULL PRIMARY KEY,' +
    '"order" INTEGER,' +
    '"website" TEXT,' +
    '"link" TEXT,' +
    '"title" TEXT,' +
    '"currentchapter" TEXT,' +
    '"downloadedchapterlist" TEXT,' +
    '"saveto" TEXT';
  SelectParams :=
    'SELECT * FROM "favorites" ORDER BY "order"';
end;

function TFavoritesDB.Add(const AOrder: Integer; const AWebsite, ALink, ATitle,
  ACurrentChapter, ADownloadedChapterList, ASaveTo: String): Boolean;
begin
  Result := False;
  if (AWebsite = '') or (ALink = '') then Exit;
  if not Connection.Connected then Exit;
  try
    Connection.ExecuteDirect('INSERT OR REPLACE INTO "favorites" ('+
      '"websitelink","order","website","link","title","currentchapter","downloadedchapterlist","saveto")'+
      ' VALUES ("' +
      AWebsite + ALink + '","' +
      IntToStr(AOrder) + '","' +
      AWebsite + '","' +
      ALink + '","' +
      ATitle + '","' +
      ACurrentChapter  + '","' +
      ADownloadedChapterList + '","' +
      ASaveTo + '")');
    Result := True;
    Inc(FCommitCount);
    if FCommitCount >= FAutoCommitCount then
      Commit;
  except
  end;
end;

procedure TFavoritesDB.Delete(const AWebsite, ALink: String);
begin
  if (AWebsite = '') or (ALink = '') then Exit;
  if not Connection.Connected then Exit;
  try
    Connection.ExecuteDirect(
      'DELETE FROM "favorites" WHERE "websitelink"="' + AWebsite + ALink + '"');
    Inc(FCommitCount);
    if FCommitCount >= FAutoCommitCount then
      Commit;
  except
  end;
end;

procedure TFavoritesDB.Commit;
begin
  if not Connection.Connected then Exit;
  try
    Transaction.Commit;
    FCommitCount := 0;
  except
    Transaction.Rollback;
  end;
end;

end.

