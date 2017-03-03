unit FavoritesDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLiteData, uBaseUnit;

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
  Table.PacketRecords := 1;
  CreateParams :=
    '"websitelink" VARCHAR(3000) NOT NULL PRIMARY KEY,' +
    '"order" INTEGER,' +
    '"website" TEXT,' +
    '"link" TEXT,' +
    '"title" TEXT,' +
    '"currentchapter" TEXT,' +
    '"downloadedchapterlist" TEXT,' +
    '"saveto" TEXT';
  FieldsParams := '"websitelink","order","website","link","title","currentchapter","downloadedchapterlist","saveto"';
  SelectParams := 'SELECT ' + FieldsParams + ' FROM "'+TableName+'" ORDER BY "order"';
end;

function TFavoritesDB.Add(const AOrder: Integer; const AWebsite, ALink, ATitle,
  ACurrentChapter, ADownloadedChapterList, ASaveTo: String): Boolean;
begin
  Result := False;
  if (AWebsite = '') or (ALink = '') then Exit;
  if not Connection.Connected then Exit;
  try
    Connection.ExecuteDirect('INSERT OR REPLACE INTO "favorites" (' +
      FieldsParams +
      ') VALUES ("' +
      LowerCase(AWebsite + ALink) + '","' +
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
    on E: Exception do
      SendLogException(ClassName + '.Add failed!', E);
  end;
end;

procedure TFavoritesDB.Delete(const AWebsite, ALink: String);
begin
  if (AWebsite = '') or (ALink = '') then Exit;
  if not Connection.Connected then Exit;
  try
    Connection.ExecuteDirect(
      'DELETE FROM "favorites" WHERE "websitelink"="' + LowerCase(AWebsite + ALink) + '"');
    Inc(FCommitCount);
    if FCommitCount >= FAutoCommitCount then
      Commit;
  except
    on E: Exception do
      SendLogException(ClassName + '.Delete failed!', E);
  end;
end;

procedure TFavoritesDB.Commit;
begin
  if not Connection.Connected then Exit;
  try
    Transaction.Commit;
    FCommitCount := 0;
  except
    on E: Exception do
      begin
        Transaction.Rollback;
        SendLogException(ClassName + '.Commit failed! Rollback!', E);
      end;
  end;
end;

end.

