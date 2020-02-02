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
    procedure InternalUpdate(const AOrder:Integer;const AEnabled:Boolean;
      const AWebsite,ALink,ATitle,ACurrentChapter,ADownloadedChapterList,ASaveTo: String); inline;
    procedure InternalAdd(const AOrder:Integer;const AEnabled:Boolean;
      const AWebsite,ALink,ATitle,ACurrentChapter,ADownloadedChapterList,ASaveTo: String); inline;
    function Add(const AOrder:Integer;const AEnabled:Boolean;
      const AWebsite,ALink,ATitle,ACurrentChapter,ADownloadedChapterList,ASaveTo:String):Boolean;
    procedure Delete(const AWebsite, ALink: String);
    procedure Commit; override;
    procedure Close; override;
    property AutoCommitCount: Integer read FAutoCommitCount write SetAutoCommitCount;
  end;

const
  f_websitelink            = 0;
  f_order                  = 1;
  f_enabled                = 2;
  f_website                = 3;
  f_link                   = 4;
  f_title                  = 5;
  f_currentchapter         = 6;
  f_downloadedchapterlist  = 7;
  f_saveto                 = 8;

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
    '"enabled" BOOLEAN,' +
    '"website" TEXT,' +
    '"link" TEXT,' +
    '"title" TEXT,' +
    '"currentchapter" TEXT,' +
    '"downloadedchapterlist" TEXT,' +
    '"saveto" TEXT';
  FieldsParams := '"websitelink","order","enabled","website","link","title","currentchapter","downloadedchapterlist","saveto"';
  SelectParams := 'SELECT * FROM ' + QuotedStrD(TableName) + ' ORDER BY "order"';
end;

procedure TFavoritesDB.InternalUpdate(const AOrder:Integer;const AEnabled:Boolean;
  const AWebsite,ALink,ATitle,ACurrentChapter,ADownloadedChapterList,ASaveTo:String);
begin
  Connection.ExecuteDirect('UPDATE "favorites" SET ' +
    '"order"='+QuotedStr(AOrder)+', '+
    '"enabled"='+QuotedStr(AEnabled)+', '+
    '"title"='+QuotedStr(ATitle)+', '+
    '"currentchapter"='+QuotedStr(ACurrentChapter)+', '+
    '"downloadedchapterlist"='+QuotedStr(ADownloadedChapterList)+', '+
    '"saveto"='+QuotedStr(ASaveTo)+
    ' WHERE "websitelink"='+QuotedStr(LowerCase(AWebsite+ALink)));
end;

procedure TFavoritesDB.InternalAdd(const AOrder:Integer;const AEnabled:Boolean;
  const AWebsite,ALink,ATitle,ACurrentChapter,ADownloadedChapterList,ASaveTo:String);
begin
  Connection.ExecuteDirect('INSERT OR REPLACE INTO "favorites" (' +
    FieldsParams +
    ') VALUES (' +
    QuotedStr(LowerCase(AWebsite + ALink)) + ', ' +
    QuotedStr(AOrder) + ', ' +
    QuotedStr(AEnabled) + ', ' +
    QuotedStr(AWebsite) + ', ' +
    QuotedStr(ALink) + ', ' +
    QuotedStr(ATitle) + ', ' +
    QuotedStr(ACurrentChapter)  + ', ' +
    QuotedStr(ADownloadedChapterList) + ', ' +
    QuotedStr(ASaveTo) + ')');
end;

function TFavoritesDB.Add(const AOrder:Integer;const AEnabled:Boolean;
  const AWebsite,ALink,ATitle,ACurrentChapter,ADownloadedChapterList,ASaveTo:String):Boolean;
begin
  Result := False;
  if (AWebsite = '') or (ALink = '') then Exit;
  if not Connection.Connected then Exit;
  try
    InternalAdd(AOrder,AEnabled,AWebsite,ALink,ATitle,ACurrentChapter,ADownloadedChapterList,ASaveTo);
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
      'DELETE FROM "favorites" WHERE "websitelink"=' + QuotedStr(LowerCase(AWebsite + ALink)));
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

procedure TFavoritesDB.Close;
begin
  if FCommitCount <> 0 then
    Commit;
  inherited Close;
end;

end.

