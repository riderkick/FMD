unit FavoritesDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLiteData;

type

  { TFavoritesDB }

  TFavoritesDB = class(TSQliteData)
  private
  public
    constructor Create;
    function Add(const AWebsiteLink: string; const AOrder: Integer;
      const AWebsite, ALink, ATitle, ASaveTo, ACurrentChapter, ADownloadedChapterList: string): Boolean;
    procedure Delete(const AWebsiteLink: string);
  end;

implementation

{ TFavoritesDB }

constructor TFavoritesDB.Create;
begin
  inherited Create;
  TableName := 'favorites';
  CreateParams :=
    '"websitelink" VARCHAR(3000) NOT NULL PRIMARY KEY,' +
    '"order" INTEGER',
    '"website" TEXT',
    '"link" TEXT',
    '"title" TEXT',
    '"saveto" TEXT',
    '"currentchapter" TEXT',
    '"downloadedchapterlist" TEXT';
  SelectParams :=
    'SELECT * FROM "favorites" ORDER BY "order"');
  {
    CREATE TABLE "favorites" (
      "websitelink" VARCHAR(3000) NOT NULL PRIMARY KEY,
      "order" TEXT,
      "website" TEXT,
      "link" TEXT,
      "title" TEXT,
      "saveto" TEXT,
      "currentchapter" TEXT,
      "downloadedchapterlist" TEXT
      );
  }
end;

function TFavoritesDB.Add(const AWebsiteLink: string; const AOrder: Integer;
  const AWebsite, ALink, ATitle, ASaveTo, ACurrentChapter, ADownloadedChapterList: string): Boolean;
begin
  Result := False;
  if AWebsiteLink = '' then Exit;
  if not Connection.Connected then Exit;
  try
    Connection.ExecuteDirect('INSERT OR REPLACE INTO "favorites" ('+
      '"websitelink", "order", "website", "link", "title", "saveto", "currentchapter", "downloadedchapterlist")'+
      ' VALUES ("' +
      AWebsiteLink + '","' +
      IntToStr(AOrder) + '","' +
      AWebsite + '","' +
      ALink + '","' +
      ATitle + '","' +
      ASaveTo + '","' +
      ACurrentChapter  + '","' +
      ADownloadedChapterList + '")');
    Result := True;
  except
  end;
end;

procedure TFavoritesDB.Delete(const AWebsiteLink: string);
begin
  if AWebsiteLink = '' then Exit;
  if not Connection.Connected then Exit;
  try
    Connection.ExecuteDirect('DELETE FROM "favorites" WHERE "AWebsiteLink"="' + AWebsiteLink + '"');
  except
  end;
end;

end.

