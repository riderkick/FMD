unit DownloadsDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLiteData, uBaseUnit;

type

  { TDownloadsDB }

  TDownloadsDB = class(TSQliteData)
  private
    FCommitCount: Integer;
    FAutoCommitCount: Integer;
    procedure SetAutoCommitCount(AValue: Integer);
  public
    constructor Create(const AFilename: String);
    function Add(var Adlid: Integer;
      const Aenabled: Boolean;
      const Aorder, Ataskstatus, Achapterptr, Anumberofpages, Acurrentpage: Integer;
      const Awebsite, Alink, Atitle, Astatus, Aprogress, Asaveto: String;
      const Adatetime: TDateTime;
      const Achapterslinks, Achaptersnames, Apagelinks, Apagecontainerlinks, Afilenames, Acustomfilenames,
        Afailedchapterslinks, Afailedchaptersnames: String): Boolean;
    procedure Delete(const ADlId: Integer);
    procedure Commit; override;
    property AutoCommitCount: Integer read FAutoCommitCount write SetAutoCommitCount;
  end;

implementation

{ TDownloadsDB }

procedure TDownloadsDB.SetAutoCommitCount(AValue: Integer);
begin
  if FAutoCommitCount = AValue then Exit;
  FAutoCommitCount := AValue;
end;

constructor TDownloadsDB.Create(const AFilename: String);
begin
  inherited Create;
  FCommitCount := 0;
  FAutoCommitCount := 300;
  Filename := AFilename;
  TableName := 'downloads';
  Table.PacketRecords := 1;
  CreateParams :=
    '"dlid" INTEGER PRIMARY KEY,' +
    '"enabled" BOOLEAN,' +
    '"order" INTEGER,' +
    '"taskstatus" INTEGER,' +
    '"chapterptr" INTEGER,' +
    '"numberofpages" INTEGER,' +
    '"currentpage" INTEGER,' +
    '"website" TEXT,' +
    '"link" TEXT,' +
    '"title" TEXT,' +
    '"status" TEXT,' +
    '"progress" TEXT,' +
    '"saveto" TEXT,' +
    '"datetime" DATETIME,' +
    '"chapterslinks" TEXT,' +
    '"chaptersnames" TEXT,' +
    '"pagelinks" TEXT,' +
    '"pagecontainerlinks" TEXT,' +
    '"filenames" TEXT,' +
    '"customfilenames" TEXT,' +
    '"failedchapterlinks" TEXT,' +
    '"failedchapternames" TEXT';
  FieldsParams := '"dlid","enabled","order","taskstatus","chapterptr","numberofpages","currentpage","website","link","title","status","progress","saveto","datetime","chapterslinks","chaptersnames","pagelinks","pagecontainerlinks","filenames","customfilenames","failedchapterlinks","failedchapternames"';
  SelectParams := 'SELECT ' + FieldsParams + ' FROM "'+TableName+'" ORDER BY "order"';
end;

function TDownloadsDB.Add(var Adlid: Integer;
  const Aenabled: Boolean;
  const Aorder, Ataskstatus, Achapterptr, Anumberofpages, Acurrentpage: Integer;
  const Awebsite, Alink, Atitle, Astatus, Aprogress, Asaveto: String;
  const Adatetime: TDateTime;
  const Achapterslinks, Achaptersnames, Apagelinks, Apagecontainerlinks, Afilenames, Acustomfilenames,
    Afailedchapterslinks, Afailedchaptersnames: String): Boolean;
begin
  Result := False;
  if (AWebsite = '') or (ALink = '') then Exit;
  if not Connection.Connected then Exit;
  try
    if Adlid <> -1 then
      Connection.ExecuteDirect('UPDATE "downloads" SET ' +
        '"enabled"="' +            BoolToStr(Aenabled, '1', '0') + '",' +
        '"order"="' +              IntToStr(Aorder) + '",' +
        '"taskstatus"="' +         IntToStr(Ataskstatus) + '",' +
        '"chapterptr"="' +         IntToStr(Achapterptr) + '",' +
        '"numberofpages"="' +      IntToStr(Anumberofpages) + '",' +
        '"currentpage"="' +        IntToStr(Acurrentpage) + '",' +
        '"website"="' +            Awebsite + '",' +
        '"link"="' +               Alink + '",' +
        '"title"="' +              Atitle + '",' +
        '"status"="' +             Astatus + '",' +
        '"progress"="' +           Aprogress + '",' +
        '"saveto"="' +             Asaveto + '",' +
        '"datetime"="' +           FormatDateTime('yyyy-mm-dd hh:mm:ss', Adatetime) + '",' +
        '"chapterslinks"="' +      Achapterslinks + '",' +
        '"chaptersnames"="' +      Achaptersnames + '",' +
        '"pagelinks"="' +          Apagelinks + '",' +
        '"pagecontainerlinks"="' + Apagecontainerlinks + '",' +
        '"filenames"="' +          Afilenames + '",' +
        '"customfilenames"="' +    Acustomfilenames + '",' +
        '"failedchapterlinks"="' + Afailedchapterslinks + '",' +
        '"failedchapternames"="' + Afailedchaptersnames + '"' +
        ' WHERE "dlid"="' + IntToStr(Adlid) + '"')
    else
      with Table do
      begin
        Append;
        Fields[1].AsBoolean   := Aenabled;
        Fields[2].AsInteger   := Aorder;
        Fields[3].AsInteger   := Ataskstatus;
        Fields[4].AsInteger   := Achapterptr;
        Fields[5].AsInteger   := Anumberofpages;
        Fields[6].AsInteger   := Acurrentpage;
        Fields[7].AsString    := Awebsite;
        Fields[8].AsString    := Alink;
        Fields[9].AsString    := Atitle;
        Fields[10].AsString   := Astatus;
        Fields[11].AsString   := Aprogress;
        Fields[12].AsString   := Asaveto;
        Fields[13].AsDateTime := Adatetime;
        Fields[14].AsString   := Achapterslinks;
        Fields[15].AsString   := Achaptersnames;
        Fields[16].AsString   := Apagelinks;
        Fields[17].AsString   := Apagecontainerlinks;
        Fields[18].AsString   := Afilenames;
        Fields[19].AsString   := Acustomfilenames;
        Fields[20].AsString   := Afailedchapterslinks;
        Fields[21].AsString   := Afailedchaptersnames;
        Post;
        Adlid := Fields[0].AsInteger;
      end;
    Result := True;
    Inc(FCommitCount);
    if FCommitCount >= FAutoCommitCount then
      Commit;
  except
  end;
end;

procedure TDownloadsDB.Delete(const ADlId: Integer);
begin
  if ADlId = -1 then Exit;
  if not Connection.Connected then Exit;
  try
    Connection.ExecuteDirect(
      'DELETE FROM "downloads" WHERE "dlid"="' + IntToStr(ADlId) + '"');
    Inc(FCommitCount);
    if FCommitCount >= FAutoCommitCount then
      Commit;
  except
  end;
end;

procedure TDownloadsDB.Commit;
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

