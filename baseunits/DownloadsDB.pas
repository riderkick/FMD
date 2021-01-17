unit DownloadsDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLiteData, uBaseUnit, sqlite3dyn;

type

  { TDownloadsDB }

  TDownloadsDB = class(TSQliteData)
  private
    FCommitCount: Integer;
    FAutoCommitCount: Integer;
    procedure SetAutoCommitCount(AValue: Integer);
  public
    constructor Create(const AFilename: String);
    procedure InternalAdd(
      const Aenabled:Boolean;
      const Aorder,Ataskstatus,Achapterptr,Anumberofpages,Acurrentpage:Integer;
      const Awebsite,Alink,Atitle,Astatus,Aprogress,Asaveto:String;
      const Adatetime:TDateTime;
      const Achapterslinks,Achaptersnames,Apagelinks,Apagecontainerlinks,Afilenames,Acustomfilenames,Achaptersstatus:String); inline;
    procedure InternalUpdate(
      const Adlid:Integer;
      const Aenabled:Boolean;
      const Aorder,Ataskstatus,Achapterptr,Anumberofpages,Acurrentpage:Integer;
      const Awebsite,Alink,Atitle,Astatus,Aprogress,Asaveto:String;
      const Adatetime:TDateTime;
      const Achapterslinks,Achaptersnames,Apagelinks,Apagecontainerlinks,Afilenames,Acustomfilenames,Achaptersstatus:String); inline;
    procedure InternalUpdateOrderEnabled(const Adlid,AOrder:Integer;const Aenabled:Boolean);
    procedure InternalUpdateOrder(const Adlid,AOrder:Integer);
    procedure InternalUpdateEnabled(const Adlid:Integer;const Aenabled:Boolean);
    function Add(
      var Adlid:Integer;
      const Aenabled:Boolean;
      const Aorder,Ataskstatus,Achapterptr,Anumberofpages,Acurrentpage:Integer;
      const Awebsite,Alink,Atitle,Astatus,Aprogress,Asaveto:String;
      const Adatetime:TDateTime;
      const Achapterslinks,Achaptersnames,Apagelinks,Apagecontainerlinks,Afilenames,Acustomfilenames,Achaptersstatus:String):Boolean;
    procedure Delete(const ADlId: Integer);
    procedure Commit; override;
    procedure Close; override;
    property AutoCommitCount: Integer read FAutoCommitCount write SetAutoCommitCount;
  end;

const
  f_dlid               = 0;
  f_enabled            = 1;
  f_order              = 2;
  f_taskstatus         = 3;
  f_chapterptr         = 4;
  f_numberofpages      = 5;
  f_currentpage        = 6;
  f_website            = 7;
  f_link               = 8;
  f_title              = 9;
  f_status             = 10;
  f_progress           = 11;
  f_saveto             = 12;
  f_datetime           = 13;
  f_chapterslinks      = 14;
  f_chaptersnames      = 15;
  f_pagelinks          = 16;
  f_pagecontainerlinks = 17;
  f_filenames          = 18;
  f_customfilenames    = 19;
  f_chaptersstatus     = 20;

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
    '"chaptersstatus" TEXT';
  FieldsParams := '"dlid","enabled","order","taskstatus","chapterptr","numberofpages","currentpage","website","link","title","status","progress","saveto","datetime","chapterslinks","chaptersnames","pagelinks","pagecontainerlinks","filenames","customfilenames","chaptersstatus"';
  SelectParams := 'SELECT ' + FieldsParams + ' FROM '+QuotedStrD(TableName)+' ORDER BY "order"';
end;

procedure TDownloadsDB.InternalAdd(
  const Aenabled:Boolean;
  const Aorder,Ataskstatus,Achapterptr,Anumberofpages,Acurrentpage:Integer;
  const Awebsite,Alink,Atitle,Astatus,Aprogress,Asaveto:String;
  const Adatetime:TDateTime;
  const Achapterslinks,Achaptersnames,Apagelinks,Apagecontainerlinks,Afilenames,Acustomfilenames,Achaptersstatus:String);
begin
  Connection.ExecuteDirect('INSERT INTO "downloads" ("enabled","order","taskstatus","chapterptr","numberofpages","currentpage","website","link","title","status","progress","saveto","datetime","chapterslinks","chaptersnames","pagelinks","pagecontainerlinks","filenames","customfilenames","chaptersstatus")' +
    ' VALUES (' +
    QuotedStr(Aenabled) + ', ' +
    QuotedStr(Aorder) + ', ' +
    QuotedStr(Ataskstatus) + ', ' +
    QuotedStr(Achapterptr) + ', ' +
    QuotedStr(Anumberofpages) + ', ' +
    QuotedStr(Acurrentpage) + ', ' +
    QuotedStr(Awebsite) + ', ' +
    QuotedStr(Alink) + ', ' +
    QuotedStr(Atitle) + ', ' +
    QuotedStr(Astatus) + ', ' +
    QuotedStr(Aprogress) + ', ' +
    QuotedStr(Asaveto) + ', ' +
    QuotedStr(Adatetime) + ', ' +
    QuotedStr(Achapterslinks) + ', ' +
    QuotedStr(Achaptersnames) + ', ' +
    QuotedStr(Apagelinks) + ', ' +
    QuotedStr(Apagecontainerlinks) + ', ' +
    QuotedStr(Afilenames) + ', ' +
    QuotedStr(Acustomfilenames) + ', ' +
    QuotedStr(Achaptersstatus) +
    ')');
end;

procedure TDownloadsDB.InternalUpdate(
  const Adlid:Integer;
  const Aenabled:Boolean;
  const Aorder,Ataskstatus,Achapterptr,Anumberofpages,Acurrentpage:Integer;
  const Awebsite,Alink,Atitle,Astatus,Aprogress,Asaveto:String;
  const Adatetime:TDateTime;
  const Achapterslinks,Achaptersnames,Apagelinks,Apagecontainerlinks,Afilenames,Acustomfilenames,Achaptersstatus:String);
begin
  Connection.ExecuteDirect('UPDATE "downloads" SET ' +
    '"enabled"=' +            QuotedStr(Aenabled) + ', ' +
    '"order"=' +              QuotedStr(Aorder) + ', ' +
    '"taskstatus"=' +         QuotedStr(Ataskstatus) + ', ' +
    '"chapterptr"=' +         QuotedStr(Achapterptr) + ', ' +
    '"numberofpages"=' +      QuotedStr(Anumberofpages) + ', ' +
    '"currentpage"=' +        QuotedStr(Acurrentpage) + ', ' +
    '"website"=' +            QuotedStr(Awebsite) + ', ' +
    '"link"=' +               QuotedStr(Alink) + ', ' +
    '"title"=' +              QuotedStr(Atitle) + ', ' +
    '"status"=' +             QuotedStr(Astatus) + ', ' +
    '"progress"=' +           QuotedStr(Aprogress) + ', ' +
    '"saveto"=' +             QuotedStr(Asaveto) + ', ' +
    '"datetime"=' +           QuotedStr(Adatetime) + ', ' +
    '"chapterslinks"=' +      QuotedStr(Achapterslinks) + ', ' +
    '"chaptersnames"=' +      QuotedStr(Achaptersnames) + ', ' +
    '"pagelinks"=' +          QuotedStr(Apagelinks) + ', ' +
    '"pagecontainerlinks"=' + QuotedStr(Apagecontainerlinks) + ', ' +
    '"filenames"=' +          QuotedStr(Afilenames) + ', ' +
    '"customfilenames"=' +    QuotedStr(Acustomfilenames) + ', ' +
    '"chaptersstatus"=' +     QuotedStr(Achaptersstatus) +
    ' WHERE "dlid"=' + QuotedStr(Adlid));
end;

procedure TDownloadsDB.InternalUpdateOrderEnabled(const Adlid, AOrder: Integer;
  const Aenabled: Boolean);
begin
  Connection.ExecuteDirect('UPDATE "downloads" SET ' +
    '"enabled"=' +            QuotedStr(Aenabled) + ', ' +
    '"order"=' +              QuotedStr(Aorder) +
    ' WHERE "dlid"=' + QuotedStr(Adlid));
end;

procedure TDownloadsDB.InternalUpdateOrder(const Adlid, AOrder: Integer);
begin
  Connection.ExecuteDirect('UPDATE "downloads" SET ' +
    '"order"='+QuotedStr(AOrder)+
    ' WHERE "dlid"=' + QuotedStr(Adlid));
end;

procedure TDownloadsDB.InternalUpdateEnabled(const Adlid: Integer;
  const Aenabled: Boolean);
begin
  Connection.ExecuteDirect('UPDATE "downloads" SET ' +
    '"enabled"=' +QuotedStr(Aenabled)+
    ' WHERE "dlid"=' + QuotedStr(Adlid));
end;

function TDownloadsDB.Add(
  var Adlid:Integer;
  const Aenabled:Boolean;
  const Aorder,Ataskstatus,Achapterptr,Anumberofpages,Acurrentpage:Integer;
  const Awebsite,Alink,Atitle,Astatus,Aprogress,Asaveto:String;
  const Adatetime:TDateTime;
  const Achapterslinks,Achaptersnames,Apagelinks,Apagecontainerlinks,Afilenames,Acustomfilenames,Achaptersstatus:String):Boolean;
begin
  Result := False;
  if not Connection.Connected then Exit;
  try
    if Adlid = -1 then
    begin
      InternalAdd(Aenabled,Aorder,Ataskstatus,Achapterptr,Anumberofpages,Acurrentpage,Awebsite,
        Alink,Atitle,Astatus,Aprogress,Asaveto,Adatetime,Achapterslinks,Achaptersnames,Apagelinks,
        Apagecontainerlinks,Afilenames,Acustomfilenames,Achaptersstatus);
      Adlid := Connection.GetInsertID;
    end
    else
      InternalUpdate(Adlid,Aenabled,Aorder,Ataskstatus,Achapterptr,Anumberofpages,Acurrentpage,Awebsite,
        Alink,Atitle,Astatus,Aprogress,Asaveto,Adatetime,Achapterslinks,Achaptersnames,Apagelinks,
        Apagecontainerlinks,Afilenames,Acustomfilenames,Achaptersstatus);
    Inc(FCommitCount);
    if FCommitCount >= FAutoCommitCount then
      Commit;
    Result := True;
  except
    on E: Exception do
      SendLogException(ClassName + '.Add failed!', E);
  end;
end;

procedure TDownloadsDB.Delete(const ADlId: Integer);
begin
  if ADlId = -1 then Exit;
  if not Connection.Connected then Exit;
  try
    Connection.ExecuteDirect(
      'DELETE FROM "downloads" WHERE "dlid"=' + QuotedStr(ADlId));
    Inc(FCommitCount);
    if FCommitCount >= FAutoCommitCount then
      Commit;
  except
    on E: Exception do
      SendLogException(ClassName + '.Delete failed!', E);
  end;
end;

procedure TDownloadsDB.Commit;
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

procedure TDownloadsDB.Close;
begin
  if FCommitCount <> 0 then
    Commit;
  inherited Close;
end;

end.

