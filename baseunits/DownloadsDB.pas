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
  f_failedchapterlinks = 20;
  f_failedchapternames = 21;

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
        Fields[f_enabled           ].AsBoolean   := Aenabled;
        Fields[f_order             ].AsInteger   := Aorder;
        Fields[f_taskstatus        ].AsInteger   := Ataskstatus;
        Fields[f_chapterptr        ].AsInteger   := Achapterptr;
        Fields[f_numberofpages     ].AsInteger   := Anumberofpages;
        Fields[f_currentpage       ].AsInteger   := Acurrentpage;
        Fields[f_website           ].AsString    := Awebsite;
        Fields[f_link              ].AsString    := Alink;
        Fields[f_title             ].AsString    := Atitle;
        Fields[f_status            ].AsString    := Astatus;
        Fields[f_progress          ].AsString    := Aprogress;
        Fields[f_saveto            ].AsString    := Asaveto;
        Fields[f_datetime          ].AsDateTime  := Adatetime;
        Fields[f_chapterslinks     ].AsString    := Achapterslinks;
        Fields[f_chaptersnames     ].AsString    := Achaptersnames;
        Fields[f_pagelinks         ].AsString    := Apagelinks;
        Fields[f_pagecontainerlinks].AsString    := Apagecontainerlinks;
        Fields[f_filenames         ].AsString    := Afilenames;
        Fields[f_customfilenames   ].AsString    := Acustomfilenames;
        Fields[f_failedchapterlinks].AsString    := Afailedchapterslinks;
        Fields[f_failedchapternames].AsString    := Afailedchaptersnames;
        Post;
        Adlid := Fields[f_dlid].AsInteger;
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

