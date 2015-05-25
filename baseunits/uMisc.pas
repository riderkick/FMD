unit  uMisc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, strutils, syncobjs, IniFiles,
  NaturalSortUnit;

type
  TIniFileR = class(TMemIniFile)
  private
    FCSReload: TCriticalSection;
    FFileAge: longint;
  public
    constructor Create(const AFileName: String; AEscapeLineFeeds: Boolean = False);
      override;
    destructor Destroy; override;
    procedure Reload;
  end;

  TLogType = (LOG_debug, LOG_info, LOG_warning, LOG_error);

//String utils
procedure padZero(var S: String; VolLength, ChapLength: Integer);
function padZeros(const S: String; VolLength, ChapLength: Integer): String;
function BrackText(const S: String): String; overload;
function BrackText(const S: Integer): String; overload;
function BrackSquareText(const S: String): String; overload;
function BrackSquareText(const S: Integer): String; overload;
function BrackTextQuoted(const S: String): String; overload;
function BrackTextQuoted(const S: Integer): String; overload;
function StringToASCII(S: String): String;
function StringToHex(S: String): String;

procedure QuickSortNaturalPart(var Alist: TStringList; Separator: String;
  PartIndex: Integer);

//Images
function MangaFoxRemoveWatermarks(const Filename: String): Boolean;

//Logging
procedure WriteLog(msg: String; logType: TLogType = LOG_debug);
procedure WriteOtherLog(msg: String);

//Searching
function FindStrLinear(aList: TStrings; aValue: String): Boolean;
function FindStrLinearPos(aList: TStrings; aValue: String): Integer;

//formatting
function FormatByteSize(const bytes :longint; persecond: boolean = False) :string;

//sorting
function NaturalCompareStr(Str1, Str2: string): integer;

var
  CS_LOG, CS_OTHERLOG: TCriticalSection;

const
  fLogFile = 'fmd_log.txt';
  fOtherLogFile = 'fmd_otherLog.txt';

  UA_CURL      = 'curl/7.42.1';
  UA_MSIE      = 'Mozilla/5.0 (compatible; WOW64; MSIE 10.0; Windows NT 6.2)';
  UA_FIREFOX   = 'Mozilla/5.0 (Windows NT 6.3; WOW64; rv:37.0) Gecko/20100101 Firefox/37.0';
  UA_CHROME    = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36';
  UA_OPERA     = 'Mozilla/5.0 (Windows NT 6.3; WOW64; rv:37.0) Gecko/20100101 Firefox/37.0';
  UA_GOOGLEBOT = 'Mozilla/5.0 (compatible; Googlebot/2.1;  http://www.google.com/bot.html)';

  RANDOM_SLEEP = 3000;

implementation

{ TIniFileR }

constructor TIniFileR.Create(const AFileName: String; AEscapeLineFeeds: Boolean = False);
begin
  inherited Create(AFileName, AEscapeLineFeeds);
  FCSReload := TCriticalSection.Create;
  FFileAge := FileAge(Self.FileName);
end;

destructor TIniFileR.Destroy;
begin
  FCSReload.Free;
  inherited Destroy;
end;

procedure TIniFileR.Reload;
var
  slLines: TStringList;
begin
  if FCSReload.TryEnter then try
    if FileExists(FileName) then
      if FileAge(FileName) <> FFileAge then
      begin
        slLines := TStringList.Create;
        try
          FFileAge := FileAge(FileName);
          slLines.LoadFromFile(FileName);
          SetStrings(slLines);
        finally
          slLines.Free;
        end;
      end;
  finally
    FCSReload.Release;
  end;
end;

{ uMisc }

procedure WriteLog(msg: String; logType: TLogType = LOG_debug);
{$IFDEF LOGACTIVE}
var
  s: String;
  f: TextFile;
{$ENDIF}
begin
  {$IFDEF LOGACTIVE}
  CS_LOG.Acquire;
  try
    s := FormatDateTime('dd/mm/yyyy|hh:nn:ss.zzz ', Now);
    case logType of
      LOG_debug: s := s + '[D]';
      LOG_info: s := s + '[I]';
      LOG_warning: s := s + '[W]';
      LOG_error: s := s + '[E]';
    end;
    AssignFile(f, fLogFile);
    try
      if FileExists(fLogFile) then
        Append(f)
      else
        Rewrite(f);
      WriteLn(f, s + ' ' + msg);
    finally
      CloseFile(f);
    end;
  finally
    CS_LOG.Release;
  end;
  {$ENDIF}
end;

procedure WriteOtherLog(msg: String);
{$IFDEF LOGACTIVE}
var
  f: TextFile;
{$ENDIF}
begin
  {$IFDEF LOGACTIVE}
  CS_OTHERLOG.Acquire;
  try
    AssignFile(f, fOtherLogFile);
    try
      if FileExists(fOtherLogFile) then
        Append(f)
      else
        Rewrite(f);
      WriteLn(f, msg);
    finally
      CloseFile(f);
    end;
  finally
    CS_OTHERLOG.Release;
  end;
  {$ENDIF}
end;

function BrackText(const S: String): String;
begin
  Result := '(' + S + ')';
end;

function BrackText(const S: Integer): String;
begin
  Result := BrackText(IntToStr(S));
end;

function BrackSquareText(const S: String): String;
begin
  Result := '[' + S + ']';
end;

function BrackSquareText(const S: Integer): String;
begin
  Result := BrackText(IntToStr(S));
end;

function BrackTextQuoted(const S : String) : String;
begin
  Result := BrackText(QuotedStr(S));
end;

function BrackTextQuoted(const S : Integer) : String;
begin
  Result := BrackText(QuotedStr(IntToStr(S)));
end;

function StringToASCII(S : String) : String;
var
  i: Integer;
begin
  Result:='#0';
  if Length(S) > 0 then
  begin
    Result := '';
    for i := 1 to Length(S) do
      Result := Result + '#' + IntToStr(Ord(S[i])) + ';';
  end;
end;

function StringToHex(S : String) : String;
var
  i: Integer;
begin
  Result:='#0';
  if Length(S) > 0 then
  begin
    Result := '';
    for i := 1 to Length(S) do
      Result := Result + '#' + IntToHex(Ord(S[i]), 2) + ';';
  end;
end;

procedure padZero(var S: String; VolLength, ChapLength: Integer);

  procedure searchChap(var i, cstart, clength: Integer; var t: String);
  var
    j: Integer;
  begin
    for  j := i to Length(t) do
    begin
      if (cstart = -1) and (t[j] in ['0'..'9']) then
        cstart := j
      else if (cstart > -1) and ((not (t[j] in ['0'..'9'])) or (j = Length(t))) then
      begin
        if j = Length(t) then
          clength := j - cstart + 2
        else
          clength := j - cstart;
        Break;
      end;
    end;
  end;

var
  i, cstart, vstart, clength, vlength, vp: Integer;
  t, c, v: String;
  vol: Boolean = False;
  cha: Boolean = False;
begin
  cstart := 0;
  vstart := 0;
  clength := 0;
  vlength := 0;
  vp := 0;
  c := '';
  v := '';

  t := S;
  i := 1;

  // Volume number
  if VolLength > 0 then
  begin
    vp := Pos('VOL', upcase(t));
    if (vp > 0) then
    begin
      if (vp > 2) and (not (t[vp - 1] in [',', '.', '-', '_', ' '])) then
        vol := False
      else if Pos('VOLUME NOT AVAILABLE', upcase(t)) > 0 then
        vol := False
      else
        vol := True;
    end;

    vstart := -1;
    vlength := 1;
    if vol then
    begin
      for  i := Pos('VOL', upcase(t)) to Length(t) do
      begin
        if (vstart = -1) and (t[i] in ['0'..'9']) then
          vstart := i
        else if (vstart > -1) and ((not (t[i] in ['0'..'9'])) or (i = Length(t))) then
        begin
          if i = Length(t) then
            vlength := i - vstart + 1
          else
            vlength := i - vstart;
          Break;
        end;
      end;
      if vstart = -1 then
        vol := False;
    end;
    if vol then
    begin
      v := Copy(t, vstart, vlength);
      while Length(v) < VolLength do
      begin
        v := '0' + v;
      end;
    end;
  end;

  cstart := -1;
  clength := 1;
  // Chapter number
  if ChapLength > 0 then
  begin
    if i = Length(t) then
      i := 1;
    searchChap(i, cstart, clength, t);
    if (cstart = -1) and (not (i = 1)) then  //if not found at first try
    begin
      i := 1;
      searchChap(i, cstart, clength, t);
    end
    else
    if (cstart = -1) then
      cha := False
    else
      cha := True;

    if cha then
    begin
      c := Copy(t, cstart, clength);
      while Length(c) < ChapLength do
      begin
        c := '0' + c;
      end;
    end;
  end;

  if (cha) and (vol) and (vstart < cstart) then
  begin
    Delete(t, cstart, clength);
    Insert(c, t, cstart);
    Delete(t, vstart, vlength);
    Insert(v, t, vstart);
  end
  else
  if (cha) and (vol) and (vstart > cstart) then
  begin
    Delete(t, vstart, vlength);
    Insert(v, t, vstart);
    Delete(t, cstart, clength);
    Insert(c, t, cstart);
  end
  else
  if (vol) then
  begin
    Delete(t, vstart, vlength);
    Insert(v, t, vstart);
  end
  else
  if (cha) then
  begin
    Delete(t, cstart, clength);
    Insert(c, t, cstart);
  end;
  S := t;
end;

function padZeros(const S: String; VolLength, ChapLength: Integer): String;
begin
  Result := S;
  padZero(Result, VolLength, ChapLength);
end;

//loading directly from stream accepted as raw (file become bigger)
//Not all mangafox image has watermarks, old manga doesn't have watermarks
//recognizing by file age or by scanning images manually.
function MangaFoxRemoveWatermarks(const Filename: String): Boolean;
var
  fpic: TPicture;
begin
  Result := False;
  Exit; //Disable for a moment
  fpic := TPicture.Create;
  try
    fpic.LoadFromFile(Filename);
    if fpic.Bitmap.Height < 100 then
      Exit;
    fpic.Bitmap.Height := fpic.Bitmap.Height - 90;// crop by 90px
    if FileExists(Filename) then
      DeleteFile(Filename);
    fpic.SaveToFile(Filename);
    Result := True;
  finally
    fpic.Free;
  end;
end;

function getStringPart(const txt, sep: String; partIndex: Cardinal): String;
var
  i, j, lpos, rpos: Integer;
begin
  lpos := 1;
  rpos := 1;
  Result := '';

  for i := 0 to partIndex do
  begin
    j := PosEx(sep, txt, rpos);
    if (j > 0) then
    begin
      lpos := rpos;
      rpos := j + Length(sep);
    end
    else
      Break;
  end;
  Result := Copy(txt, lpos, rpos - lpos - Length(sep));
end;

function NaturalCompareStr(Str1, Str2: string): integer;
begin
  Result := NaturalSortUnit.UTF8NaturalCompareText(Str1, Str2);
end;

procedure QuickSortNaturalPart(var Alist: TStringList; Separator: String;
  PartIndex: Integer);

  procedure QuickSort(L, R: Integer);
  var
    Pivot, vL, vR: Integer;
    PivotStr: String;
  begin
    if R - L <= 1 then
    begin // a little bit of time saver
      if L < R then
        if NaturalCompareStr(getStringPart(Alist.Strings[L], Separator, PartIndex),
          getStringPart(Alist.Strings[R], Separator, PartIndex)) > 0 then
          Alist.Exchange(L, R);
      Exit;
    end;
    vL := L;
    vR := R;
    Pivot := L + Random(R - L); // they say random is best
    PivotStr := getStringPart(Alist.Strings[Pivot], Separator, PartIndex);
    while vL < vR do
    begin
      while (vL < Pivot) and (NaturalCompareStr(
          getStringPart(Alist.Strings[vL], Separator, PartIndex), PivotStr) <= 0) do
        Inc(vL);
      while (vR > Pivot) and (NaturalCompareStr(
          getStringPart(Alist.Strings[vR], Separator, PartIndex), PivotStr) > 0) do
        Dec(vR);
      Alist.Exchange(vL, vR);
      if Pivot = vL then // swap pivot if we just hit it from one side
      begin
        Pivot := vR;
        PivotStr := getStringPart(Alist.Strings[Pivot], Separator, PartIndex);
      end
      else
      if Pivot = vR then
      begin
        Pivot := vL;
        PivotStr := getStringPart(Alist.Strings[Pivot], Separator, PartIndex);
      end;
    end;
    if Pivot - 1 >= L then
      QuickSort(L, Pivot - 1);
    if Pivot + 1 <= R then
      QuickSort(Pivot + 1, R);
  end;

begin
  if Alist.Count < 2 then Exit;
  try
    Alist.BeginUpdate;
    QuickSort(0, Alist.Count - 1);
  finally
    Alist.EndUpdate;
  end;
end;

function FindStrLinearPos(aList: TStrings; aValue: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to aList.Count - 1 do
  begin
    if CompareText(aList.Strings[i], aValue) = 0 then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function FindStrLinear(aList: TStrings; aValue: String): Boolean;
begin
  if FindStrLinearPos(aList, aValue) >= 0 then
    Result := True
  else
    Result := False;
end;

function FormatByteSize(const bytes :longint; persecond: boolean = False) :string;
const
  B  = 1;
  KB = 1024 * B;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if bytes > GB then
    Result := FormatFloat('#.## GB', bytes / GB)
  else
  if bytes > MB then
    Result := FormatFloat('#.## MB', bytes / MB)
  else
  if bytes > KB then
    Result := FormatFloat('#.## KB', bytes / KB)
  else
  if bytes = 0 then
  begin
    if persecond then
      Result := '0 B'
    else
      Result := '0 bytes';
  end
  else
  begin
    if persecond then
      Result := FormatFloat('#.## B', bytes)
    else
      Result := FormatFloat('#.## bytes', bytes);
  end;
  if persecond then
    Result := Result + 'ps';
end;

initialization
  CS_LOG := TCriticalSection.Create;
  CS_OTHERLOG := TCriticalSection.Create;
  {$IFDEF LOGACTIVE}
  WriteLog('Starting FMD', LOG_Info);
  {$ENDIF}

finalization
  {$IFDEF LOGACTIVE}
  WriteLog('FMD exit normally', LOG_Info);
  {$ENDIF}
  FreeAndNil(CS_OTHERLOG);
  FreeAndNil(CS_LOG);

end.
