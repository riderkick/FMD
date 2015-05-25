unit  uMisc;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  unixtype,
  {$ENDIF}
  Classes, SysUtils, math, Graphics, strutils, syncobjs, IniFiles, LCLProc;

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

function NaturalCompareStr_RVK(Str1, Str2: string): integer;
function NaturalCompareStr_Typo(Str1, Str2: string):Integer;
{$IFDEF WINDOWS}
function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall; external 'shlwapi.dll';
{$ELSE}
function strcoll(s1, s2 :pchar):integer; cdecl; external 'libc';
function wcscoll(s1, s2: pwchar_t): integer; cdecl; external 'libc' Name 'wcscoll';
{$ENDIF}


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
  //{$IFDEF WINDOWS}
  //Result := StrCmpLogicalW(PWideChar(UTF8Decode(Str1)), PWideChar(UTF8Decode(Str2)));
  //{$ELSE}
  Result := NaturalCompareStr_Typo(Str1, Str2);
  //{$ENDIF}
end;

function NaturalCompareStr_RVK(Str1, Str2: string): integer;
var
  Num1, Num2: double;
  pStr1, pStr2: PChar;
  qStr1, qStr2: PChar;
  Len1, Len2: integer;
  char1, char2: char;

  function IsNumber(ch: char): boolean; inline;
  begin
    Result := ch in ['0'..'9'];
  end;

  function GetNumber(var pch: PChar; var Len: integer): double;
  var
    FoundPeriod: boolean;
    Count: integer = 0;
  begin
    FoundPeriod := False;
    Result := 0;
    while (pch^ <> #0) and (IsNumber(pch^) or ((not FoundPeriod) and
        (pch^ = '.'))) do
    begin
      if pch^ = '.' then
      begin
        FoundPeriod := True;
        Count := 0;
      end
      else
      begin
        if FoundPeriod then
        begin
          Inc(Count);
          Result := Result + (Ord(pch^) - Ord('0')) * Power(10, -Count);
        end
        else
          Result := Result * 10 + Ord(pch^) - Ord('0');
      end;
      Inc(Len);
      Inc(pch);
    end;
  end;

begin
  if (Str1 <> '') and (Str2 <> '') then
  begin
    pStr1 := @Str1[1];
    pStr2 := @Str2[1];
    Result := 0;
    while not ((pStr1^ = #0) or (pStr2^ = #0)) do
    begin
      Len1 := 0;
      Len2 := 0;
      while (pStr1^ = ' ') do
      begin
        Inc(pStr1);
        Inc(Len1);
      end;
      while (pStr2^ = ' ') do
      begin
        Inc(pStr2);
        Inc(Len2);
      end;
      if IsNumber(pStr1^) and IsNumber(pStr2^) then
      begin
        Num1 := GetNumber(pStr1, Len1);
        Num2 := GetNumber(pStr2, Len2);
        if Num1 < Num2 then
          Result := -1
        else if Num1 > Num2 then
          Result := 1
        else
        begin
          if Len1 < Len2 then
            Result := -1
          else if Len1 > Len2 then
            Result := 1;
        end;
        Dec(pStr1);
        Dec(pStr2);
      end
      else
      begin
        qStr1 := pStr1;
        qStr2 := pStr2;
        repeat
          if qStr1^ <> qStr2^ then break; // no need to check further
          Inc(qStr1);
          Inc(qStr2);
        until (qStr1^ = #0) or (qStr2^ = #0) or IsNumber(qStr1^) or IsNumber(qStr2^);

        Char1 := qstr1^;
        Char2 := qstr2^;
        qStr1 := #0;
        qStr2 := #0;

        // SLOW SLOW SLOW SLOW SLOW SLOW SLOW
         //Result := WideCompareText(UTF8Decode(pStr1), UTF8Decode(pStr2));

        // This needs to be optimized even further
        {$IFDEF WINDOWS}

        Result := CompareStringW(LOCALE_USER_DEFAULT, 0,
          pWideChar(UTF8Decode(pStr1)), Length(pStr1),
          pWideChar(UTF8Decode(pStr2)), Length(pStr2)) - 2;

        {$ELSE}
        if IsAscii(pStr1) and IsAscii(pStr2) then
          Result := strcoll(pchar(pStr1), pchar(pStr2))
        else
          Result := wcscoll(pWchar_t(UTF8Decode(pStr1)), pWChar_t(UTF8Decode(pStr2)));
        {$ENDIF}


        if Result <> 0 then // no need to set char back if result <> 0
        begin
          qStr1^ := Char1;
          qStr2^ := Char2;
          pStr1 := qStr1 - 1;
          pStr2 := qStr2 - 1;
        end;

      end;

      if Result <> 0 then Break;

      Inc(pStr1);
      Inc(pStr2);

    end;
  end;
  Num1 := Length(Str1);
  Num2 := Length(Str2);
  if (Result = 0) and (Num1 <> Num2) then
  begin
    if Num1 < Num2 then
      Result := -1
    else
      Result := 1;
  end;
end;

function NaturalCompareStr_Typo(Str1, Str2: string):Integer;
var
  Num1, Num2: double;
  pStr1, pStr2: PChar;
  Len1, Len2: integer;
  TextLen1, TextLen2: integer;
  TextStr1: string = '';
  TextStr2: string = '';
  i: integer;
  j: integer;
  {$IFDEF LINUX}
  ConvertedString1 :UCS4string = nil;
  ConvertedString2 :UCS4string = nil;
  {$ENDIF}

  function IsNumber(ch: char): boolean;
  begin
    Result := ch in ['0'..'9'];
  end;

  function GetNumber(var pch: PChar; var Len: integer): double;
  var
    FoundPeriod: boolean;
    Count: integer;
  begin
    FoundPeriod := False;
    Result := 0;
    while (pch^ <> #0) and (IsNumber(pch^) or ((not FoundPeriod) and
        (pch^ = '.'))) do
    begin
      if pch^ = '.' then
      begin
        FoundPeriod := True;
        Count := 0;
      end
      else
      begin
        if FoundPeriod then
        begin
          Inc(Count);
          Result := Result + (Ord(pch^) - Ord('0')) * Power(10, -Count);
        end
        else
          Result := Result * 10 + Ord(pch^) - Ord('0');
      end;
      Inc(Len);
      Inc(pch);
    end;
  end;

  procedure GetChars;
  begin
    TextLen1 := 0;
    while not ((pStr1 + TextLen1)^ in ['0'..'9']) and ((pStr1 + TextLen1)^ <> #0) do
      Inc(TextLen1);
    SetLength(TextStr1, TextLen1);
    i := 1;
    j := 0;
    while i <= TextLen1 do
    begin
      TextStr1[i] := (pStr1 + j)^;
      Inc(i);
      Inc(j);
    end;

    TextLen2 := 0;
    while not ((pStr2 + TextLen2)^ in ['0'..'9']) and ((pStr2 + TextLen2)^ <> #0) do
      Inc(TextLen2);
    SetLength(TextStr2, TextLen2);
    i := 1;
    j := 0;
    while i <= TextLen2 do
    begin
      TextStr2[i] := (pStr2 + j)^;
      Inc(i);
      Inc(j);
    end;
    {$IFDEF LINUX}
    ConvertedString1 := UnicodeStringToUCS4String(UTF8Decode(TextStr1));
    ConvertedString2 := UnicodeStringToUCS4String(UTF8Decode(TextStr2));
    {$ENDIF}
  end;

begin
  if (Str1 <> '') and (Str2 <> '') then
  begin
    pStr1 := @Str1[1];
    pStr2 := @Str2[1];
    Result := 0;
    while not ((pStr1^ = #0) or (pStr2^ = #0)) do
    begin
      TextLen1 := 1;
      TextLen2 := 1;
      Len1 := 0;
      Len2 := 0;
      while (pStr1^ = ' ') do
      begin
        Inc(pStr1);
        Inc(Len1);
      end;
      while (pStr2^ = ' ') do
      begin
        Inc(pStr2);
        Inc(Len2);
      end;
      if IsNumber(pStr1^) and IsNumber(pStr2^) then
      begin
        Num1 := GetNumber(pStr1, Len1);
        Num2 := GetNumber(pStr2, Len2);
        if Num1 < Num2 then
          Result := -1
        else if Num1 > Num2 then
          Result := 1
        else
        begin
          if Len1 < Len2 then
            Result := -1
          else if Len1 > Len2 then
            Result := 1;
        end;
        Dec(pStr1);
        Dec(pStr2);
      end
      else
      begin
        GetChars;

        //Result := WideCompareText(UTF8Decode(TextStr1), UTF8Decode(TextStr2));
        {$IFDEF WINDOWS}
        Result := CompareStringW(LOCALE_USER_DEFAULT, 0,
          pWideChar(UTF8Decode(TextStr1)), Length(TextStr1),
          pWideChar(UTF8Decode(TextStr2)), Length(TextStr2)) - 2;
        {$ELSE}
        if IsAscii(TextStr1) and IsAscii(TextStr2) then
          Result := strcoll(pchar(textstr1), pchar(textstr2))
        else
          Result := wcscoll(pWchar_t(ConvertedString1), pWChar_t(ConvertedString2));
        {$ENDIF}
      end;
      if Result <> 0 then
        Break;
      Inc(pStr1, TextLen1);
      Inc(pStr2, TextLen2);
    end;
  end;
  Num1 := Length(Str1);
  Num2 := Length(Str2);
  if (Result = 0) and (Num1 <> Num2) then
  begin
    if Num1 < Num2 then
      Result := -1
    else
      Result := 1;
  end;
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
