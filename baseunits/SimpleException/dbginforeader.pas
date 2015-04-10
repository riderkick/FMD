{
    This file is a modified copy of the FreePascal units lnfodwrf and lineinfo
}
unit DbgInfoReader;

{$mode objfpc}{$H-}{$S-}

interface

function  OpenSymbolFile(AFileName: string): boolean;
procedure CloseSymbolFile;
function  GetLineInfo(addr:ptruint; out func,source:string; out line:longint) : boolean;

implementation

uses
  exeinfo, strings;

var
  { the input file to read DWARF/STABS debug info from, i.e. paramstr(0) }
  e : TExeFile;
  filename, dbgfn : string;
  //baseaddr : pointer;
  HasStabs, HasDwarf: Boolean;

{%region ********************* lnfodwrf ************************************}

{ Current issues:

  - ignores DW_LNS_SET_FILE
}

{$MACRO ON}

//{$DEFINE DEBUG_DWARF_PARSER}
{$ifdef DEBUG_DWARF_PARSER}
  {$define DEBUG_WRITELN := WriteLn}
  {$define DEBUG_COMMENT :=  }
{$else}
  {$define DEBUG_WRITELN := //}
  {$define DEBUG_COMMENT := //}
{$endif}

{ some type definitions }
type
  Bool8 = ByteBool;

const
  EBUF_SIZE = 100;

//{$WARNING This code is not thread-safe, and needs improvement}
var
  EBuf: Array [0..EBUF_SIZE-1] of Byte;
  EBufCnt, EBufPos: Integer;
  { the offset and size of the DWARF debug_line section in the file }
  DwarfOffset : longint;
  DwarfSize : longint;

{ DWARF 2 default opcodes}
const
  { Extended opcodes }
  DW_LNE_END_SEQUENCE = 1;
  DW_LNE_SET_ADDRESS = 2;
  DW_LNE_DEFINE_FILE = 3;
  { Standard opcodes }
  DW_LNS_COPY = 1;
  DW_LNS_ADVANCE_PC = 2;
  DW_LNS_ADVANCE_LINE = 3;
  DW_LNS_SET_FILE = 4;
  DW_LNS_SET_COLUMN = 5;
  DW_LNS_NEGATE_STMT = 6;
  DW_LNS_SET_BASIC_BLOCK = 7;
  DW_LNS_CONST_ADD_PC = 8;
  DW_LNS_FIXED_ADVANCE_PC = 9;
  DW_LNS_SET_PROLOGUE_END = 10;
  DW_LNS_SET_EPILOGUE_BEGIN = 11;
  DW_LNS_SET_ISA = 12;

type
  { state record for the line info state machine }
  TMachineState = record
    address : QWord;
    file_id : DWord;
    line : QWord;
    column : DWord;
    is_stmt : Boolean;
    basic_block : Boolean;
    end_sequence : Boolean;
    prolouge_end : Boolean;
    epilouge_begin : Boolean;
    isa : DWord;
    append_row : Boolean;
  end;

{ DWARF line number program header preceding the line number program, 64 bit version }
  TLineNumberProgramHeader64 = packed record
    magic : DWord;
    unit_length : QWord;
    version : Word;
    length : QWord;
    minimum_instruction_length : Byte;
    default_is_stmt : Bool8;
    line_base : ShortInt;
    line_range : Byte;
    opcode_base : Byte;
  end;

{ DWARF line number program header preceding the line number program, 32 bit version }
  TLineNumberProgramHeader32 = packed record
    unit_length : DWord;
    version : Word;
    length : DWord;
    minimum_instruction_length : Byte;
    default_is_stmt : Bool8;
    line_base : ShortInt;
    line_range : Byte;
    opcode_base : Byte;
  end;

{---------------------------------------------------------------------------
 I/O utility functions
---------------------------------------------------------------------------}

var
  base, limit : SizeInt;
  index : SizeInt;

function Init(aBase, aLimit : Int64) : Boolean;
begin
  base := aBase;
  limit := aLimit;
  Init := (aBase + limit) <= e.size;
  seek(e.f, base);
  EBufCnt := 0;
  EBufPos := 0;
  index := 0;
end;

function Init(aBase : Int64) : Boolean;
begin
  Init := Init(aBase, limit - (aBase - base));
end;


function Pos() : Int64;
begin
  Pos := index;
end;


procedure Seek(const newIndex : Int64);
begin
  index := newIndex;
  system.seek(e.f, base + index);
  EBufCnt := 0;
  EBufPos := 0;
end;


{ Returns the next Byte from the input stream, or -1 if there has been
  an error }
function ReadNext() : Longint; inline;
var
  bytesread : SizeInt;
begin
  ReadNext := -1;
  if EBufPos >= EBufCnt then begin
    EBufPos := 0;
    EBufCnt := EBUF_SIZE;
    if EBufCnt > limit - index then
      EBufCnt := limit - index;
    blockread(e.f, EBuf, EBufCnt, bytesread{%H-});
    EBufCnt := bytesread;
  end;
  if EBufPos < EBufCnt then begin
    ReadNext := EBuf[EBufPos];
    inc(EBufPos);
    inc(index);
  end
  else
    ReadNext := -1;
end;

{ Reads the next size bytes into dest. Returns true if successful,
  false otherwise. Note that dest may be partially overwritten after
  returning false. }
function ReadNext(var dest; size : SizeInt) : Boolean; //inline;
var
  bytesread, totalread : SizeInt;
  r: Boolean;
  d: PByte;
begin
  d := @dest;
  totalread := 0;
  r := True;
  while (totalread < size) and r do begin;
    if EBufPos >= EBufCnt then begin
      EBufPos := 0;
      EBufCnt := EBUF_SIZE;
      if EBufCnt > limit - index then
        EBufCnt := limit - index;
      blockread(e.f, EBuf, EBufCnt, bytesread{%H-});
      EBufCnt := bytesread;
      if bytesread <= 0 then
        r := False;
    end;
    if EBufPos < EBufCnt then begin
      bytesread := EBufCnt - EBufPos;
      if bytesread > size - totalread then bytesread := size - totalread;
      System.Move(EBuf[EBufPos], d[totalread], bytesread);
      inc(EBufPos, bytesread);
      inc(index, bytesread);
      inc(totalread, bytesread);
    end;
  end;
  ReadNext := r;
end;


{ Reads an unsigned LEB encoded number from the input stream }
function ReadULEB128() : QWord;
var
  shift : Byte;
  data : PtrInt;
  val : QWord;
begin
  shift := 0;
  ReadULEB128 := 0;
  data := ReadNext();
  while (data <> -1) do begin
    val := data and $7f;
    ReadULEB128 := ReadULEB128 or (val shl shift);
    inc(shift, 7);
    if ((data and $80) = 0) then
      break;
    data := ReadNext();
  end;
end;

{ Reads a signed LEB encoded number from the input stream }
function ReadLEB128() : Int64;
var
  shift : Byte;
  data : PtrInt;
  val : Int64;
begin
  shift := 0;
  ReadLEB128 := 0;
  data := ReadNext();
  while (data <> -1) do begin
    val := data and $7f;
    ReadLEB128 := ReadLEB128 or (val shl shift);
    inc(shift, 7);
    if ((data and $80) = 0) then
      break;
    data := ReadNext();
  end;
  { extend sign. Note that we can not use shl/shr since the latter does not
    translate to arithmetic shifting for signed types }
  ReadLEB128 := (not ((ReadLEB128 and (1 shl (shift-1)))-1)) or ReadLEB128;
end;


{ Reads an address from the current input stream }
function ReadAddress() : PtrUInt;
begin
  ReadNext(ReadAddress{%H-}, sizeof(ReadAddress));
end;


{ Reads a zero-terminated string from the current input stream. If the
  string is larger than 255 chars (maximum allowed number of elements in
  a ShortString, excess characters will be chopped off. }
function ReadString() : ShortString;
var
  temp : PtrInt;
  i : PtrUInt;
begin
  i := 1;
  temp := ReadNext();
  while (temp > 0) do begin
    ReadString[i] := char(temp);
    if (i = 255) then begin
      { skip remaining characters }
      repeat
        temp := ReadNext();
      until (temp <= 0);
      break;
    end;
    inc(i);
    temp := ReadNext();
  end;
  { unexpected end of file occurred? }
  if (temp = -1) then
    ReadString := ''
  else
    Byte(ReadString[0]) := i-1;
end;


{ Reads an unsigned Half from the current input stream }
function ReadUHalf() : Word;
begin
  ReadNext(ReadUHalf{%H-}, sizeof(ReadUHalf));
end;


{---------------------------------------------------------------------------

 Generic Dwarf lineinfo reader

 The line info reader is based on the information contained in

   DWARF Debugging Information Format Version 3
   Chapter 6.2 "Line Number Information"

 from the

   DWARF Debugging Information Format Workgroup.

 For more information on this document see also

   http://dwarf.freestandards.org/

---------------------------------------------------------------------------}

{ initializes the line info state to the default values }
procedure InitStateRegisters(var state : TMachineState; const aIs_Stmt : Bool8);
begin
  with state do begin
    address := 0;
    file_id := 1;
    line := 1;
    column := 0;
    is_stmt := aIs_Stmt;
    basic_block := false;
    end_sequence := false;
    prolouge_end := false;
    epilouge_begin := false;
    isa := 0;
    append_row := false;
  end;
end;


{ Skips all line info directory entries }
procedure SkipDirectories();
var s : ShortString;
begin
  while (true) do begin
    s := ReadString();
    if (s = '') then break;
    DEBUG_WRITELN('Skipping directory : ', s);
  end;
end;

{ Skips an LEB128 }
procedure SkipLEB128();
{$ifdef DEBUG_DWARF_PARSER}
var temp : QWord;
{$endif}
begin
  {$ifdef DEBUG_DWARF_PARSER}temp := {$endif}ReadLEB128();
  DEBUG_WRITELN('Skipping LEB128 : ', temp);
end;

{ Skips the filename section from the current file stream }
procedure SkipFilenames();
var s : ShortString;
begin
  while (true) do begin
    s := ReadString();
    if (s = '') then break;
    DEBUG_WRITELN('Skipping filename : ', s);
    SkipLEB128(); { skip the directory index for the file }
    SkipLEB128(); { skip last modification time for file }
    SkipLEB128(); { skip length of file }
  end;
end;

function CalculateAddressIncrement(opcode : Byte; const header : TLineNumberProgramHeader64) : Int64;
begin
  CalculateAddressIncrement := (Int64(opcode) - header.opcode_base) div header.line_range * header.minimum_instruction_length;
end;

function GetFullFilename(const filenameStart, directoryStart : Int64; const file_id : DWord) : ShortString;
var
  i : DWord;
  filename, directory : ShortString;
  dirindex : Int64;
begin
  filename := '';
  directory := '';
  i := 1;
  Seek(filenameStart);
  while (i <= file_id) do begin
    filename := ReadString();
    DEBUG_WRITELN('Found "', filename, '"');
    if (filename = '') then break;
    dirindex := ReadLEB128(); { read the directory index for the file }
    SkipLEB128(); { skip last modification time for file }
    SkipLEB128(); { skip length of file }
    inc(i);
  end;
  { if we could not find the file index, exit }
  if (filename = '') then begin
    GetFullFilename := '(Unknown file)';
    exit;
  end;

  Seek(directoryStart);
  i := 1;
  while (i <= dirindex) do begin
    directory := ReadString();
    if (directory = '') then break;
    inc(i);
  end;
  if (directory<>'') and (directory[length(directory)]<>'/') then
    directory:=directory+'/';
  GetFullFilename := directory + filename;
end;


function ParseCompilationUnit(const addr : PtrUInt; const file_offset : QWord;
  var source : String; var line : longint; var found : Boolean) : QWord;
var
  state : TMachineState;
  { we need both headers on the stack, although we only use the 64 bit one internally }
  header64 : TLineNumberProgramHeader64;
  header32 : TLineNumberProgramHeader32;

  adjusted_opcode : Int64;

  opcode : PtrInt;
  extended_opcode : Byte;
  extended_opcode_length : PtrInt;
  i, addrIncrement, lineIncrement : PtrInt;

  {$ifdef DEBUG_DWARF_PARSER}
  s : ShortString;
  {$endif}

  numoptable : array[1..255] of Byte;
  { the offset into the file where the include directories are stored for this compilation unit }
  include_directories : QWord;
  { the offset into the file where the file names are stored for this compilation unit }
  file_names : Int64;

  temp_length : DWord;
  unit_length : QWord;
  header_length : SizeInt;

  first_row : Boolean;

  prev_line : QWord;
  prev_file : DWord;

begin
  prev_line := 0;
  prev_file := 0;
  first_row := true;

  found := false;

  ReadNext(temp_length{%H-}, sizeof(temp_length));
  if (temp_length <> $ffffffff) then begin
    unit_length := temp_length + sizeof(temp_length)
  end else begin
    ReadNext(unit_length, sizeof(unit_length));
    inc(unit_length, 12);
  end;

  ParseCompilationUnit := file_offset + unit_length;

  Init(file_offset, unit_length);

  DEBUG_WRITELN('Unit length: ', unit_length);
  if (temp_length <> $ffffffff) then begin
    DEBUG_WRITELN('32 bit DWARF detected');
    ReadNext(header32{%H-}, sizeof(header32));
    header64.magic := $ffffffff;
    header64.unit_length := header32.unit_length;
    header64.version := header32.version;
    header64.length := header32.length;
    header64.minimum_instruction_length := header32.minimum_instruction_length;
    header64.default_is_stmt := header32.default_is_stmt;
    header64.line_base := header32.line_base;
    header64.line_range := header32.line_range;
    header64.opcode_base := header32.opcode_base;
    header_length :=
      sizeof(header32.length) + sizeof(header32.version) +
      sizeof(header32.unit_length);
  end else begin
    DEBUG_WRITELN('64 bit DWARF detected');
    ReadNext(header64, sizeof(header64));
    header_length :=
      sizeof(header64.magic) + sizeof(header64.version) +
      sizeof(header64.length) + sizeof(header64.unit_length);
  end;

  inc(header_length, header64.length);

  fillchar(numoptable{%H-}, sizeof(numoptable), #0);
  ReadNext(numoptable, header64.opcode_base-1);
  DEBUG_WRITELN('Opcode parameter count table');
  for i := 1 to header64.opcode_base-1 do begin
    DEBUG_WRITELN('Opcode[', i, '] - ', numoptable[i], ' parameters');
  end;

  DEBUG_WRITELN('Reading directories...');
  include_directories := Pos();
  SkipDirectories();
  DEBUG_WRITELN('Reading filenames...');
  file_names := Pos();
  SkipFilenames();

  Seek(header_length);

  with header64 do begin
    InitStateRegisters(state{%H-}, default_is_stmt);
  end;
  opcode := ReadNext();
  while (opcode <> -1) and (not found) do begin
    DEBUG_WRITELN('Next opcode: ');
    case (opcode) of
      { extended opcode }
      0 : begin
        extended_opcode_length := ReadULEB128();
        extended_opcode := ReadNext();
        case (extended_opcode) of
          DW_LNE_END_SEQUENCE : begin
            state.end_sequence := true;
            state.append_row := true;
            DEBUG_WRITELN('DW_LNE_END_SEQUENCE');
          end;
          DW_LNE_SET_ADDRESS : begin
            state.address := ReadAddress();
            DEBUG_WRITELN('DW_LNE_SET_ADDRESS (', hexstr(state.address, sizeof(state.address)*2), ')');
          end;
          DW_LNE_DEFINE_FILE : begin
            {$ifdef DEBUG_DWARF_PARSER}s := {$endif}ReadString();
            SkipLEB128();
            SkipLEB128();
            SkipLEB128();
            DEBUG_WRITELN('DW_LNE_DEFINE_FILE (', s, ')');
          end;
          else begin
            DEBUG_WRITELN('Unknown extended opcode (opcode ', extended_opcode, ' length ', extended_opcode_length, ')');
            for i := 0 to extended_opcode_length-2 do
              ReadNext();
          end;
        end;
      end;
      DW_LNS_COPY : begin
        state.basic_block := false;
        state.prolouge_end := false;
        state.epilouge_begin := false;
        state.append_row := true;
        DEBUG_WRITELN('DW_LNS_COPY');
      end;
      DW_LNS_ADVANCE_PC : begin
        inc(state.address, ReadULEB128() * header64.minimum_instruction_length);
        DEBUG_WRITELN('DW_LNS_ADVANCE_PC (', hexstr(state.address, sizeof(state.address)*2), ')');
      end;
      DW_LNS_ADVANCE_LINE : begin
        // inc(state.line, ReadLEB128()); negative values are allowed
        // but those may generate a range check error
        state.line := state.line + ReadLEB128();
        DEBUG_WRITELN('DW_LNS_ADVANCE_LINE (', state.line, ')');
      end;
      DW_LNS_SET_FILE : begin
        state.file_id := ReadULEB128();
        DEBUG_WRITELN('DW_LNS_SET_FILE (', state.file_id, ')');
      end;
      DW_LNS_SET_COLUMN : begin
        state.column := ReadULEB128();
        DEBUG_WRITELN('DW_LNS_SET_COLUMN (', state.column, ')');
      end;
      DW_LNS_NEGATE_STMT : begin
        state.is_stmt := not state.is_stmt;
        DEBUG_WRITELN('DW_LNS_NEGATE_STMT (', state.is_stmt, ')');
      end;
      DW_LNS_SET_BASIC_BLOCK : begin
        state.basic_block := true;
        DEBUG_WRITELN('DW_LNS_SET_BASIC_BLOCK');
      end;
      DW_LNS_CONST_ADD_PC : begin
        inc(state.address, CalculateAddressIncrement(255, header64));
        DEBUG_WRITELN('DW_LNS_CONST_ADD_PC (', hexstr(state.address, sizeof(state.address)*2), ')');
      end;
      DW_LNS_FIXED_ADVANCE_PC : begin
        inc(state.address, ReadUHalf());
        DEBUG_WRITELN('DW_LNS_FIXED_ADVANCE_PC (', hexstr(state.address, sizeof(state.address)*2), ')');
      end;
      DW_LNS_SET_PROLOGUE_END : begin
        state.prolouge_end := true;
        DEBUG_WRITELN('DW_LNS_SET_PROLOGUE_END');
      end;
      DW_LNS_SET_EPILOGUE_BEGIN : begin
        state.epilouge_begin := true;
        DEBUG_WRITELN('DW_LNS_SET_EPILOGUE_BEGIN');
      end;
      DW_LNS_SET_ISA : begin
        state.isa := ReadULEB128();
        DEBUG_WRITELN('DW_LNS_SET_ISA (', state.isa, ')');
      end;
      else begin { special opcode }
        if (opcode < header64.opcode_base) then begin
          DEBUG_WRITELN('Unknown standard opcode $', hexstr(opcode, 2), '; skipping');
          for i := 1 to numoptable[opcode] do
            SkipLEB128();
        end else begin
          adjusted_opcode := opcode - header64.opcode_base;
          addrIncrement := CalculateAddressIncrement(opcode, header64);
          inc(state.address, addrIncrement);
          lineIncrement := header64.line_base + (adjusted_opcode mod header64.line_range);
          inc(state.line, lineIncrement);
          DEBUG_WRITELN('Special opcode $', hexstr(opcode, 2), ' address increment: ', addrIncrement, ' new line: ', lineIncrement);
          state.basic_block := false;
          state.prolouge_end := false;
          state.epilouge_begin := false;
          state.append_row := true;
        end;
      end;
    end;

    if (state.append_row) then begin
      DEBUG_WRITELN('Current state : address = ', hexstr(state.address, sizeof(state.address) * 2),
      DEBUG_COMMENT ' file_id = ', state.file_id, ' line = ', state.line, ' column = ', state.column,
      DEBUG_COMMENT  ' is_stmt = ', state.is_stmt, ' basic_block = ', state.basic_block,
      DEBUG_COMMENT  ' end_sequence = ', state.end_sequence, ' prolouge_end = ', state.prolouge_end,
      DEBUG_COMMENT  ' epilouge_begin = ', state.epilouge_begin, ' isa = ', state.isa);

      if (first_row) then begin
        if (state.address > addr) then
          break;
        first_row := false;
      end;

      { when we have found the address we need to return the previous
        line because that contains the call instruction }
      if (state.address >= addr) then
        found:=true
      else
        begin
          { save line information }
          prev_file := state.file_id;
          prev_line := state.line;
        end;

      state.append_row := false;
      if (state.end_sequence) then begin
        InitStateRegisters(state, header64.default_is_stmt);
        first_row := true;
      end;
    end;

    opcode := ReadNext();
  end;

  if (found) then begin
    line := prev_line;
    source := GetFullFilename(file_names, include_directories, prev_file);
  end;
end;

function GetLineInfoDwarf(addr : ptruint; var func, source : string; var line : longint) : boolean;
var
  current_offset : QWord;
  end_offset : QWord;

  found : Boolean;

begin
  func := '';
  source := '';
  found := false;
  GetLineInfoDwarf:=false;
  if not e.isopen then exit;

  addr := addr - e.processaddress;

  current_offset := DwarfOffset;
  end_offset := DwarfOffset + DwarfSize;

  while (current_offset < end_offset) and (not found) do begin
    Init(current_offset, end_offset - current_offset);
    current_offset := ParseCompilationUnit(addr, current_offset,
      source, line, found);
  end;
  GetLineInfoDwarf:=found;
end;

{%endregion ********************* lnfodwrf ************************************}

{%region ********************* lineinfo ************************************}

const
  N_Function    = $24;
  N_TextLine    = $44;
  N_DataLine    = $46;
  N_BssLine     = $48;
  N_SourceFile  = $64;
  N_IncludeFile = $84;

  maxstabs = 40; { size of the stabs buffer }

var
  { GDB after 4.18 uses offset to function begin
    in text section but OS/2 version still uses 4.16 PM }
  StabsFunctionRelative: boolean;

type
  //pstab=^tstab;
  tstab=packed record
    strpos  : longint;
    ntype   : byte;
    nother  : byte;
    ndesc   : word;
    nvalue  : dword;
  end;

{ We use static variable so almost no stack is required, and is thus
  more safe when an error has occured in the program }
//{$WARNING This code is not thread-safe, and needs improvement }
var
  stabcnt,              { amount of stabs }
  stablen,
  stabofs,              { absolute stab section offset in executable }
  stabstrlen,
  stabstrofs : longint; { absolute stabstr section offset in executable }
  dirlength  : longint; { length of the dirctory part of the source file }
  stabs      : array[0..maxstabs-1] of tstab;  { buffer }
  funcstab,             { stab with current function info }
  linestab,             { stab with current line info }
  dirstab,              { stab with current directory info }
  filestab   : tstab;   { stab with current file info }

function GetLineInfoStabs(addr:ptruint;var func,source:string;var line:longint) : boolean;
var
  res,
  stabsleft,
  stabscnt,i : longint;
  found : boolean;
  lastfunc : tstab;
begin
  GetLineInfoStabs:=false;
{$ifdef DEBUG_LINEINFO}
  writeln(stderr,'GetLineInfo called');
{$endif DEBUG_LINEINFO}
  fillchar(func,high(func)+1,0);
  fillchar(source,high(source)+1,0);
  line:=0;
  if not e.isopen then exit;

  { correct the value to the correct address in the file }
  { processaddress is set in OpenStabs                   }
  addr := dword(addr - e.processaddress);

{$ifdef DEBUG_LINEINFO}
  writeln(stderr,'Addr: ',hexstr(addr,sizeof(addr)*2));
{$endif DEBUG_LINEINFO}

  fillchar(funcstab,sizeof(tstab),0);
  fillchar(filestab,sizeof(tstab),0);
  fillchar(dirstab,sizeof(tstab),0);
  fillchar(linestab,sizeof(tstab),0);
  fillchar(lastfunc{%H-},sizeof(tstab),0);
  found:=false;
  system.seek(e.f,stabofs);
  stabsleft:=stabcnt;
  repeat
    if stabsleft>maxstabs then
     stabscnt:=maxstabs
    else
     stabscnt:=stabsleft;
    blockread(e.f,stabs,stabscnt*sizeof(tstab),res{%H-});
    stabscnt:=res div sizeof(tstab);
    for i:=0 to stabscnt-1 do
     begin
       case stabs[i].ntype of
         N_BssLine,
         N_DataLine,
         N_TextLine :
           begin
             if (stabs[i].ntype=N_TextLine) and StabsFunctionRelative then
               inc(stabs[i].nvalue,lastfunc.nvalue);
             if (stabs[i].nvalue<=addr) and
                (stabs[i].nvalue>linestab.nvalue) then
              begin
                { if it's equal we can stop and take the last info }
                if stabs[i].nvalue=addr then
                 found:=true
                else
                 linestab:=stabs[i];
              end;
           end;
         N_Function :
           begin
             lastfunc:=stabs[i];
             if (stabs[i].nvalue<=addr) and
                (stabs[i].nvalue>funcstab.nvalue) then
              begin
                funcstab:=stabs[i];
                fillchar(linestab,sizeof(tstab),0);
              end;
           end;
         N_SourceFile,
         N_IncludeFile :
           begin
             if (stabs[i].nvalue<=addr) and
                (stabs[i].nvalue>=filestab.nvalue) then
              begin
                { if same value and type then the first one
                  contained the directory PM }
                if (stabs[i].nvalue=filestab.nvalue) and
                   (stabs[i].ntype=filestab.ntype) then
                  dirstab:=filestab
                else
                  fillchar(dirstab,sizeof(tstab),0);
                filestab:=stabs[i];
                fillchar(linestab,sizeof(tstab),0);
                { if new file then func is not valid anymore PM }
                if stabs[i].ntype=N_SourceFile then
                  begin
                    fillchar(funcstab,sizeof(tstab),0);
                    fillchar(lastfunc,sizeof(tstab),0);
                  end;
              end;
           end;
       end;
     end;
    dec(stabsleft,stabscnt);
  until found or (stabsleft=0);

{ get the line,source,function info }
  line:=linestab.ndesc;
  if dirstab.ntype<>0 then
   begin
     system.seek(e.f,stabstrofs+dirstab.strpos);
     blockread(e.f,source[1],high(source)-1,res);
     dirlength:=strlen(@source[1]);
     source[0]:=chr(dirlength);
   end
  else
   dirlength:=0;
  if filestab.ntype<>0 then
   begin
     system.seek(e.f,stabstrofs+filestab.strpos);
     blockread(e.f,source[dirlength+1],high(source)-(dirlength+1),res);
     source[0]:=chr(strlen(@source[1]));
   end;
  if funcstab.ntype<>0 then
   begin
     system.seek(e.f,stabstrofs+funcstab.strpos);
     blockread(e.f,func[1],high(func)-1,res);
     func[0]:=chr(strlen(@func[1]));
     i:=system.pos(':',func);
     if i>0 then
      Delete(func,i,255);
   end;
  GetLineInfoStabs:=found;
end;

{%endregion ********************* lineinfo ************************************}

function OpenSymbolFile(AFileName: string): boolean;
begin
  Result := False;
  HasStabs := False;
  HasDwarf := False;
  filename := AFileName;

  if not OpenExeFile(e,filename) then
    exit;
  if ReadDebugLink(e,dbgfn) then
    begin
      CloseExeFile(e);
      if not OpenExeFile(e,dbgfn) then
        exit;
    end;

  e.processaddress:=0;
//  e.processaddress:=ptruint(baseaddr)-e.processaddress;


  {%region ********************* lnfodwrf ************************************}
  if FindExeSection(e,'.debug_line',dwarfoffset,dwarfsize) then begin
    HasDwarf := True;
    Result:=true;
  end;
  {%endregion ********************* lnfodwrf ************************************}

  {%region ********************* lineinfo ************************************}
  StabsFunctionRelative := E.FunctionRelative;
  if FindExeSection(e,'.stab',stabofs,stablen) and
     FindExeSection(e,'.stabstr',stabstrofs,stabstrlen) then
  begin
    stabcnt:=stablen div sizeof(tstab);
    HasStabs := True;
    Result:=true;
  end;
  {%endregion ********************* lineinfo ************************************}
end;

procedure CloseSymbolFile;
begin
  CloseExeFile(e);
end;

function GetLineInfo(addr: ptruint; out func, source: string; out line: longint): boolean;
begin
  Result := False;
  if HasDwarf then
    Result := GetLineInfoDwarf(addr, func{%H-}, source{%H-}, line{%H-});
  if (not Result) and HasStabs then
    Result := GetLineInfoStabs(addr, func, source, line);
end;


end.

