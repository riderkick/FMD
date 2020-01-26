{ pcre2lib

  Copyright (C) 2020 riderkick

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit pcre2lib;

{$mode delphi}

interface

uses
  Classes, SysUtils, DynLibs, ctypes;

type
  PPCRE2_UCHAR8 = ^PCRE2_UCHAR8;
  PCRE2_UCHAR8 = Char;
  PCRE2_SPTR8 = ^PCRE2_UCHAR8;

  PPCRE2_SIZE = ^PCRE2_SIZE;
  PCRE2_SIZE = SIZE_T;

  PCRE2_OPTIONS = cuint32;

  pcre2_real_compile_context_8 = Pointer;
  ppcre2_compile_context_8 = ^pcre2_compile_context_8;
  pcre2_compile_context_8 = pcre2_real_compile_context_8;

  pcre2_real_code_8 = Pointer;
  ppcre2_code_8 = ^pcre2_code_8;
  pcre2_code_8 = pcre2_real_code_8;

  pcre2_real_match_data_8 = Pointer;
  ppcre2_match_data_8 = ^pcre2_match_data_8;
  pcre2_match_data_8 = pcre2_real_match_data_8;

  pcre2_real_general_context_8 = Pointer;
  ppcre2_general_context_8 = ^pcre2_general_context_8;
  pcre2_general_context_8 = pcre2_real_general_context_8;

  pcre2_real_match_context_8=Pointer;
  ppcre2_match_context_8 = ^pcre2_match_context_8;
  pcre2_match_context_8 = pcre2_real_match_context_8;


const
  { options }
  PCRE2_UTF =                                                         $00080000;
  PCRE2_DUPNAMES =                                                    $00000040;
  PCRE2_ZERO_TERMINATED =                                     not PCRE2_SIZE(0);

  { substitute options }
  PCRE2_SUBSTITUTE_GLOBAL =                                           $00000100;
  PCRE2_SUBSTITUTE_EXTENDED =                                         $00000200;
  PCRE2_SUBSTITUTE_UNSET_EMPTY =                                      $00000400;
  PCRE2_SUBSTITUTE_UNKNOWN_UNSET =                                    $00000800;
  PCRE2_SUBSTITUTE_OVERFLOW_LENGTH =                                  $00001000;

  { error code }
  PCRE2_ERROR_NOMEMORY =                                                    -48;

  { pcre2 config }
  PCRE2_CONFIG_VERSION =                                                     11;

var
  PCRE2LibName:String{$IFDEF MSWINDOWS}='libpcre2-8.dll'{$ENDIF}{$IFDEF UNIX}='libpcre2.so'{$ENDIF};
  PCRE2LibHandle:TLibHandle=NilHandle;

  pcre2_compile_8:function(pattern:PCRE2_SPTR8;length:PCRE2_SIZE;options:PCRE2_OPTIONS;
    errorcode:PInteger;erroroffset:PPCRE2_SIZE;ccontext:ppcre2_compile_context_8):ppcre2_code_8;cdecl;
  pcre2_match_data_create_from_pattern_8:function(const code:ppcre2_code_8;gcontext:ppcre2_general_context_8):
    ppcre2_match_data_8;cdecl;
  pcre2_match_8:function(const code:ppcre2_code_8;subject:PCRE2_SPTR8;length:PCRE2_SIZE;startoffset:PCRE2_SIZE;
    options:PCRE2_OPTIONS;match_data:ppcre2_match_data_8;mcontext:ppcre2_match_context_8):Integer;cdecl;

  pcre2_substitute_8:function(const code:ppcre2_code_8;subject:PCRE2_SPTR8;length:PCRE2_SIZE;
    startoffset:PCRE2_SIZE;options:PCRE2_OPTIONS;match_data:ppcre2_match_data_8;mcontext:ppcre2_match_context_8;
    replacement:PCRE2_SPTR8;rlength:PCRE2_SIZE;outputbuffer:PPCRE2_UCHAR8;outlengthptr:PPCRE2_SIZE):Integer; cdecl;

  pcre2_match_data_free_8:procedure(match_data:ppcre2_match_data_8);cdecl;
  pcre2_code_free_8:procedure(code:ppcre2_code_8);cdecl;

  pcre2_get_error_message_8:function(errorcode:Integer;buffer:PPCRE2_UCHAR8;
    bufflen:PCRE2_SIZE):Integer;cdecl;

  pcre2_config_8:function(what:cuint32;where:Pointer):Integer;cdecl;

implementation

procedure InitPCRE2Lib;
begin
  if PCRE2LibHandle<>NilHandle then Exit;
  PCRE2LibHandle:=LoadLibrary(PCRE2LibName);
  if PCRE2LibHandle=NilHandle then Exit;
  @pcre2_compile_8:=GetProcedureAddress(PCRE2LibHandle,'pcre2_compile_8');
  @pcre2_match_data_create_from_pattern_8:=GetProcedureAddress(PCRE2LibHandle,'pcre2_match_data_create_from_pattern_8');
  @pcre2_match_8:=GetProcedureAddress(PCRE2LibHandle,'pcre2_match_8');
  @pcre2_substitute_8:=GetProcedureAddress(PCRE2LibHandle,'pcre2_substitute_8');
  @pcre2_match_data_free_8:=GetProcedureAddress(PCRE2LibHandle,'pcre2_match_data_free_8');
  @pcre2_code_free_8:=GetProcedureAddress(PCRE2LibHandle,'pcre2_code_free_8');
  @pcre2_get_error_message_8:=GetProcedureAddress(PCRE2LibHandle,'pcre2_get_error_message_8');
  @pcre2_config_8:=GetProcedureAddress(PCRE2LibHandle,'pcre2_config_8');
end;

procedure DestroyPCRE2Lib;
begin
  if PCRE2LibHandle=NilHandle then Exit;
  @pcre2_compile_8:=nil;
  @pcre2_match_data_create_from_pattern_8:=nil;
  @pcre2_match_8:=nil;
  @pcre2_substitute_8:=nil;
  @pcre2_match_data_free_8:=nil;
  @pcre2_code_free_8:=nil;
  @pcre2_get_error_message_8:=nil;
  @pcre2_config_8:=nil;
  FreeLibrary(PCRE2LibHandle);
end;

initialization
  InitPCRE2Lib;

finalization
  DestroyPCRE2Lib;

end.

