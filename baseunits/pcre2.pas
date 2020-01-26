{ pcre2

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


unit pcre2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pcre2lib;

function PCRE2Match(const Expression,InputString:String):Boolean;
function PCRE2Replace(const Expression,InputString,Replacement:String;ReplaceAll:Boolean=False):String;
function PCRE2Version:String;

implementation

function PCRE2GetErrorMessage(const error_code:Integer;const error_offset:Integer = -1):String;
begin
  setlength(result,MAX_PATH);
  setlength(result,pcre2_get_error_message_8(error_code,PAnsiChar(result),MAX_PATH));
  result:='PCRE2 error '+inttostr(error_code)+': '+result;
  if error_offset>0 then
    result:=result+' at '+IntToStr(error_offset)+' position';
end;

function PCRE2Match(const Expression,InputString:String):Boolean;
var
  re:ppcre2_code_8;
  error_code,error_offset:Integer;
  match_data:ppcre2_match_data_8;
begin
  result:=false;
  if PCRE2LibHandle=0 then exit;
  re:=pcre2_compile_8(PAnsiChar(Expression),Length(Expression),PCRE2_UTF,@error_code,@error_offset,nil);
  if re<>nil then begin
    match_data:=pcre2_match_data_create_from_pattern_8(re,nil);
    error_code:=pcre2_match_8(re,@InputString[1],Length(InputString),0,0,match_data,nil);
    pcre2_match_data_free_8(match_data);
    pcre2_code_free_8(re);
    if error_code>=0 then
      result:=true
    else
      raise Exception.Create(PCRE2GetErrorMessage(error_code));
  end
  else
    raise Exception.Create(PCRE2GetErrorMessage(error_code,error_offset));
end;

function PCRE2Replace(const Expression,InputString,Replacement:String;ReplaceAll:Boolean=False):String;
var
  re:ppcre2_code_8;
  options:PCRE2_OPTIONS=PCRE2_SUBSTITUTE_OVERFLOW_LENGTH;
  error_code,error_offset,result_len:Integer;

  procedure dosubstitute;
  begin
    error_code:=pcre2_substitute_8(re,PAnsiChar(InputString),Length(InputString),
      0,options,nil,nil,PAnsiChar(Replacement),length(Replacement),PAnsiChar(result),@result_len);
    if error_code=PCRE2_ERROR_NOMEMORY then
    begin
      setlength(result,result_len);
      dosubstitute;
    end
    else
      setlength(result,result_len);
  end;

begin
  result:=InputString;
  if PCRE2LibHandle=0 then exit;
  re:=pcre2_compile_8(PAnsiChar(Expression),Length(Expression),PCRE2_UTF,@error_code,@error_offset,nil);
  if re<>nil then begin
    if ReplaceAll then
      options:=options+PCRE2_SUBSTITUTE_GLOBAL;
    setlength(result,MAX_PATH);
    result_len:=MAX_PATH;
    dosubstitute;
    pcre2_code_free_8(re);
    if error_code<0 then
      raise Exception.Create(PCRE2GetErrorMessage(error_code));
  end
  else
    raise Exception.Create(PCRE2GetErrorMessage(error_code,error_offset));
end;

function PCRE2Version: String;
begin
  result:='';
  if PCRE2LibHandle=0 then exit;
  setlength(result,MAX_PATH);
  setlength(result,pcre2_config_8(PCRE2_CONFIG_VERSION,PAnsiChar(result))-1);
end;

end.

