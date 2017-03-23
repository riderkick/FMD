unit BaseCrypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base64, DCPrijndael, DCPsha256;

function Pkcs7AddPad(const s: String): String;
function Pkcs7RemovePad(const s: String): String;
function AESEncrpytCBCSHA256Base64Pkcs7(const s, key: String; const IV): string;
function AESDecryptCBCSHA256Base64Pkcs7(const s, key: String; const IV): string;

implementation

// Pkcs7 padding described in RFC 5652 https://tools.ietf.org/html/rfc5652#section-6.3
function Pkcs7AddPad(const s: String): String;
var
  l: Integer;
begin
  Result:=s;
  l:=16-(Length(s) and 15);
  if l>0 then
    result += StringOfChar(Char(l),l);
end;

function Pkcs7RemovePad(const s: String): String;
begin
  Result:=s;
  SetLength(Result,Length(Result)-Ord(Result[Length(Result)]));
end;

function AESEncrpytCBCSHA256Base64Pkcs7(const s, key: String; const IV): string;
var
  i: String;
begin
  Result:='';
  with TDCP_rijndael.Create(nil) do
    try
      InitStr(key,TDCP_sha256);
      SetIV(IV);
      i:=Pkcs7AddPad(s);
      SetLength(Result,Length(i));
      EncryptCBC(i[1],Result[1],Length(i));
      Burn;
      Result:=EncodeStringBase64(Result);
    finally
      Free;
    end;
end;

function AESDecryptCBCSHA256Base64Pkcs7(const s, key: String; const IV): string;
var
  i: String;
begin
  Result:='';
  with TDCP_rijndael.Create(nil) do
    try
      InitStr(key,TDCP_sha256);
      SetIV(IV);
      i:=DecodeStringBase64(s);
      SetLength(Result,Length(i));
      DecryptCBC(i[1],Result[1],Length(i));
      Burn;
      Result:=Pkcs7RemovePad(Result);
    finally
      Free;
    end;
end;

end.

