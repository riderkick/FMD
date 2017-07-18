unit BaseCrypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base64, DCPrijndael, DCPsha256;

function HexToStr(const h: String): String;
procedure HexToBytes(const h: String; var o :TBytes);
function BytesToHex(const h: TBytes): String;
function JSHexToStr(const h: String): String;

function Pkcs7AddPad(const s: String): String;
function Pkcs7RemovePad(const s: String): String;
function AESEncrpytCBCSHA256Base64Pkcs7(const s, key, iv: String): string;
function AESDecryptCBCSHA256Base64Pkcs7(const s, key, iv: String): string;

implementation

function HexToStr(const h: String): String;
var
  i: Integer;
begin
  SetLength(Result,Length(h) div 2);
  for i:=1 to Length(Result) do
    Result[i]:=Char(StrToInt('$'+Copy(h,(i*2)-1,2)));
end;

procedure HexToBytes(const h: String; var o :TBytes);
var
  i, l: Integer;
begin
  l:=Length(h) div 2;
  SetLength(o,l);
  for i:=Low(o) to High(o) do
    o[i]:=Byte(StrToInt('$'+Copy(h,(i*2)+1,2)));
end;

function BytesToHex(const h: TBytes): String;
var
  i: Integer;
begin
  Result:='';
  for i:=Low(h) to High(h) do
    Result+=hexStr(h[i],2);
end;

function JSHexToStr(const h: String): String;
begin
  Result := HexToStr(StringReplace(h, '\x', '', [rfIgnoreCase, rfReplaceAll]));
end;

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

function AESEncrpytCBCSHA256Base64Pkcs7(const s, key, iv: String): string;
var
  i: String;
  ivb: TBytes;
begin
  Result:='';
  with TDCP_rijndael.Create(nil) do
  begin
    try
      InitStr(key,TDCP_sha256);
      HexToBytes(iv,ivb);
      SetIV(ivb[0]);
      i:=Pkcs7AddPad(s);
      SetLength(Result,Length(i));
      EncryptCBC(i[1],Result[1],Length(i));
      Burn;
      Result:=EncodeStringBase64(Result);
    except
    end;
    Free;
  end;
end;

function AESDecryptCBCSHA256Base64Pkcs7(const s, key, iv: String): string;
var
  i: String;
  ivb: TBytes;
begin
  Result:='';
  with TDCP_rijndael.Create(nil) do
  begin
    try
      InitStr(key,TDCP_sha256);
      HexToBytes(iv,ivb);
      SetIV(ivb[0]);
      i:=DecodeStringBase64(s);
      SetLength(Result,Length(i));
      DecryptCBC(i[1],Result[1],Length(i));
      Burn;
      Result:=Pkcs7RemovePad(Result);
    except
    end;
    Free;
  end;
end;

end.

