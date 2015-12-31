(*******************************************************************************
                                 L I C E N S E
********************************************************************************

BESEN - A ECMAScript Fifth Edition Object Pascal Implementation
Copyright (C) 2009-2015, Benjamin 'BeRo' Rosseaux

The source code of the BESEN ecmascript engine library and helper tools are 
distributed under the Library GNU Lesser General Public License Version 2.1 
(see the file copying.txt) with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,
and to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a module
which is not derived from or based on this library. If you modify this
library, you may extend this exception to your version of the library, but you 
are not obligated to do so. If you do not wish to do so, delete this exception
statement from your version.

If you didn't receive a copy of the license, see <http://www.gnu.org/licenses/>
or contact:
      Free Software Foundation
      675 Mass Ave
      Cambridge, MA  02139
      USA

*******************************************************************************)
unit BESENRegExpCache;
{$i BESEN.inc}

interface

uses SysUtils,BESENConstants,BESENTypes,BESENValue,BESENBaseObject,BESENRegExp;

type TBESENRegExpCacheItems=array of TBESENRegExp;

     TBESENRegExpCache=class(TBESENBaseObject)
      private
       HashSize:longword;
       HashSizeMask:longword;
       HashItems:TBESENRegExpCacheItems;
       procedure SetCacheSize(NewSize:longword);
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       function Hash(const Source:TBESENString;Flags:TBESENRegExpFlags):TBESENHash;
       function Get(const Source:TBESENString;Flags:TBESENRegExpFlags):TBESENRegExp;
       function IsCached(RegExp:TBESENRegExp):boolean;
      published
       property CacheSize:longword read HashSize write SetCacheSize;
     end;

implementation

uses BESEN,BESENUtils,BESENStringUtils,BESENErrors,BESENHashUtils;

constructor TBESENRegExpCache.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 HashItems:=nil;
 SetCacheSize(BESENRegExpCacheSize);
end;

destructor TBESENRegExpCache.Destroy;
var i:integer;
begin
 for i:=0 to length(HashItems)-1 do begin
  if assigned(HashItems[i]) then begin
   BesenFreeAndNil(HashItems[i]);
   HashItems[i]:=nil;
  end;
 end;
 SetLength(HashItems,0);
 inherited Destroy;
end;

procedure TBESENRegExpCache.SetCacheSize(NewSize:longword);
var i:integer;
begin
 for i:=0 to length(HashItems)-1 do begin
  if assigned(HashItems[i]) then begin
   HashItems[i].DecRef;
   HashItems[i]:=nil;
  end;
 end;
 HashSize:=BESENRoundUpToPowerOfTwo(NewSize);
 HashSizeMask:=HashSize-1;
 SetLength(HashItems,HashSize);
 for i:=0 to length(HashItems)-1 do begin
  HashItems[i]:=nil;
 end;
end;

function TBESENRegExpCache.Hash(const Source:TBESENString;Flags:TBESENRegExpFlags):TBESENHash;
begin
 result:=BESENHashKey(Source);
 if brefGLOBAL in Flags then begin
  result:=result+4;
 end;
 if brefIGNORECASE in Flags then begin
  result:=result+2;
 end;
 if brefMULTILINE in Flags then begin
  result:=result+1;
 end;
end;

function TBESENRegExpCache.Get(const Source:TBESENString;Flags:TBESENRegExpFlags):TBESENRegExp;
var HashValue:TBESENUINT32;
begin
 if HashSize>0 then begin
  HashValue:=Hash(Source,Flags) and HashSizeMask;
  result:=HashItems[HashValue];
  if (assigned(result) and ((result.Source<>Source) or (result.Flags<>Flags))) or not assigned(result) then begin
   result:=TBESENRegExp.Create(Instance);
   try
    result.Compile(Source,Flags);
    try
     if assigned(HashItems[HashValue]) then begin
      HashItems[HashValue].DecRef;
     end;
     HashItems[HashValue]:=result;
     result.IncRef;
    except
     HashItems[HashValue]:=nil;
     raise;
    end;
   except
    BESENFreeAndNil(result);
    raise;
   end;
  end;
 end else begin
  result:=TBESENRegExp.Create(Instance);
  try
   result.Compile(Source,Flags);
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
end;

function TBESENRegExpCache.IsCached(RegExp:TBESENRegExp):boolean;
begin
 result:=(HashSize>0) and (assigned(RegExp) and (RegExp=HashItems[Hash(RegExp.Source,RegExp.Flags) and HashSizeMask]));
end;

end.
