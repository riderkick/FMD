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
unit BESENEvalCache;
{$i BESEN.inc}

interface

uses SysUtils,BESENConstants,BESENTypes,BESENValue,BESENBaseObject,BESENASTNodes,
     BESENEvalCacheItem;

type TBESENEvalCache=class(TBESENBaseObject)
      private
       procedure SetCacheSize(NewSize:longword);
      public
       HashSize:longword;
       HashSizeMask:longword;
       HashItems:TBESENEvalCacheItems;
       MaxSourceLength:integer;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       function Hash(const Source:TBESENString;CallerStrict:TBESENBoolean):TBESENHash;
       function Get(const Source:TBESENString;CallerStrict:TBESENBoolean):TBESENEvalCacheItem;
      published
       property CacheSize:longword read HashSize write SetCacheSize;
     end;

implementation

uses BESEN,BESENUtils,BESENStringUtils,BESENErrors,BESENHashUtils;

constructor TBESENEvalCache.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 HashItems:=nil;
 SetCacheSize(BESENEvalCacheSize);
 MaxSourceLength:=BESENEvalCacheMaxSourceLength;
end;

destructor TBESENEvalCache.Destroy;
var i:integer;
begin
 for i:=0 to length(HashItems)-1 do begin
  if assigned(HashItems[i]) then begin
   BESENFreeAndNil(HashItems[i]);
   HashItems[i]:=nil;
  end;
 end;
 SetLength(HashItems,0);
 inherited Destroy;
end;

procedure TBESENEvalCache.SetCacheSize(NewSize:longword);
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

function TBESENEvalCache.Hash(const Source:TBESENString;CallerStrict:TBESENBoolean):TBESENHash;
begin
 result:=BESENHashKey(Source);
 if CallerStrict then begin
  result:=not (result+1);
 end;
end;

function TBESENEvalCache.Get(const Source:TBESENString;CallerStrict:TBESENBoolean):TBESENEvalCacheItem;
var HashValue:TBESENUINT32;
    Node:TBESENASTNode;
begin
 if HashSize>0 then begin
  HashValue:=Hash(Source,CallerStrict) and HashSizeMask;
  result:=HashItems[HashValue];
  if (assigned(result) and ((result.Source<>Source) or (result.CallerStrict<>CallerStrict))) or not assigned(result) then begin
   result:=TBESENEvalCacheItem.Create(Instance);
   try
    result.Source:=Source;
    result.CallerStrict:=CallerStrict;
    Node:=TBESEN(Instance).Compile({$ifndef BESENSingleStringType}BESENUTF16ToUTF8({$endif}Source{$ifndef BESENSingleStringType}){$endif});
    if (not assigned(Node)) or not (Node is TBESENASTNodeProgram) then begin
     BESENThrowError('No program');
    end;
    result.Node:=TBESENASTNodeProgram(Node);
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
  result:=nil;
 end;
end;

end.
