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
unit BESENObjectFunctionConstructor;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENObject,BESENObjectFunction,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectFunctionConstructor=class(TBESENObjectFunction)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       function HasConstruct:TBESENBoolean; override;
       function HasCall:TBESENBoolean; override;
     end;

implementation

uses BESEN,BESENErrors,BESENASTNodes,BESENStringUtils,BESENUtils,BESENObjectDeclaredFunction;

constructor TBESENObjectFunctionConstructor.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
end;

destructor TBESENObjectFunctionConstructor.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectFunctionConstructor.Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
var Body,Parameters:TBESENString;
    i:integer;
    Node:TBESENASTNode;
    f:TBESENObjectDeclaredFunction;
    OldIsStrict:boolean;
begin
 Body:='';
 Parameters:='';
 OldIsStrict:=TBESEN(Instance).IsStrict;
 try
  if CountArguments>0 then begin
   for i:=0 to CountArguments-2 do begin
    if i>0 then begin
     Parameters:=Parameters+',';
    end;
    Parameters:=Parameters+TBESEN(Instance).ToStr(Arguments^[i]^);
   end;
   Body:=TBESEN(Instance).ToStr(Arguments[CountArguments-1]^);
  end;
  TBESEN(Instance).IsStrict:=false;
  Node:=TBESEN(Instance).Compile({$ifndef BESENSingleStringType}BESENUTF16ToUTF8({$endif}Body{$ifndef BESENSingleStringType}),BESENUTF16ToUTF8({$endif}Parameters{$ifndef BESENSingleStringType}){$endif},true);
  if assigned(Node) and (Node is TBESENASTNodeFunctionExpression) then begin
   if assigned(TBESENASTNodeFunctionExpression(Node).Container.Literal.Name) then begin
    if TBESEN(Instance).IsStrict then begin
     if TBESENASTNodeFunctionExpression(Node).Container.Literal.Name.Name='eval' then begin
      raise EBESENSyntaxError.Create('"eval" not allowed here');
     end;
    end;
    f:=TBESEN(Instance).MakeFunction(TBESENASTNodeFunctionExpression(Node).Container.Literal,TBESENASTNodeFunctionExpression(Node).Container.Literal.Name.Name,TBESEN(Instance).GlobalLexicalEnvironment);
   end else begin
    f:=TBESEN(Instance).MakeFunction(TBESENASTNodeFunctionExpression(Node).Container.Literal,'',TBESEN(Instance).GlobalLexicalEnvironment);
   end;
   TBESEN(Instance).GarbageCollector.Add(f);
   AResult.ValueType:=bvtOBJECT;
   AResult.Obj:=f;
  end else begin
   AResult.ValueType:=bvtUNDEFINED;
  end;
  BESENFreeAndNil(Node);
 finally
  TBESEN(Instance).IsStrict:=OldIsStrict;
 end;
end;

procedure TBESENObjectFunctionConstructor.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
begin
 Construct(ThisArgument,Arguments,CountArguments,AResult);
end;

function TBESENObjectFunctionConstructor.HasConstruct:TBESENBoolean;
begin
 result:=true;
end;

function TBESENObjectFunctionConstructor.HasCall:TBESENBoolean;
begin
 result:=true;
end;


end.
 