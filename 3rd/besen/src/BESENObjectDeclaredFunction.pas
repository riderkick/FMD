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
unit BESENObjectDeclaredFunction;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENObject,BESENObjectFunction,BESENValue,BESENObjectPropertyDescriptor,
     BESENLexicalEnvironment,BESENContext,BESENASTNodes;

type TBESENObjectDeclaredFunctionParameters=array of TBESENString;

     TBESENObjectDeclaredFunction=class(TBESENObjectFunction)
      public
       LexicalEnvironment:TBESENLexicalEnvironment;
       Node:TBESENASTNodeFunctionLiteral;
       Parameters:TBESENObjectDeclaredFunctionParameters;
       Container:TBESENFunctionLiteralContainer;
       ContextCache:TBESENContextCache;
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       function GetEx(const P:TBESENString;var AResult:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean; override;
       function GetIndex(const Index,ID:integer;var AResult:TBESENValue;Base:TBESENObject=nil):boolean; override;
       procedure Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       procedure CallEx(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue;IsConstruct:boolean); virtual;
       function HasConstruct:TBESENBoolean; override;
       function HasCall:TBESENBoolean; override;
       procedure Finalize; override;
       procedure Mark; override;
     end;

implementation

uses BESEN,BESENErrors,BESENUtils,BESENCode,BESENDeclarativeEnvironmentRecord;

constructor TBESENObjectDeclaredFunction.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Function';
 ObjectName:='declared';
 LexicalEnvironment:=nil;
 Node:=nil;
 Parameters:=nil;
 Container:=nil;
 ContextCache:=TBESENContextCache.Create(Instance);
end;

destructor TBESENObjectDeclaredFunction.Destroy;
begin
 SetLength(Parameters,0);
 BESENFreeAndNil(ContextCache);
 inherited Destroy;
end;

function TBESENObjectDeclaredFunction.GetEx(const P:TBESENString;var AResult:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean;
begin
 if not assigned(Base) then begin
  Base:=self;
 end;
 result:=inherited GetEx(P,AResult,Descriptor,Base,Hash);
 if Node.Body.IsStrict and (P='caller') then begin
  raise EBESENTypeError.Create('"caller" not allowed here');
 end;
end;

function TBESENObjectDeclaredFunction.GetIndex(const Index,ID:integer;var AResult:TBESENValue;Base:TBESENObject=nil):boolean;
begin
 if not assigned(Base) then begin
  Base:=self;
 end;
 result:=inherited GetIndex(Index,ID,AResult,Base);
 if TBESEN(Instance).IsStrict and (ID=TBESEN(Instance).KeyIDManager.CallerID) then begin
  raise EBESENTypeError.Create('"caller" not allowed here');
 end;
end;

procedure TBESENObjectDeclaredFunction.Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
var r1:TBESENObject;
    r3:TBESENValue;
begin
 Get('prototype',r3);
 if (r3.ValueType=bvtOBJECT) and assigned(TBESENObject(r3.Obj)) then begin
  r1:=TBESENObject.Create(Instance,TBESENObject(r3.Obj),false);
 end else begin
  r1:=TBESENObject.Create(Instance,TBESEN(Instance).ObjectPrototype,false);
 end;
 TBESEN(Instance).GarbageCollector.Add(r1);
 r1.GarbageCollectorLock;
 try
  r1.Extensible:=true;
  CallEx(BESENObjectValue(r1),Arguments,CountArguments,AResult,true);
 finally
  r1.GarbageCollectorUnlock;
 end;
 if AResult.ValueType<>bvtOBJECT then begin
  AResult.ValueType:=bvtOBJECT;
  AResult.Obj:=r1;
 end;
end;

procedure TBESENObjectDeclaredFunction.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
begin
 CallEx(ThisArgument,Arguments,CountArguments,AResult,false);
end;

procedure TBESENObjectDeclaredFunction.CallEx(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue;IsConstruct:boolean);
var NewContext:TBESENContext;
    LocalEnv:TBESENLexicalEnvironment;
 procedure SetThisBinding;
 begin
  if Node.Body.IsStrict then begin
   BESENCopyValue(NewContext.ThisBinding,ThisArgument);
  end else if ThisArgument.ValueType in [bvtUNDEFINED,bvtNULL] then begin
   NewContext.ThisBinding.ValueType:=bvtOBJECT;
   NewContext.ThisBinding.Obj:=TBESEN(Instance).ObjectGlobal;
  end else if ThisArgument.ValueType<>bvtOBJECT then begin
   TBESEN(Instance).ToObjectValue(ThisArgument,NewContext.ThisBinding);
  end else begin
   BESENCopyValue(NewContext.ThisBinding,ThisArgument);
  end;
 end;
begin
 if assigned(Node) and (assigned(Node.Body) and not (Node.Body.IsEmpty and (CountArguments=0))) then begin
  GarbageCollectorLock;
  try
   NewContext:=ContextCache.Pop;
   if assigned(NewContext) then begin
    LocalEnv:=NewContext.VariableEnvironment;
   end else begin
    NewContext:=TBESENContext.Create(Instance);
    LocalEnv:=nil;
   end;
   if assigned(LocalEnv) then begin
    LocalEnv.Outer:=LexicalEnvironment;
    NewContext.LexicalEnvironment:=LocalEnv;
    SetThisBinding;
    NewContext.InitializeDeclarationBindingInstantiation(Node.Body,self,false,Arguments,CountArguments,true);
   end else begin
    LocalEnv:=TBESEN(Instance).NewDeclarativeEnvironment(LexicalEnvironment,Node.Body.IsStrict,TBESENCode(Node.Body.Code).HasMaybeDirectEval);
    TBESEN(Instance).GarbageCollector.Add(LocalEnv);
    NewContext.LexicalEnvironment:=LocalEnv;
    NewContext.VariableEnvironment:=LocalEnv;
    SetThisBinding;
    NewContext.InitializeDeclarationBindingInstantiation(Node.Body,self,false,Arguments,CountArguments,false);
    if not IsConstruct then begin
     TBESEN(Instance).GarbageCollector.TriggerCollect;
    end;
   end;
   try
    if Node.Body.IsEmpty then begin
     AResult.ValueType:=bvtUNDEFINED;
    end else begin
     Node.ExecuteCode(NewContext,AResult);
    end;
    if (ContextCache.Count<TBESEN(Instance).MaxCountOfFreeContexts) and (Node.Body.DisableArgumentsObject and ((length(Node.Body.Functions)=0) and (assigned(Node.Body.Code) and ((TBESENCode(Node.Body.Code).CountFunctionLiteralContainers=0) and not (TBESENCode(Node.Body.Code).IsComplexFunction or TBESENCode(Node.Body.Code).HasLocalDelete))))) then begin
     NewContext.Reset;
     if (((assigned(LocalEnv) and (NewContext.VariableEnvironment=LocalEnv){ and (LocalEnv.Outer=Instance.GlobalLexicalEnvironment)}) and assigned(LocalEnv.EnvironmentRecord)) and (LocalEnv.EnvironmentRecord is TBESENDeclarativeEnvironmentRecord)) and not TBESENDeclarativeEnvironmentRecord(LocalEnv.EnvironmentRecord).Touched then begin
      LocalEnv.Outer:=nil;
      TBESENDeclarativeEnvironmentRecord(LocalEnv.EnvironmentRecord).Reset;
      ContextCache.Push(NewContext);
      NewContext:=nil;
     end;
    end;
   finally
    NewContext.Free;
   end;
  finally
   GarbageCollectorUnlock;
  end;
 end else begin
  AResult.ValueType:=bvtUNDEFINED;
 end;
end;

function TBESENObjectDeclaredFunction.HasConstruct:TBESENBoolean;
begin
 result:=true;
end;

function TBESENObjectDeclaredFunction.HasCall:TBESENBoolean;
begin
 result:=true;
end;

procedure TBESENObjectDeclaredFunction.Finalize;
begin
 Container:=nil;
 LexicalEnvironment:=nil;
 inherited Finalize;
end;

procedure TBESENObjectDeclaredFunction.Mark;
begin
 if assigned(Container) then begin
  TBESEN(Instance).GarbageCollector.GrayIt(Container);
 end;
 if assigned(LexicalEnvironment) then begin
  TBESEN(Instance).GarbageCollector.GrayIt(LexicalEnvironment);
 end;
 if assigned(ContextCache) then begin
  ContextCache.Mark;
 end;
 inherited Mark;
end;

end.
