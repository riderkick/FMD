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
unit BESENContext;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENValue,BESENBaseObject,
     BESENCollectorObject,BESENObjectPropertyDescriptor,
     BESENLexicalEnvironment,BESENASTNodes,
     BESENEnvironmentRecord,BESENStringTree,
     BESENObjectFunctionArguments;

type TBESENContextCache=class;

     TBESENContext=class(TBESENCollectorObject)
      private
       Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;
       Temp:TBESENValue;
      public
       Cache:TBESENContextCache;
       CachePrevious,CacheNext:TBESENContext;
       Previous,Next:TBESENContext;
       CodeContext:TObject;
       LexicalEnvironment:TBESENLexicalEnvironment;
       VariableEnvironment:TBESENLexicalEnvironment;
       ThisBinding:TBESENValue;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure Reset;
       function CreateArgumentsObject(const Body:TBESENASTNodeFunctionBody;const FunctionObject:TObject;Arguments:PPBESENValues;CountArguments:integer;const Env:TBESENEnvironmentRecord;const IsStrict:longbool):TBESENObjectFunctionArguments;
       procedure InitializeDeclarationBindingInstantiation(const Body:TBESENASTNodeFunctionBody;const FunctionObject:TObject;const IsEval:boolean;Arguments:PPBESENValues;CountArguments:integer;const Reinitialize:boolean);
       procedure Mark;
     end;

     TBESENContextCacheItems=array of TBESENContext;

     TBESENContextCache=class(TBESENCollectorObject)
      public
       First,Last:TBESENContext;
       Count:integer;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure Clear;
       procedure Cleanup;
       procedure Push(Context:TBESENContext);
       function Pop:TBESENContext;
       procedure Mark;
     end;

implementation

uses BESEN,BESENUtils,BESENDeclarativeEnvironmentRecord,BESENObject,
     BESENArrayUtils,BESENCodeContext,BESENErrors,
     BESENObjectDeclaredFunction,BESENObjectArgGetterFunction,
     BESENObjectArgSetterFunction;

constructor TBESENContext.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 Cache:=nil;
 CachePrevious:=nil;
 CacheNext:=nil;
 Previous:=TBESEN(Instance).ContextLast;
 Next:=nil;
 if assigned(Previous) then begin
  Previous.Next:=self;
 end else begin
  TBESEN(Instance).ContextFirst:=self;
 end;
 TBESEN(Instance).ContextLast:=self;
 CodeContext:=nil;
 LexicalEnvironment:=nil;
 VariableEnvironment:=nil;
 ThisBinding.ValueType:=bvtUNDEFINED;
end;

destructor TBESENContext.Destroy;
begin
 if assigned(Instance) and assigned(TBESEN(Instance).GarbageCollector) and (TBESEN(Instance).GarbageCollector.CurrentContext=self) then begin
  TBESEN(Instance).GarbageCollector.CurrentContext:=Next;
 end;
 if assigned(Previous) then begin
  Previous.Next:=Next;
 end else if TBESEN(Instance).ContextFirst=self then begin
  TBESEN(Instance).ContextFirst:=Next;
 end;
 if assigned(Next) then begin
  Next.Previous:=Previous;
 end else if TBESEN(Instance).ContextLast=self then begin
  TBESEN(Instance).ContextLast:=Previous;
 end;
 Next:=nil;
 Previous:=nil;
 BESENFreeAndNil(CodeContext);
 if assigned(Cache) then begin
  dec(Cache.Count);
  if assigned(CachePrevious) then begin
   CachePrevious.CacheNext:=CacheNext;
  end else if Cache.First=self then begin
   Cache.First:=CacheNext;
  end;
  if assigned(CacheNext) then begin
   CacheNext.CachePrevious:=CachePrevious;
  end else if Cache.Last=self then begin
   Cache.Last:=CachePrevious;
  end;
  Cache:=nil;
  CacheNext:=nil;
  CachePrevious:=nil;
 end;
 inherited Destroy;
end;

procedure TBESENContext.Reset;
begin
 CodeContext:=nil;
 ThisBinding.ValueType:=bvtUNDEFINED;
 while assigned(VariableEnvironment) and assigned(VariableEnvironment.EnvironmentRecord) do begin
  if VariableEnvironment.EnvironmentRecord is TBESENDeclarativeEnvironmentRecord then begin
   break;
  end;
  VariableEnvironment:=VariableEnvironment.Outer;
 end;
 if not (assigned(VariableEnvironment) and assigned(VariableEnvironment.EnvironmentRecord) and (VariableEnvironment.EnvironmentRecord is TBESENDeclarativeEnvironmentRecord)) then begin
  VariableEnvironment:=nil;
 end;
 LexicalEnvironment:=nil;
end;

function TBESENContext.CreateArgumentsObject(const Body:TBESENASTNodeFunctionBody;const FunctionObject:TObject;Arguments:PPBESENValues;CountArguments:integer;const Env:TBESENEnvironmentRecord;const IsStrict:longbool):TBESENObjectFunctionArguments;
var Len,Index,NamesCount:integer;
    Map:TBESENObject;
    Val:TBESENValue;
    Name:TBESENString;
    MappedNames:TBESENStringTree;
    MappedNamesCount:integer;
    StringTreeData:TBESENStringTreeData;
 function MakeArgGetter(const Name:TBESENString;const Env:TBESENEnvironmentRecord):TBESENObjectArgGetterFunction;
 begin
  result:=TBESENObjectArgGetterFunction.Create(Instance,TBESEN(Instance).ObjectFunctionPrototype,true);
  TBESEN(Instance).GarbageCollector.Add(result);
  result.Env:=Env;
  result.ArgName:=Name;
 end;
 function MakeArgSetter(const Name:TBESENString;const Env:TBESENEnvironmentRecord):TBESENObjectArgSetterFunction;
 begin
  result:=TBESENObjectArgSetterFunction.Create(Instance,TBESEN(Instance).ObjectFunctionPrototype,true);
  TBESEN(Instance).GarbageCollector.Add(result);
  result.Env:=Env;
  result.ArgName:=Name;
 end;
begin
 Len:=CountArguments;
 NamesCount:=length(TBESENObjectDeclaredFunction(FunctionObject).Parameters);
 result:=TBESENObjectFunctionArguments.Create(Instance,TBESEN(Instance).ObjectPrototype,false);
 result.GarbageCollectorLock;
 try
  result.IsStrict:=IsStrict;
  result.OverwriteData('length',BESENNumberValue(Len),[bopaWRITABLE,bopaCONFIGURABLE],false);
  Map:=TBESENObject.Create(Instance,TBESEN(Instance).ObjectPrototype);
  MappedNames:=TBESENStringTree.Create;
  MappedNamesCount:=0;
  try
   for Index:=Len-1 downto 0 do begin
    BESENCopyValue(Val,Arguments^[Index]^);
    result.DefineOwnProperty(BESENArrayIndexToStr(Index),BESENDataPropertyDescriptor(Val,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),false);
    if (not IsStrict) and (Index<NamesCount) then begin
     Name:=TBESENObjectDeclaredFunction(FunctionObject).Parameters[Index];
     if not MappedNames.Find(Name,StringTreeData) then begin
      StringTreeData.i:=Index;
      MappedNames.Add(Name,StringTreeData,true);
      inc(MappedNamesCount);
      Map.DefineOwnProperty(BESENArrayIndexToStr(Index),BESENAccessorPropertyDescriptor(MakeArgGetter(Name,Env),MakeArgSetter(Name,Env),[bopaCONFIGURABLE]),false);
     end;
    end;
   end;
  finally
   MappedNames.Free;
  end;
  if MappedNamesCount>0 then begin
   result.ParameterMap:=Map;
  end;
  if IsStrict then begin
   result.OverwriteAccessor('caller',TBESEN(Instance).ObjectThrowTypeErrorFunction,TBESEN(Instance).ObjectThrowTypeErrorFunction,[],false);
   result.OverwriteAccessor('callee',TBESEN(Instance).ObjectThrowTypeErrorFunction,TBESEN(Instance).ObjectThrowTypeErrorFunction,[],false);
  end else begin
   result.OverwriteData('callee',BESENObjectValueEx(TBESENObjectDeclaredFunction(FunctionObject)),[bopaWRITABLE,bopaCONFIGURABLE],false);
  end;
 finally
  result.GarbageCollectorUnlock;
 end;
end;

procedure TBESENContext.InitializeDeclarationBindingInstantiation(const Body:TBESENASTNodeFunctionBody;const FunctionObject:TObject;const IsEval:boolean;Arguments:PPBESENValues;CountArguments:integer;const Reinitialize:boolean);
 procedure SetParameters(const Body:TBESENASTNodeFunctionBody;const Env:TBESENEnvironmentRecord;const IsStrict:boolean;const ParamCount:integer);
 var i,j:integer;
     ArgName:PBESENString;
     v:PBESENValue;
 begin
  if (Env is TBESENDeclarativeEnvironmentRecord) and TBESENDeclarativeEnvironmentRecord(Env).IndexInitialized then begin
   for i:=0 to ParamCount-1 do begin
    j:=Body.Parameters[i].ParameterIndex;
    if i<CountArguments then begin
     BESENCopyValue(TBESENDeclarativeEnvironmentRecord(Env).HashValues[j]^,Arguments^[i]^);
    end else begin
     TBESENDeclarativeEnvironmentRecord(Env).HashValues[j]^.ValueType:=bvtUNDEFINED;
    end;
   end;
  end else begin
   for i:=0 to ParamCount-1 do begin
    ArgName:=@Body.Parameters[i].Name;
    if i<CountArguments then begin
     v:=Arguments^[i];
    end else begin
     v:=@BESENUndefinedValue;
    end;
    if not Env.HasBindingEx(ArgName^,Descriptor) then begin
     Env.CreateMutableBinding(ArgName^);
    end;
    Env.SetMutableBindingEx(ArgName^,v^,IsStrict,Descriptor,OwnDescriptor,Temp);
   end;
  end;
 end;
 procedure SetFunctions(const Body:TBESENASTNodeFunctionBody;const Env:TBESENEnvironmentRecord;const ConfigurableBindings,IsStrict:boolean);
 var i:integer;
     fn:PBESENString;
     fd:TBESENASTNodeFunctionDeclaration;
     fo:TBESENObjectDeclaredFunction;
     go:TBESENObject;
 begin
  for i:=0 to length(Body.Functions)-1 do begin
   if assigned(Body.Functions[i]) and (Body.Functions[i] is TBESENASTNodeFunctionDeclaration) then begin
    if assigned(TBESENASTNodeFunctionDeclaration(Body.Functions[i]).Container.Literal) and assigned(TBESENASTNodeFunctionDeclaration(Body.Functions[i]).Container.Literal.Name) then begin
     fd:=TBESENASTNodeFunctionDeclaration(Body.Functions[i]);
     fn:=@fd.Container.Literal.Name.Name;
     fo:=TBESEN(Instance).MakeFunction(fd.Container.Literal,fd.Container.Literal.Name.Name,LexicalEnvironment);
     if not Env.HasBindingEx(fn^,Descriptor) then begin
      Env.CreateMutableBinding(fn^,ConfigurableBindings);
     end else if Env=TBESEN(Instance).GlobalLexicalEnvironment.EnvironmentRecord then begin
      // ES5-errata fix
      go:=TBESEN(Instance).ObjectGlobal;
      go.GetProperty(fn^,Descriptor);
      if (boppCONFIGURABLE in Descriptor.Presents) and (bopaCONFIGURABLE in Descriptor.Attributes) then begin
       if ConfigurableBindings then begin
        go.DefineOwnPropertyEx(fn^,BESENDataPropertyDescriptor(BESENUndefinedValue,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),true,Descriptor);
       end else begin
        go.DefineOwnPropertyEx(fn^,BESENDataPropertyDescriptor(BESENUndefinedValue,[bopaWRITABLE,bopaENUMERABLE]),true,Descriptor);
       end;
      end else if (([boppGETTER,boppSETTER]*Descriptor.Presents)<>[]) or ((([boppWRITABLE,boppENUMERABLE]*Descriptor.Presents)<>[boppWRITABLE,boppENUMERABLE]) or (([bopaWRITABLE,bopaENUMERABLE]*Descriptor.Attributes)<>[bopaWRITABLE,bopaENUMERABLE])) then begin
       BESENThrowTypeErrorDeclarationBindingInstantiationAtFunctionBinding(fn^);
      end;
     end;
     Env.SetMutableBinding(fn^,BESENObjectValue(fo),IsStrict);
    end;
   end;
  end;
 end;
 procedure SetArguments(const Body:TBESENASTNodeFunctionBody;const Env:TBESENEnvironmentRecord;const IsStrict:boolean);
 var ArgsObj:TBESENObjectFunctionArguments;
 begin
  ArgsObj:=CreateArgumentsObject(Body,TBESENObjectDeclaredFunction(FunctionObject),Arguments,CountArguments,Env,IsStrict);
  if IsStrict then begin
   Env.CreateImmutableBinding('arguments');
   Env.InitializeImmutableBinding('arguments',BESENObjectValue(ArgsObj));
  end else begin
   Env.CreateMutableBinding('arguments');
   Env.SetMutableBindingEx('arguments',BESENObjectValue(ArgsObj),false,Descriptor,OwnDescriptor,Temp);
  end;
 end;
 procedure SetVariables(const Body:TBESENASTNodeFunctionBody;const Env:TBESENEnvironmentRecord;const ConfigurableBindings,IsStrict:boolean);
 var i:integer;
     d:TBESENASTNodeIdentifier;
     dn:PBESENString;
 begin
  for i:=0 to length(Body.Variables)-1 do begin
   d:=Body.Variables[i];
   if assigned(d) and (length(d.Name)>0) then begin
    dn:=@d.Name;
    if not Env.HasBindingEx(dn^,Descriptor) then begin
     Env.CreateMutableBinding(dn^,ConfigurableBindings);
     Env.SetMutableBindingEx(dn^,BESENUndefinedValue,IsStrict,Descriptor,OwnDescriptor,Temp);
    end;
   end;
  end;
 end;
var IsFunction:boolean;
    ParamCount:integer;
    Env:TBESENEnvironmentRecord;
begin
 Env:=VariableEnvironment.EnvironmentRecord;
 IsFunction:=assigned(TBESENObjectDeclaredFunction(FunctionObject)) and Body.IsFunction;
 if IsFunction then begin
  ParamCount:=length(Body.Parameters);
  if ParamCount>0 then begin
   SetParameters(Body,Env,Body.IsStrict,ParamCount);
  end;
 end;
 if length(Body.Functions)>0 then begin
  SetFunctions(Body,Env,IsEval,Body.IsStrict);
 end;
 if not Reinitialize then begin
  if IsFunction and not (Body.DisableArgumentsObject or Env.HasBinding('arguments')) then begin
   SetArguments(Body,Env,Body.IsStrict);
  end;
  SetVariables(Body,Env,IsEval,Body.IsStrict);
 end;
 if Env.RecordType=BESENEnvironmentRecordTypeDeclarative then begin
  TBESENDeclarativeEnvironmentRecord(Env).Touched:=false;
 end;
end;

procedure TBESENContext.Mark;
var i:integer;
begin
 if assigned(LexicalEnvironment) then begin
  TBESEN(Instance).GarbageCollector.GrayIt(LexicalEnvironment);
 end;
 if assigned(VariableEnvironment) then begin
  TBESEN(Instance).GarbageCollector.GrayIt(VariableEnvironment);
 end;                        
 TBESEN(Instance).GarbageCollector.GrayValue(ThisBinding);
 if assigned(CodeContext) then begin
  if assigned(TBESENCodeContext(CodeContext).Code) then begin
   TBESENCodeContext(CodeContext).Code.Mark;
  end;
  for i:=0 to length(TBESENCodeContext(CodeContext).RegisterValues)-1 do begin
   TBESEN(Instance).GarbageCollector.GrayValue(TBESENCodeContext(CodeContext).RegisterValues[i]);
  end;
 end;
end;

constructor TBESENContextCache.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 First:=nil;
 Last:=nil;
 Count:=0;
end;

destructor TBESENContextCache.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TBESENContextCache.Clear;
var CurrentContext:TBESENContext;
begin
 while true do begin
  CurrentContext:=Pop;
  if assigned(CurrentContext) then begin
   BESENFreeAndNil(CurrentContext);
  end else begin
   break;
  end;
 end;
end;

procedure TBESENContextCache.Cleanup;
var CurrentContext:TBESENContext;
begin
 while Count>TBESEN(Instance).MaxCountOfFreeContexts do begin
  CurrentContext:=Pop;
  if assigned(CurrentContext) then begin
   BESENFreeAndNil(CurrentContext);
  end else begin
   break;
  end;
 end;
end;

procedure TBESENContextCache.Push(Context:TBESENContext);
begin
 Context.Cache:=self;
 if assigned(Last) then begin
  Last.CacheNext:=Context;
  Context.CachePrevious:=Last;
  Context.CacheNext:=nil;
  Last:=Context;
 end else begin
  First:=Context;
  Last:=Context;
  Context.CachePrevious:=nil;
  Context.CacheNext:=nil;
 end;
 inc(Count);
end;

function TBESENContextCache.Pop:TBESENContext;
begin
 if assigned(Last) then begin
  result:=Last;
  if assigned(result.CachePrevious) then begin
   result.CachePrevious.CacheNext:=result.CacheNext;
  end else if First=result then begin
   First:=result.CacheNext;
  end;
  if assigned(result.CacheNext) then begin
   result.CacheNext.CachePrevious:=result.CachePrevious;
  end else if Last=result then begin
   Last:=result.CachePrevious;
  end;
  result.Cache:=nil;
  result.CachePrevious:=nil;
  result.CacheNext:=nil;
  dec(Count);
 end else begin
  result:=nil;
  Count:=0;
 end;
end;

procedure TBESENContextCache.Mark;
var CurrentContext:TBESENContext;
begin
 CurrentContext:=First;
 while assigned(CurrentContext) do begin
  CurrentContext.Mark;
  CurrentContext:=CurrentContext.CacheNext;
 end;
end;

end.
