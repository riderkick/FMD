program BESENShell;
{$i BESEN.inc}
{$ifdef win32}
 {$apptype console}
{$endif}

uses
{$ifdef fpc}
{$ifdef win32}
  Windows,
{$endif}
{$else}
{$ifdef win32}
//  FastMM4,
  Windows,
{$endif}
{$endif}
  SysUtils,
  Classes,
  BESEN in 'BESEN.pas',
  BESENCodeSnapshot in 'BESENCodeSnapshot.pas',
  BESENVersionConstants in 'BESENVersionConstants.pas',
  BESENConstants in 'BESENConstants.pas',
  BESENValueContainer in 'BESENValueContainer.pas',
  BESENUnicodeTables in 'BESENUnicodeTables.pas',
  BESENStringUtils in 'BESENStringUtils.pas',
  BESENStringTree in 'BESENStringTree.pas',
  BESENStringList in 'BESENStringList.pas',
  BESENSelfBalancedTree in 'BESENSelfBalancedTree.pas',
  BESENScope in 'BESENScope.pas',
  BESENRegExpCache in 'BESENRegExpCache.pas',
  BESENRandomGenerator in 'BESENRandomGenerator.pas',
  BESENPointerSelfBalancedTree in 'BESENPointerSelfBalancedTree.pas',
  BESENPointerList in 'BESENPointerList.pas',
  BESENParser in 'BESENParser.pas',
  BESENOpcodes in 'BESENOpcodes.pas',
  BESENObjectThrowTypeErrorFunction in 'BESENObjectThrowTypeErrorFunction.pas',
  BESENObjectStringPrototype in 'BESENObjectStringPrototype.pas',
  BESENObjectStringConstructor in 'BESENObjectStringConstructor.pas',
  BESENObjectString in 'BESENObjectString.pas',
  BESENObjectRegExpPrototype in 'BESENObjectRegExpPrototype.pas',
  BESENObjectRegExpConstructor in 'BESENObjectRegExpConstructor.pas',
  BESENObjectRegExp in 'BESENObjectRegExp.pas',
  BESENObjectPrototype in 'BESENObjectPrototype.pas',
  BESENObjectPropertyDescriptor in 'BESENObjectPropertyDescriptor.pas',
  BESENObjectNumberPrototype in 'BESENObjectNumberPrototype.pas',
  BESENObjectNumberConstructor in 'BESENObjectNumberConstructor.pas',
  BESENObjectNumber in 'BESENObjectNumber.pas',
  BESENObjectNativeFunction in 'BESENObjectNativeFunction.pas',
  BESENObjectMath in 'BESENObjectMath.pas',
  BESENObjectJSON in 'BESENObjectJSON.pas',
  BESENObjectGlobal in 'BESENObjectGlobal.pas',
  BESENObjectFunctionPrototype in 'BESENObjectFunctionPrototype.pas',
  BESENObjectFunctionConstructor in 'BESENObjectFunctionConstructor.pas',
  BESENObjectFunctionArguments in 'BESENObjectFunctionArguments.pas',
  BESENObjectFunction in 'BESENObjectFunction.pas',
  BESENObjectErrorPrototype in 'BESENObjectErrorPrototype.pas',
  BESENObjectErrorConstructor in 'BESENObjectErrorConstructor.pas',
  BESENObjectError in 'BESENObjectError.pas',
  BESENObjectEnvironmentRecord in 'BESENObjectEnvironmentRecord.pas',
  BESENObjectDeclaredFunction in 'BESENObjectDeclaredFunction.pas',
  BESENObjectDatePrototype in 'BESENObjectDatePrototype.pas',
  BESENObjectDateConstructor in 'BESENObjectDateConstructor.pas',
  BESENObjectDate in 'BESENObjectDate.pas',
  BESENObjectConstructor in 'BESENObjectConstructor.pas',
  BESENObjectBooleanPrototype in 'BESENObjectBooleanPrototype.pas',
  BESENObjectBooleanConstructor in 'BESENObjectBooleanConstructor.pas',
  BESENObjectBoolean in 'BESENObjectBoolean.pas',
  BESENObjectBindingFunction in 'BESENObjectBindingFunction.pas',
  BESENObjectArrayPrototype in 'BESENObjectArrayPrototype.pas',
  BESENObjectArrayConstructor in 'BESENObjectArrayConstructor.pas',
  BESENObjectArray in 'BESENObjectArray.pas',
  BESENObjectArgSetterFunction in 'BESENObjectArgSetterFunction.pas',
  BESENObjectArgGetterFunction in 'BESENObjectArgGetterFunction.pas',
  BESENObject in 'BESENObject.pas',
  BESENNumberUtils in 'BESENNumberUtils.pas',
  BESENNativeObject in 'BESENNativeObject.pas',
  BESENNativeCodeMemoryManager in 'BESENNativeCodeMemoryManager.pas',
  BESENLocale in 'BESENLocale.pas',
  BESENLexicalEnvironment in 'BESENLexicalEnvironment.pas',
  BESENLexer in 'BESENLexer.pas',
  BESENKeyIDManager in 'BESENKeyIDManager.pas',
  BESENIntegerList in 'BESENIntegerList.pas',
  BESENInt64SelfBalancedTree in 'BESENInt64SelfBalancedTree.pas',
  BESENHashUtils in 'BESENHashUtils.pas',
  BESENHashMap in 'BESENHashMap.pas',
  BESENGlobals in 'BESENGlobals.pas',
  BESENGarbageCollector in 'BESENGarbageCollector.pas',
  BESENEvalCacheItem in 'BESENEvalCacheItem.pas',
  BESENEvalCache in 'BESENEvalCache.pas',
  BESENErrors in 'BESENErrors.pas',
  BESENEnvironmentRecord in 'BESENEnvironmentRecord.pas',
  BESENDoubleList in 'BESENDoubleList.pas',
  BESENDecompiler in 'BESENDecompiler.pas',
  BESENDeclarativeEnvironmentRecord in 'BESENDeclarativeEnvironmentRecord.pas',
  BESENDateUtils in 'BESENDateUtils.pas',
  BESENCompiler in 'BESENCompiler.pas',
  BESENCollectorObject in 'BESENCollectorObject.pas',
  BESENCollector in 'BESENCollector.pas',
  BESENCharset in 'BESENCharset.pas',
  BESENBaseObject in 'BESENBaseObject.pas',
  BESENArrayUtils in 'BESENArrayUtils.pas',
  BESENTypes in 'BESENTypes.pas',
  BESENUtils in 'BESENUtils.pas',
  BESENValue in 'BESENValue.pas',
  BESENRegExp in 'BESENRegExp.pas',
  BESENCode in 'BESENCode.pas',
  BESENASTNodes in 'BESENASTNodes.pas',
  BESENCodeContext in 'BESENCodeContext.pas',
  BESENCodeGeneratorContext in 'BESENCodeGeneratorContext.pas',
  BESENContext in 'BESENContext.pas',
  BESENObjectConsole in 'BESENObjectConsole.pas';

type TShellFunctions=class
      procedure RegExpDebugOutputHook(const Instance:TBESEN;const Data:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENUTF8STRING{$endif};NewLine:TBESENBOOLEAN);
      procedure NativeSetRegExpDebugMode(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeGetRegExpDebugMode(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeSetRegExpTimeOutSteps(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeGetRegExpTimeOutSteps(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativePrint(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativePrintLn(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeReadLn(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeTrace(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeAlert(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativePrompt(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeExit(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeReadFile(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeWriteFile(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeReadDirectory(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeLoad(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeGC(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
      procedure NativeVersion(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
     end;

     TFileObject=class(TBESENNativeObject)
      private
       fFileName:TBESENString;
       fFileStream:TFileStream;
      protected
       procedure ConstructObject(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer); override;
       procedure InitializeObject; override;
       procedure FinalizeObject; override;
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); override;
       destructor Destroy; override;
      published
       procedure close(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure read(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure write(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure seek(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure position(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure eof(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure flush(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       property fileName:TBESENString read fFileName write fFileName;
     end;

var Instance:TBESEN;
    ShellFunctions:TShellFunctions;
    DoExit:boolean;

constructor TFileObject.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 fFileStream:=nil;
end;

destructor TFileObject.Destroy;
begin
 BesenFreeAndNil(fFileStream);
 inherited Destroy;
end;

procedure TFileObject.ConstructObject(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer);
var s:string;
    Mode:longword;
begin
 inherited ConstructObject(ThisArgument,Arguments,CountArguments);
 if CountArguments=0 then begin
  raise EBESENError.Create('FileError','Too few arguments');
 end else begin
  fFileName:=TBESEN(Instance).ToStr(Arguments^[0]^);
  if FileExists(fFileName) then begin
   Mode:=fmOpenReadWrite or fmShareExclusive;
  end else begin
   Mode:=fmCreate or fmShareDenyRead;
  end;
  if CountArguments>1 then begin
   s:=TBESEN(Instance).ToStr(Arguments^[1]^);
   if s='c' then begin
    Mode:=fmCreate or fmShareDenyRead;
   end else if s='r' then begin
    Mode:=fmOpenRead or fmShareDenyWrite;
   end else if s='rw' then begin
    Mode:=fmOpenReadWrite or fmShareExclusive;
   end else if s='w' then begin
    if FileExists(fFileName) then begin
     Mode:=fmOpenWrite or fmShareDenyRead;
    end else begin
     Mode:=fmCreate or fmShareDenyRead;
    end;
   end;
  end;
{$ifdef BESENSingleStringType}
  fFileStream:=TFileStream.Create(fFileName,Mode);
{$else}
  fFileStream:=TFileStream.Create(String(BESENEncodeString(BESENUTF16ToUTF8(fFileName),UTF_8,BESENLocaleCharset)),Mode);
{$endif}
 end;
 s:='';
end;

procedure TFileObject.InitializeObject;
begin
 inherited InitializeObject;
 fFileName:='';
 fFileStream:=nil;
end;

procedure TFileObject.FinalizeObject;
begin
 BesenFreeAndNil(fFileStream);
 inherited FinalizeObject;
end;

procedure TFileObject.close(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue.ValueType:=bvtUNDEFINED;
 BesenFreeAndNil(fFileStream);
end;

procedure TFileObject.read(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var l,ms:int64;
    s:string;
begin
 s:='';
 ResultValue.ValueType:=bvtUNDEFINED;
 if assigned(fFileStream) then begin
  ms:=fFileStream.Size-fFileStream.Position;
  if CountArguments=0 then begin
   l:=ms;
  end else begin
   if Arguments^[0]^.ValueType=bvtUNDEFINED then begin
    l:=ms;
   end else begin
    l:=TBESEN(Instance).ToInt(Arguments^[0]^);
    if l>ms then begin
     l:=ms;
    end;
   end;
  end;
  SetLength(s,l);
  if l>0 then begin
   fFileStream.Read(s[1],l);
  end;
  ResultValue.ValueType:=bvtSTRING;
  ResultValue.Str:=s;
  SetLength(s,0);
 end;
end;

procedure TFileObject.write(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var l:int64;
    s:string;
begin
 s:='';
 ResultValue.ValueType:=bvtUNDEFINED;
 if assigned(fFileStream) then begin
  if CountArguments>0 then begin
   ResultValue.ValueType:=bvtNUMBER;
   ResultValue.Num:=0;
   s:=TBESEN(Instance).ToStr(Arguments^[0]^);
   l:=length(s);
   if l>0 then begin
    ResultValue.Num:=fFileStream.Write(s[1],l);
   end;
   SetLength(s,0);
  end;
 end;
end;

procedure TFileObject.seek(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var l,ms:int64;
begin
 ResultValue.ValueType:=bvtUNDEFINED;
 if assigned(fFileStream) then begin
  ms:=fFileStream.Size;
  if CountArguments=0 then begin
   l:=fFileStream.Position;
  end else begin
   if Arguments^[0]^.ValueType=bvtUNDEFINED then begin
    l:=fFileStream.Position;
   end else begin
    l:=TBESEN(Instance).ToInt(Arguments^[0]^);
    if l>ms then begin
     l:=ms;
    end;
   end;
  end;
  ResultValue.ValueType:=bvtNUMBER;
  ResultValue.Num:=fFileStream.Seek(l,soFromBeginning);
 end;
end;

procedure TFileObject.position(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue.ValueType:=bvtUNDEFINED;
 if assigned(fFileStream) then begin
  ResultValue.ValueType:=bvtNUMBER;
  ResultValue.Num:=fFileStream.Position;
 end;
end;

procedure TFileObject.eof(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue.ValueType:=bvtUNDEFINED;
 if assigned(fFileStream) then begin
  ResultValue.ValueType:=bvtBOOLEAN;
  ResultValue.Bool:=fFileStream.Position<fFileStream.Size;
 end;
end;

procedure TFileObject.flush(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue.ValueType:=bvtUNDEFINED;
 if assigned(fFileStream) then begin
{$ifdef win32}
  Windows.FlushFileBuffers(fFileStream.Handle);
{$endif}
 end;
end;

procedure TShellFunctions.RegExpDebugOutputHook(const Instance:TBESEN;const Data:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENUTF8STRING{$endif};NewLine:TBESENBOOLEAN);
begin
 if NewLine then begin
  writeln(Data);
 end else begin
  write(Data);
 end;
end;

procedure TShellFunctions.NativeSetRegExpDebugMode(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue.ValueType:=bvtUNDEFINED;
 if CountArguments>0 then begin
  Instance.RegExpDebug:=Instance.ToUINT32(Arguments^[0]^);
 end;
end;

procedure TShellFunctions.NativeGetRegExpDebugMode(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue.ValueType:=bvtNUMBER;
 ResultValue.Num:=Instance.RegExpDebug;
end;

procedure TShellFunctions.NativeSetRegExpTimeOutSteps(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue.ValueType:=bvtUNDEFINED;
 if CountArguments>0 then begin
  Instance.RegExpTimeOutSteps:=Instance.ToINT(Arguments^[0]^);
 end;
end;

procedure TShellFunctions.NativeGetRegExpTimeOutSteps(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue.ValueType:=bvtNUMBER;
 ResultValue.Num:=Instance.RegExpTimeOutSteps;
end;

procedure TShellFunctions.NativePrint(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var i:integer;
    v:PBESENValue;
    fOutput:widestring;
 procedure writeit(s:widestring);
 begin
  fOutput:=fOutput+s;
 end;
begin
 fOutput:='';
 ResultValue.ValueType:=bvtUNDEFINED;
 for i:=0 to CountArguments-1 do begin
  v:=Arguments^[i];
  case v^.ValueType of
   bvtUNDEFINED:begin
    writeit('undefined');
   end;
   bvtNULL:begin
    writeit('null');
   end;
   bvtBOOLEAN:begin
    if v^.Bool then begin
     writeit('true');
    end else begin
     writeit('false');
    end;
   end;
   bvtNUMBER:begin
    writeit(BESENFloatToStr(v^.Num));
   end;
   bvtSTRING:begin
    writeit(v^.Str);
   end;
   bvtOBJECT:begin
    writeit(TBESEN(Instance).ToStr(v^));
   end;
   bvtREFERENCE:begin
    writeit('reference');
   end;
  end;
 end;
{$ifdef Delphi2009AndUp}
 writeln(fOutput);
{$else}
 writeln(BESENEncodeString(BESENUTF16ToUTF8(fOutput),UTF_8,BESENLocaleCharset));
{$endif}
end;

procedure TShellFunctions.NativePrintLn(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 NativePrint(ThisArgument,Arguments,CountArguments,ResultValue);
 writeln;
end;

procedure TShellFunctions.NativeReadLn(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var s:string;
begin
 readln(s);
{$ifdef Delphi2009AndUp}
 ResultValue:=BESENStringValue(s);
{$else}
 ResultValue:=BESENStringLocaleCharsetValue(s);
{$endif}
end;

procedure TShellFunctions.NativeTrace(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 NativePrintLn(ThisArgument,Arguments,CountArguments,ResultValue);
end;

procedure TShellFunctions.NativeAlert(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var i:integer;
    v:PBESENValue;
    ss:widestring;
 procedure writeit(s:widestring);
 begin
  ss:=ss+s;
 end;
begin
 ss:='';
 ResultValue.ValueType:=bvtUNDEFINED;
 for i:=0 to CountArguments-1 do begin
  v:=Arguments^[i];
  case v^.ValueType of
   bvtUNDEFINED:begin
    writeit('undefined');
   end;
   bvtNULL:begin
    writeit('null');
   end;
   bvtBOOLEAN:begin
    if v^.Bool then begin
     writeit('true');
    end else begin
     writeit('false');
    end;
   end;
   bvtNUMBER:begin
    writeit(BESENFloatToStr(v^.Num));
   end;
   bvtSTRING:begin
    writeit(v^.Str);
   end;
   bvtOBJECT:begin
    writeit(TBESEN(Instance).ToStr(v^));
   end;
   bvtREFERENCE:begin
    writeit('reference');
   end;
  end;
 end;
{$ifdef Delphi2009AndUp}
 writeln(ss);
{$else}
 writeln(BESENEncodeString(BESENUTF16ToUTF8(ss),UTF_8,BESENLocaleCharset));
{$endif}
end;

procedure TShellFunctions.NativePrompt(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ss:{$ifdef BESENSingleStringType}TBESENString{$else}ansistring{$endif};
    s:string;
begin
 ss:='';
 try
  if CountArguments>0 then begin
{$ifdef BESENSingleStringType}
   ss:=TBESEN(Instance).ToStr(Arguments^[0]^);
{$else}
   ss:=BESENEncodeString(BESENUTF16ToUTF8(TBESEN(Instance).ToStr(Arguments^[0]^)),UTF_8,BESENLocaleCharset);
{$endif}
  end else begin
   ss:='';
  end;
(* if CountArguments>1 then begin
{$ifdef BESENSingleStringType}
    s:=TBESEN(Instance).ToStr(Arguments^[1]^);
{$else}
    s:=BESENEncodeString(BESENUTF16ToUTF8(TBESEN(Instance).ToStr(Arguments^[1]^)),UTF_8,BESENLocaleCharset);
{$endif}
  end else begin
   s:='';
  end;*)
  write(ss);
  readln(s);
{$ifdef Delphi2009AndUp}
  ResultValue:=BESENStringValue(s);
{$else}
  ResultValue:=BESENStringLocaleCharsetValue(s);
{$endif}
 finally
 end;
end;

procedure TShellFunctions.NativeExit(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 DoExit:=true;
end;

procedure TShellFunctions.NativeReadFile(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var FileName,Content:{$ifdef BESENSingleStringType}TBESENString{$else}ansistring{$endif};
    fm:byte;
    f:file;
begin
 ResultValue.ValueType:=bvtUNDEFINED;
 try
  if CountArguments>0 then begin
{$ifdef BESENSingleStringType}
   FileName:=TBESEN(Instance).ToStr(Arguments^[0]^));
{$else}
   FileName:=BESENEncodeString(BESENUTF16ToUTF8(TBESEN(Instance).ToStr(Arguments^[0]^)),UTF_8,BESENLocaleCharset);
{$endif}
   Content:='';
   fm:=filemode;
   filemode:=0;
   assignfile(f,String(FileName));
   {$i-}reset(f,1);{$i+};
   if ioresult=0 then begin
    SetLength(Content,filesize(f));
    if length(Content)>0 then begin
     {$i-}blockread(f,Content[1],length(Content));{$i+}
     if ioresult<>0 then begin
      {$i-}closefile(f);{$i+}
      filemode:=fm;
      raise EBESENError.Create('FileError','Couldn''t read file "'+String(FileName)+'"');
      exit;
     end;
    end;
    ResultValue.ValueType:=bvtSTRING;
    ResultValue.Str:={$ifndef BESENSingleStringType}BESENUTF8ToUTF16(BESENConvertToUTF8({$endif}Content{$ifndef BESENSingleStringType})){$endif};
    {$i-}closefile(f);{$i+}
    if ioresult=0 then begin
    end;
    filemode:=fm;
   end else begin
    {$i-}closefile(f);{$i+}
    if ioresult=0 then begin
    end;
    filemode:=fm;
    raise EBESENError.Create('FileError','Couldn''t read file "'+String(FileName)+'"');
   end;
  end else begin
   raise EBESENError.Create('FileError','Too few arguments');
  end;
 finally
 end;
end;

procedure TShellFunctions.NativeWriteFile(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var FileName,Content:{$ifdef BESENSingleStringType}TBESENString{$else}ansistring{$endif};
    fm:byte;
    f:file;
begin
 ResultValue.ValueType:=bvtUNDEFINED;
 try
  if CountArguments>1 then begin
{$ifdef BESENSingleStringType}
   FileName:=TBESEN(Instance).ToStr(Arguments^[0]^));
   Content:=TBESEN(Instance).ToStr(Arguments^[1]^);
{$else}
   FileName:=BESENEncodeString(BESENUTF16ToUTF8(TBESEN(Instance).ToStr(Arguments^[0]^)),UTF_8,BESENLocaleCharset);
   Content:=#$ef#$bb#$bf+BESENUTF16ToUTF8(TBESEN(Instance).ToStr(Arguments^[1]^));
{$endif}
   fm:=filemode;
   filemode:=2;
   assignfile(f,String(FileName));
   {$i-}rewrite(f,1);{$i+};
   if ioresult=0 then begin
    if length(Content)>0 then begin
     {$i-}blockwrite(f,Content[1],length(Content));{$i+}
     if ioresult<>0 then begin
      {$i-}closefile(f);{$i+}
      filemode:=fm;
      raise EBESENError.Create('FileError','Couldn''t write file "'+String(FileName)+'"');
      exit;
     end;
    end;
    {$i-}closefile(f);{$i+}
    if ioresult=0 then begin
    end;
    filemode:=fm;
   end else begin
    {$i-}closefile(f);{$i+}
    if ioresult=0 then begin
    end;
    filemode:=fm;
    raise EBESENError.Create('FileError','Couldn''t write file "'+String(FileName)+'"');
   end;
  end else begin
   raise EBESENError.Create('FileError','Too few arguments');
  end;
 finally
 end;
end;

procedure TShellFunctions.NativeReadDirectory(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
const BoolStr:array[boolean] of {$ifdef BESENSingleStringType}TBESENString{$else}ansistring{$endif}=('false','true');
var FileName,Content:{$ifdef BESENSingleStringType}TBESENString{$else}ansistring{$endif};
    SearchRec:TSearchRec;
    Count:integer;
begin
 ResultValue.ValueType:=bvtUNDEFINED;
 try
  if CountArguments>0 then begin
   Content:='[';
{$ifdef BESENSingleStringType}
   FileName:=TBESEN(Instance).ToStr(Arguments^[0]^));
{$else}
   FileName:=BESENEncodeString(BESENUTF16ToUTF8(TBESEN(Instance).ToStr(Arguments^[0]^)),UTF_8,BESENLocaleCharset);
{$endif}
   if FindFirst(String(FileName),faAnyFile or faDirectory,SearchRec)=0 then begin
    Count:=0;
    repeat
     if Count>0 then begin
      Content:=Content+',';
     end;
     Content:=Content+'{';
     Content:=Content+'"name":'+{$ifdef BESENSingleStringType}TBESENString{$else}ansistring{$endif}(BESENJSONStringQuote(BESENUTF8ToUTF16(BESENEncodeString(AnsiString(SearchRec.Name),BESENLocaleCharset,UTF_8))))+',';
     Content:=Content+'"size":'+{$ifdef BESENSingleStringType}TBESENString{$else}ansistring{$endif}(IntToStr(SearchRec.Size))+',';
     Content:=Content+'"time":'+{$ifdef BESENSingleStringType}TBESENString{$else}ansistring{$endif}(BESENFloatToStr(BESENDateTimeToBESENDate(FileDateToDateTime(SearchRec.Time))))+',';
     Content:=Content+'"flags":{';
     Content:=Content+'"hidden":'+BoolStr[(SearchRec.Attr and faHidden)<>0]+',';
     Content:=Content+'"systemFile":'+BoolStr[(SearchRec.Attr and faSysFile)<>0]+',';
     Content:=Content+'"volumeID":'+BoolStr[(SearchRec.Attr and faVolumeID)<>0]+',';
     Content:=Content+'"directory":'+BoolStr[(SearchRec.Attr and faDirectory)<>0]+',';
     Content:=Content+'"archive":'+BoolStr[(SearchRec.Attr and faArchive)<>0]+'';
     Content:=Content+'}}';
     inc(Count);
    until FindNext(SearchRec)<>0;
    FindClose(SearchRec);
   end;
   Content:=Content+']';
   ResultValue:=TBESEN(Instance).JSONEval(Content);
  end else begin
   raise EBESENError.Create('DirectoryError','Too few arguments');
  end;
 finally
 end;
end;

procedure TShellFunctions.NativeLoad(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var FileName,Content:{$ifdef BESENSingleStringType}TBESENString{$else}ansistring{$endif};
    fm:byte;
    f:file;
begin
 ResultValue.ValueType:=bvtUNDEFINED;
 try
  if CountArguments>0 then begin
{$ifdef BESENSingleStringType}
   FileName:=TBESEN(Instance).ToStr(Arguments^[0]^));
{$else}
   FileName:=BESENEncodeString(BESENUTF16ToUTF8(TBESEN(Instance).ToStr(Arguments^[0]^)),UTF_8,BESENLocaleCharset);
{$endif}
   Content:='';
   fm:=filemode;
   filemode:=0;
   assignfile(f,string(FileName));
   {$i-}reset(f,1);{$i+};
   if ioresult=0 then begin
    SetLength(Content,filesize(f));
    if length(Content)>0 then begin
     {$i-}blockread(f,Content[1],length(Content));{$i+}
     if ioresult<>0 then begin
      {$i-}closefile(f);{$i+}
      filemode:=fm;
      raise EBESENError.Create('FileError','Couldn''t load file "'+String(FileName)+'"');
      exit;
     end;
    end;
    ResultValue:=TBESEN(Instance).Execute({$ifndef BESENSingleStringType}BESENConvertToUTF8({$endif}Content{$ifndef BESENSingleStringType}){$endif});
    {$i-}closefile(f);{$i+}
    if ioresult=0 then begin
    end;
    filemode:=fm;
   end else begin
    {$i-}closefile(f);{$i+}
    if ioresult=0 then begin
    end;
    filemode:=fm;
    raise EBESENError.Create('FileError','Couldn''t load file "'+String(FileName)+'"');
   end;
  end else begin
   raise EBESENError.Create('FileError','Too few arguments');
  end;
 finally
 end;
end;

procedure TShellFunctions.NativeGC(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue.ValueType:=bvtUNDEFINED;
 TBESEN(Instance).GarbageCollector.CollectAll;
end;

procedure TShellFunctions.NativeVersion(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue.ValueType:=bvtSTRING;
 ResultValue.Str:='BESEN Shell v'+BESENVersion+' - Copyright (C) 2010-2015, Benjamin ''BeRo'' Rosseaux';
end;

procedure Print(const v:TBESENValue);
var fOutput:widestring;
 procedure writeit(s:widestring);
 begin
  fOutput:=fOutput+s;
 end;
begin
 fOutput:='';
 case v.ValueType of
  bvtUNDEFINED:begin
   writeit('undefined');
  end;
  bvtNULL:begin
   writeit('null');
  end;
  bvtBOOLEAN:begin
   if v.Bool then begin
    writeit('true');
   end else begin
    writeit('false');
   end;
  end;
  bvtNUMBER:begin
   writeit(BESENFloatToStr(v.Num));
  end;
  bvtSTRING:begin
   writeit(v.Str);
  end;
  bvtOBJECT:begin
{$ifdef BESENSingleStringType}
   writeit(TBESEN(Instance).ToStr(v));
{$else}
   writeit(WideString(BESENEncodeString(BESENUTF16ToUTF8(TBESEN(Instance).ToStr(v)),UTF_8,BESENLocaleCharset)));
{$endif}
  end;
  bvtREFERENCE:begin
   writeit('reference');
  end;
 end;
{$ifdef Delphi2009AndUp}
 writeln(fOutput);
{$else}
 writeln(BESENUTF16ToUTF8(fOutput));
{$endif}
end;

var FileName,s:{$ifdef BESENSingleStringType}TBESENString{$else}ansistring{$endif};
    ss:widestring;
    v:TBESENValue;
    i:integer;
    c:{$ifdef BESENSingleStringType}char{$else}ansichar{$endif};
    Compatibility:longword;
    ObjDocument,ObjNavigator,ObjShell,ObjWindow:TBESENObject;
begin
{$ifdef cpu386}
 Set8087CW($133f);
{$endif}
{$ifdef cpuamd64}
 Set8087CW($133f);
{$endif}
 DoExit:=false;
 FileName:='';
 Compatibility:=COMPAT_BESEN;
 if paramcount>0 then begin
  for i:=1 to paramcount do begin
   s:={$ifndef BESENSingleStringType}AnsiString({$endif}paramstr(i){$ifndef BESENSingleStringType}){$endif};
   if length(s)>0 then begin
    case s[1] of
     '-','+','/':begin
      c:=s[1];
      if c='-' then begin
      end;
      delete(s,1,1);
      if (s='?') or (s='h') or (s='help') then begin
       writeln('BESEN Shell v'+BESENVersion+' - Copyright (C) 2010-2015, Benjamin ''BeRo'' Rosseaux');
       writeln('Usage: ',ExtractFileName(paramstr(0)),' [filename] [options]');
       writeln('Options: +help       = This help text');
       writeln('         +javascript = Enable facile javascript compatibility mode');
       writeln('         +sgmlcom    = Treat ''<!--'' as a ''//'' comment');
       writeln('         +unsafeutf8 = Accept ''valid but insecure'' UTF8');
       DoExit:=true;
      end else if s='javascript' then begin
       Compatibility:=Compatibility or COMPAT_JS;
      end else if s='sgmlcom' then begin
       Compatibility:=Compatibility or COMPAT_SGMLCOM;
      end else if s='unsafeutf8' then begin
       Compatibility:=Compatibility or COMPAT_UTF8_UNSAFE;
      end;
     end;
     else begin
      FileName:=s;
     end;
    end;
   end;
  end;
 end;
 ShellFunctions:=TShellFunctions.Create;
 Instance:=TBESEN.Create(Compatibility);
 TBESEN(Instance).RegExpDebugOutputHook:=ShellFunctions.RegExpDebugOutputHook;
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('setRegExpDebugMode',ShellFunctions.NativeSetRegExpDebugMode,1,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('getRegExpDebugMode',ShellFunctions.NativeGetRegExpDebugMode,0,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('setRegExpTimeOutSteps',ShellFunctions.NativeSetRegExpTimeOutSteps,1,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('getRegExpTimeOutSteps',ShellFunctions.NativeGetRegExpTimeOutSteps,0,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('print',ShellFunctions.NativePrint,1,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('println',ShellFunctions.NativePrintLn,1,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('readln',ShellFunctions.NativeReadLn,1,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('trace',ShellFunctions.NativeTrace,1,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('alert',ShellFunctions.NativeAlert,1,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('prompt',ShellFunctions.NativePrompt,1,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('exit',ShellFunctions.NativeExit,1,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('readFile',ShellFunctions.NativeReadFile,1,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('writeFile',ShellFunctions.NativeWriteFile,2,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('readDirectory',ShellFunctions.NativeReadDirectory,1,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('load',ShellFunctions.NativeLoad,1,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('gc',ShellFunctions.NativeGC,0,[]);
 TBESEN(Instance).ObjectGlobal.RegisterNativeFunction('version',ShellFunctions.NativeVersion,0,[]);
 TBESEN(Instance).RegisterNativeObject('File',TFileObject);

 ObjDocument:=TBESENObject.Create(Instance,TBESEN(Instance).ObjectPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(ObjDocument);
 TBESEN(Instance).ObjectGlobal.OverwriteData('document',BESENObjectValue(ObjDocument),[bopaWRITABLE,bopaCONFIGURABLE]);
 ObjDocument.RegisterNativeFunction('write',ShellFunctions.NativePrint,1,[]);
 ObjDocument.RegisterNativeFunction('writeln',ShellFunctions.NativePrintLn,1,[]);

 ObjNavigator:=TBESENObject.Create(Instance,TBESEN(Instance).ObjectPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(ObjNavigator);
 TBESEN(Instance).ObjectGlobal.OverwriteData('navigator',BESENObjectValue(ObjNavigator),[bopaWRITABLE,bopaCONFIGURABLE]);
 ObjNavigator.OverwriteData('userAgent',BESENStringValue('BESEN Shell v'+BESENVersion+' - Copyright (C) 2010, Benjamin ''BeRo'' Rosseaux'),[bopaWRITABLE,bopaCONFIGURABLE]);

 ObjShell:=TBESENObject.Create(Instance,TBESEN(Instance).ObjectPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(ObjShell);
 TBESEN(Instance).ObjectGlobal.OverwriteData('Shell',BESENObjectValue(ObjShell),[bopaWRITABLE,bopaCONFIGURABLE]);
 ObjShell.RegisterNativeFunction('setRegExpDebugMode',ShellFunctions.NativeSetRegExpDebugMode,1,[]);
 ObjShell.RegisterNativeFunction('getRegExpDebugMode',ShellFunctions.NativeGetRegExpDebugMode,0,[]);
 ObjShell.RegisterNativeFunction('setRegExpTimeOutSteps',ShellFunctions.NativeSetRegExpTimeOutSteps,1,[]);
 ObjShell.RegisterNativeFunction('getRegExpTimeOutSteps',ShellFunctions.NativeGetRegExpTimeOutSteps,0,[]);
 ObjShell.RegisterNativeFunction('print',ShellFunctions.NativePrint,1,[]);
 ObjShell.RegisterNativeFunction('println',ShellFunctions.NativePrintLn,1,[]);
 ObjShell.RegisterNativeFunction('readln',ShellFunctions.NativeReadLn,1,[]);
 ObjShell.RegisterNativeFunction('trace',ShellFunctions.NativeTrace,1,[]);
 ObjShell.RegisterNativeFunction('alert',ShellFunctions.NativeAlert,1,[]);
 ObjShell.RegisterNativeFunction('prompt',ShellFunctions.NativePrompt,1,[]);
 ObjShell.RegisterNativeFunction('exit',ShellFunctions.NativeExit,1,[]);
 ObjShell.RegisterNativeFunction('readFile',ShellFunctions.NativeReadFile,1,[]);
 ObjShell.RegisterNativeFunction('writeFile',ShellFunctions.NativeWriteFile,2,[]);
 ObjShell.RegisterNativeFunction('readDirectory',ShellFunctions.NativeReadDirectory,1,[]);
 ObjShell.RegisterNativeFunction('load',ShellFunctions.NativeLoad,1,[]);
 ObjShell.RegisterNativeFunction('gc',ShellFunctions.NativeGC,0,[]);
 ObjShell.RegisterNativeFunction('version',ShellFunctions.NativeVersion,0,[]);

 ObjWindow:=TBESEN(Instance).ObjectGlobal;
 TBESEN(Instance).ObjectGlobal.OverwriteData('window',BESENObjectValue(ObjWindow),[bopaWRITABLE,bopaCONFIGURABLE]);
 ObjDocument.OverwriteData('window',BESENObjectValue(TBESEN(Instance).ObjectGlobal),[bopaWRITABLE,bopaCONFIGURABLE]);

 try
  try
   if not DoExit then begin
    TBESEN(Instance).InjectObject('console',{$ifndef BESENSingleStringType}BESENUTF16ToUTF8({$endif}BESENObjectConsoleSource{$ifndef BESENSingleStringType}){$endif});
   end;
  except
   on e:EBESENError do begin
    writeln(e.Name,': ',e.Message);
   end;
   on e:exception do begin
    writeln('Exception: ',e.Message);
   end;
  end;
  if length(FileName)>0 then begin
   try
    if not DoExit then begin
     v:=TBESEN(Instance).Execute({$ifndef BESENSingleStringType}BESENConvertToUTF8({$endif}BESENGetFileContent(FileName){$ifndef BESENSingleStringType}){$endif});
     Print(v);
    end;
   except
    on e:EBESENError do begin
     writeln(e.Name,': ',e.Message);
    end;
    on e:exception do begin
     writeln('Exception: ',e.Message);
    end;
   end;
  end else begin
   v.ValueType:=bvtUNDEFINED;
   v.Obj:=nil;
   while not DoExit do begin
    write('>');
    readln(ss);
    try
     v:=TBESEN(Instance).Execute({$ifndef BESENSingleStringType}BESENUTF16ToUTF8({$endif}ss{$ifndef BESENSingleStringType}){$endif});
     if not DoExit then begin
      Print(v);
      v.ValueType:=bvtUNDEFINED;
      v.Obj:=nil;
      TBESEN(Instance).GarbageCollector.CollectAll;
     end;
    except
     on e:EBESENError do begin
      writeln(e.Name,'(',TBESEN(Instance).LineNumber,'): ',e.Message);
     end;
     on e:exception do begin
      writeln('Exception(',TBESEN(Instance).LineNumber,'): ',e.Message);
     end;
    end;
   end;
  end;
 except
  on e:exception do begin
   writeln(e.Message);
  end;
 end;
 FileName:='';
 s:='';
 ShellFunctions.Destroy;
 TBESEN(Instance).Destroy;
end.
