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
unit BESENErrors;
{$i BESEN.inc}

interface

uses SysUtils,Classes,BESENConstants,BESENTypes,BESENValue;

type EBESENError=class(Exception)
      public
       OriginalMessage:TBESENString;
       Name:TBESENString;
       Value:TBESENValue;
       constructor Create; overload; virtual;
// BEGIN - To avoid "Ambiguous overloaded call to" error
       constructor Create(const Msg:string); overload; virtual;
       constructor CreateUTF16(const Msg:TBESENSTRING); overload; virtual;
// END - To avoid "Ambiguous overloaded call to" error
       constructor Create(const AValue:TBESENValue); overload; virtual;
       constructor Create(const Msg:TBESENSTRING;const AValue:TBESENValue); overload; virtual;
       constructor Create(const AName,Msg:TBESENSTRING); overload; virtual;
       constructor Create(const AName,Msg:TBESENSTRING;const AValue:TBESENValue); overload; virtual;
       destructor Destroy; override;
     end;

     EBESENUseStrict=class(EBESENError)
     end;

     EBESENInternalError=class(EBESENError)
      public
       constructor Create; overload; override;
       constructor Create(const Msg:string); overload; override;
       constructor CreateUTF16(const Msg:TBESENSTRING); overload; override;
       constructor Create(const AValue:TBESENValue); overload; override;
       constructor Create(const Msg:TBESENSTRING;const AValue:TBESENValue); overload; override;
     end;

     EBESENCompilerError=class(EBESENError)
      public
       constructor Create; overload; override;
       constructor Create(const Msg:string); overload; override;
       constructor CreateUTF16(const Msg:TBESENSTRING); overload; override;
       constructor Create(const AValue:TBESENValue); overload; override;
       constructor Create(const Msg:TBESENSTRING;const AValue:TBESENValue); overload; override;
     end;

     EBESENEvalError=class(EBESENError)
      public
       constructor Create; overload; override;
       constructor Create(const Msg:string); overload; override;
       constructor CreateUTF16(const Msg:TBESENSTRING); overload; override;
       constructor Create(const AValue:TBESENValue); overload; override;
       constructor Create(const Msg:TBESENSTRING;const AValue:TBESENValue); overload; override;
     end;

     EBESENRangeError=class(EBESENError)
      public
       constructor Create; overload; override;
       constructor Create(const Msg:string); overload; override;
       constructor CreateUTF16(const Msg:TBESENSTRING); overload; override;
       constructor Create(const AValue:TBESENValue); overload; override;
       constructor Create(const Msg:TBESENSTRING;const AValue:TBESENValue); overload; override;
     end;

     EBESENReferenceError=class(EBESENError)
      public
       constructor Create; overload; override;
       constructor Create(const Msg:string); overload; override;
       constructor CreateUTF16(const Msg:TBESENSTRING); overload; override;
       constructor Create(const AValue:TBESENValue); overload; override;
       constructor Create(const Msg:TBESENSTRING;const AValue:TBESENValue); overload; override;
     end;

     EBESENSyntaxError=class(EBESENError)
      public
       constructor Create; overload; override;
       constructor Create(const Msg:string); overload; override;
       constructor CreateUTF16(const Msg:TBESENSTRING); overload; override;
       constructor Create(const AValue:TBESENValue); overload; override;
       constructor Create(const Msg:TBESENSTRING;const AValue:TBESENValue); overload; override;
     end;

     EBESENTypeError=class(EBESENError)
      public
       constructor Create; overload; override;
       constructor Create(const Msg:string); overload; override;
       constructor CreateUTF16(const Msg:TBESENSTRING); overload; override;
       constructor Create(const AValue:TBESENValue); overload; override;
       constructor Create(const Msg:TBESENSTRING;const AValue:TBESENValue); overload; override;
     end;

     EBESENURIError=class(EBESENError)
      public
       constructor Create; overload; override;
       constructor Create(const Msg:string); overload; override;
       constructor CreateUTF16(const Msg:TBESENSTRING); overload; override;
       constructor Create(const AValue:TBESENValue); overload; override;
       constructor Create(const Msg:TBESENSTRING;const AValue:TBESENValue); overload; override;
     end;

     EBESENThrowException=class(EBESENError)
      public
       constructor Create; overload; override;
       constructor Create(const Msg:string); overload; override;
       constructor CreateUTF16(const Msg:TBESENSTRING); overload; override;
       constructor Create(const AValue:TBESENValue); overload; override;
       constructor Create(const Msg:TBESENSTRING;const AValue:TBESENValue); overload; override;
     end;

procedure BESENThrowReferenceError(const Msg:TBESENString);
procedure BESENThrowSyntaxError(const Msg:TBESENString);
procedure BESENThrowTypeError(const Msg:TBESENString);
procedure BESENThrowRangeError(const Msg:TBESENString);
procedure BESENThrowInternalError(const Msg:TBESENString);
procedure BESENThrowError(const Msg:TBESENString);
procedure BESENThrowCodeGeneratorInvalidRegister;
procedure BESENThrowRecursionLimitReached;
procedure BESENThrowNotDefined(const ARef:TBESENValue);
procedure BESENThrowReference;
procedure BESENThrowNotAccessable(const ARef:TBESENValue);
procedure BESENThrowNotReadable(const P:TBESENString);
procedure BESENThrowNotWritable(const P:TBESENString);
procedure BESENThrowNoSetter(const P:TBESENString);
procedure BESENThrowRcursivePrototypeChain;
procedure BESENThrowPut(const P:TBESENString);
procedure BESENThrowPutRecursivePrototypeChain;
procedure BESENThrowPutInvalidPrototype;
procedure BESENThrowDefineOwnProperty(const P:TBESENString);
procedure BESENThrowCaller;
procedure BESENThrowTypeErrorDeclarationBindingInstantiationAtFunctionBinding(const fn:TBESENString);
procedure BESENThrowTypeErrorNotAConstructorObject;
procedure BESENThrowTypeErrorObjectHasNoConstruct;
procedure BESENThrowTypeErrorNotAFunction;
procedure BESENThrowTypeErrorNotCallable;

implementation

uses BESEN,BESENStringUtils;

constructor EBESENError.Create;
begin
 inherited Create('');
 OriginalMessage:='';
 Name:='Error';
 Value:=BESENEmptyValue;
end;

constructor EBESENError.Create(const Msg:string);
begin
 inherited Create(Msg);
 OriginalMessage:={$ifdef Delphi2009AndUp}Msg{$else}BESENConvertToUTF8(Msg){$endif};
 Name:='Error';
 Value:=BESENEmptyValue;
end;

constructor EBESENError.CreateUTF16(const Msg:TBESENSTRING);
begin
 inherited Create({$ifdef Delphi2009AndUp}Msg{$else}BESENUTF16ToUTF8(Msg){$endif});
 OriginalMessage:=Msg;
 Name:='Error';
 Value:=BESENEmptyValue;
end;

constructor EBESENError.Create(const AValue:TBESENValue);
begin
 inherited Create('');
 OriginalMessage:='';
 Name:='Error';
 Value:=AValue;
end;

constructor EBESENError.Create(const Msg:TBESENSTRING;const AValue:TBESENValue);
begin
 inherited Create({$ifdef Delphi2009AndUp}Msg{$else}BESENUTF16ToUTF8(Msg){$endif});
 OriginalMessage:=Msg;
 Name:='Error';
 Value:=AValue;
end;

constructor EBESENError.Create(const AName,Msg:TBESENSTRING);
begin
 inherited Create({$ifdef Delphi2009AndUp}Msg{$else}BESENUTF16ToUTF8(Msg){$endif});
 OriginalMessage:=Msg;
 Name:=AName;
 Value:=BESENEmptyValue;
end;

constructor EBESENError.Create(const AName,Msg:TBESENSTRING;const AValue:TBESENValue);
begin
 inherited Create({$ifdef Delphi2009AndUp}Msg{$else}BESENUTF16ToUTF8(Msg){$endif});
 OriginalMessage:=Msg;
 Name:=AName;
 Value:=AValue;
end;

destructor EBESENError.Destroy;
begin
 OriginalMessage:='';
 Value.Str:='';
 Value.ReferenceBase.Str:='';
 Value:=BESENEmptyValue;
 Name:='';
 inherited Destroy;
end;

constructor EBESENInternalError.Create;
begin
 inherited Create;
 Name:='InternalError';
end;

constructor EBESENInternalError.Create(const Msg:string);
begin
 inherited Create(Msg);
 Name:='InternalError';
end;

constructor EBESENInternalError.CreateUTF16(const Msg:TBESENSTRING);
begin
 inherited CreateUTF16(Msg);
 Name:='InternalError';
end;

constructor EBESENInternalError.Create(const AValue:TBESENValue);
begin
 inherited Create(AValue);
 Name:='InternalError';
end;

constructor EBESENInternalError.Create(const Msg:TBESENSTRING;const AValue:TBESENValue);
begin
 inherited Create(Msg,AValue);
 Name:='InternalError';
end;

constructor EBESENCompilerError.Create;
begin
 inherited Create;
 Name:='CompilerError';
end;

constructor EBESENCompilerError.Create(const Msg:string);
begin
 inherited Create(Msg);
 Name:='CompilerError';
end;

constructor EBESENCompilerError.CreateUTF16(const Msg:TBESENSTRING);
begin
 inherited CreateUTF16(Msg);
 Name:='CompilerError';
end;

constructor EBESENCompilerError.Create(const AValue:TBESENValue);
begin
 inherited Create(AValue);
 Name:='CompilerError';
end;

constructor EBESENCompilerError.Create(const Msg:TBESENSTRING;const AValue:TBESENValue);
begin
 inherited Create(Msg,AValue);
 Name:='CompilerError';
end;

constructor EBESENEvalError.Create;
begin
 inherited Create;
 Name:='EvalError';
end;

constructor EBESENEvalError.Create(const Msg:string);
begin
 inherited Create(Msg);
 Name:='EvalError';
end;

constructor EBESENEvalError.CreateUTF16(const Msg:TBESENSTRING);
begin
 inherited CreateUTF16(Msg);
 Name:='EvalError';
end;

constructor EBESENEvalError.Create(const AValue:TBESENValue);
begin
 inherited Create(AValue);
 Name:='EvalError';
end;

constructor EBESENEvalError.Create(const Msg:TBESENSTRING;const AValue:TBESENValue);
begin
 inherited Create(Msg,AValue);
 Name:='EvalError';
end;

constructor EBESENRangeError.Create;
begin
 inherited Create;
 Name:='RangeError';
end;

constructor EBESENRangeError.Create(const Msg:string);
begin
 inherited Create(Msg);
 Name:='RangeError';
end;

constructor EBESENRangeError.CreateUTF16(const Msg:TBESENSTRING);
begin
 inherited CreateUTF16(Msg);
 Name:='RangeError';
end;

constructor EBESENRangeError.Create(const AValue:TBESENValue);
begin
 inherited Create(AValue);
 Name:='RangeError';
end;

constructor EBESENRangeError.Create(const Msg:TBESENSTRING;const AValue:TBESENValue);
begin
 inherited Create(Msg,AValue);
 Name:='RangeError';
end;

constructor EBESENReferenceError.Create;
begin
 inherited Create;
 Name:='ReferenceError';
end;

constructor EBESENReferenceError.Create(const Msg:string);
begin
 inherited Create(Msg);
 Name:='ReferenceError';
end;

constructor EBESENReferenceError.CreateUTF16(const Msg:TBESENSTRING);
begin
 inherited CreateUTF16(Msg);
 Name:='ReferenceError';
end;

constructor EBESENReferenceError.Create(const AValue:TBESENValue);
begin
 inherited Create(AValue);
 Name:='ReferenceError';
end;

constructor EBESENReferenceError.Create(const Msg:TBESENSTRING;const AValue:TBESENValue);
begin
 inherited Create(Msg,AValue);
 Name:='ReferenceError';
end;

constructor EBESENSyntaxError.Create;
begin
 inherited Create;
 Name:='SyntaxError';
end;

constructor EBESENSyntaxError.Create(const Msg:string);
begin
 inherited CreateUTF16(Msg);
 Name:='SyntaxError';
end;

constructor EBESENSyntaxError.CreateUTF16(const Msg:TBESENSTRING);
begin
 inherited CreateUTF16(Msg);
 Name:='SyntaxError';
end;

constructor EBESENSyntaxError.Create(const AValue:TBESENValue);
begin
 inherited Create(AValue);
 Name:='SyntaxError';
end;

constructor EBESENSyntaxError.Create(const Msg:TBESENSTRING;const AValue:TBESENValue);
begin
 inherited Create(Msg,AValue);
 Name:='SyntaxError';
end;

constructor EBESENTypeError.Create;
begin
 inherited Create;
 Name:='TypeError';
end;

constructor EBESENTypeError.Create(const Msg:string);
begin
 inherited CreateUTF16(Msg);
 Name:='TypeError';
end;

constructor EBESENTypeError.CreateUTF16(const Msg:TBESENSTRING);
begin
 inherited CreateUTF16(Msg);
 Name:='TypeError';
end;

constructor EBESENTypeError.Create(const AValue:TBESENValue);
begin
 inherited Create(AValue);
 Name:='TypeError';
end;

constructor EBESENTypeError.Create(const Msg:TBESENSTRING;const AValue:TBESENValue);
begin
 inherited Create(Msg,AValue);
 Name:='TypeError';
end;

constructor EBESENURIError.Create;
begin
 inherited Create;
 Name:='URIError';
end;

constructor EBESENURIError.Create(const Msg:string);
begin
 inherited CreateUTF16(Msg);
 Name:='URIError';
end;

constructor EBESENURIError.CreateUTF16(const Msg:TBESENSTRING);
begin
 inherited CreateUTF16(Msg);
 Name:='URIError';
end;

constructor EBESENURIError.Create(const AValue:TBESENValue);
begin
 inherited Create(AValue);
 Name:='URIError';
end;

constructor EBESENURIError.Create(const Msg:TBESENSTRING;const AValue:TBESENValue);
begin
 inherited Create(Msg,AValue);
 Name:='URIError';
end;

constructor EBESENThrowException.Create;
begin
 inherited Create;
 Name:='ThrowException';
end;

constructor EBESENThrowException.Create(const Msg:string);
begin
 inherited CreateUTF16(Msg);
 Name:='ThrowException';
end;

constructor EBESENThrowException.CreateUTF16(const Msg:TBESENSTRING);
begin
 inherited CreateUTF16(Msg);
 Name:='ThrowException';
end;

constructor EBESENThrowException.Create(const AValue:TBESENValue);
begin
 inherited Create(AValue);
 Name:='ThrowException';
end;

constructor EBESENThrowException.Create(const Msg:TBESENSTRING;const AValue:TBESENValue);
begin
 inherited Create(Msg,AValue);
 Name:='ThrowException';
end;

procedure BESENThrowReferenceError(const Msg:TBESENString);
begin
 raise EBESENReferenceError.CreateUTF16(Msg);
end;

procedure BESENThrowSyntaxError(const Msg:TBESENString);
begin
 raise EBESENSyntaxError.CreateUTF16(Msg);
end;

procedure BESENThrowTypeError(const Msg:TBESENString);
begin
 raise EBESENTypeError.CreateUTF16(Msg);
end;

procedure BESENThrowRangeError(const Msg:TBESENString);
begin
 raise EBESENRangeError.CreateUTF16(Msg);
end;

procedure BESENThrowInternalError(const Msg:TBESENString);
begin
 raise EBESENInternalError.CreateUTF16(Msg);
end;

procedure BESENThrowError(const Msg:TBESENString);
begin
 raise EBESENError.CreateUTF16(Msg);
end;

procedure BESENThrowCodeGeneratorInvalidRegister;
begin
 BESENThrowError('Invalid register in code generation');
end;

procedure BESENThrowRecursionLimitReached;
begin
 BESENThrowError('Recursion limit reached');
end;

procedure BESENThrowNotDefined(const ARef:TBESENValue);
begin
 BESENThrowReferenceError('"'+ARef.Str+'" is not defined');
end;

procedure BESENThrowReference;
begin
 BESENThrowReferenceError('Reference error');
end;

procedure BESENThrowNotAccessable(const ARef:TBESENValue);
begin
 BESENThrowReferenceError('"'+ARef.Str+'" is not accessable');
end;

procedure BESENThrowNotReadable(const P:TBESENString);
begin
 BESENThrowReferenceError('"'+P+'" is not readable');
end;

procedure BESENThrowNotWritable(const P:TBESENString);
begin
 BESENThrowReferenceError('"'+P+'" is not writable');
end;

procedure BESENThrowNoSetter(const P:TBESENString);
begin
  BESENThrowTypeError('"'+P+'" has no setter');
end;

procedure BESENThrowRcursivePrototypeChain;
begin
 BESENThrowTypeError('Recursive prototype chain not allowed');
end;

procedure BESENThrowPut(const P:TBESENString);
begin
 BESENThrowTypeError('Put for "'+P+'" failed');
end;

procedure BESENThrowPutRecursivePrototypeChain;
begin
 BESENThrowTypeError('Put for "__proto__" failed, because the prototype chain would be recursive');
end;

procedure BESENThrowPutInvalidPrototype;
begin
 BESENThrowTypeError('Put for "__proto__" failed, because the prototype would be invalid');
end;

procedure BESENThrowDefineOwnProperty(const P:TBESENString);
begin
 BESENThrowTypeError('DefineOwnProperty for "'+P+'" failed');
end;

procedure BESENThrowCaller;
begin
 BESENThrowTypeError('"caller" not allowed here');
end;

procedure BESENThrowTypeErrorDeclarationBindingInstantiationAtFunctionBinding(const fn:TBESENString);
begin
 BESENThrowTypeError('"'+fn+'" not writable or is a accessor descriptor');
end;

procedure BESENThrowTypeErrorNotAConstructorObject;
begin
 BESENThrowTypeError('Not a constructor object');
end;

procedure BESENThrowTypeErrorObjectHasNoConstruct;
begin
 BESENThrowTypeError('Object has no construct');
end;

procedure BESENThrowTypeErrorNotAFunction;
begin
 BESENThrowTypeError('Not a function');
end;

procedure BESENThrowTypeErrorNotCallable;
begin
 BESENThrowTypeError('Not callable');
end;

end.
