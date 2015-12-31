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
unit BESENParser;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENValue,BESENBaseObject,
     BESENCollectorObject,BESENObjectPropertyDescriptor,
     BESENLexicalEnvironment,BESENASTNodes,
     BESENEnvironmentRecord,BESENStringTree,
     BESENPointerList,
     BESENLexer;

type TBESENParserLabelSet=class(TBESENCollectorObject)
      public
       Next:TBESENParserLabelSet;
       Target:TBESENTarget;
       Continuable:longbool;
       Index:integer;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENParserLabel=class(TBESENCollectorObject)
      public
       Next:TBESENParserLabel;
       Name:TBESENString;
       LabelSet:TBESENParserLabelSet;
       Node:TBESENASTNode;
       LineNumber:integer;
       Index:integer;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENParser=class(TBESENBaseObject)
      public
       Lexer:TBESENLexer;
       WarningProc:TBESENWarningProc;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure Init;
       function Parse(IsFunction,IsJSON:boolean):TBESENASTNode;
     end;
     
implementation

uses {$ifdef BESENEmbarcaderoNextGen}System.Character,{$endif}BESEN,BESENUtils,BESENStringUtils,BESENErrors;

constructor TBESENParserLabelSet.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 Next:=nil;
 Target:=-1;
 Continuable:=false;
 Index:=-1;
end;

destructor TBESENParserLabelSet.Destroy;
begin
 Next:=nil;
 inherited Destroy;
end;

constructor TBESENParserLabel.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 Next:=nil;
 Name:='';
 LabelSet:=nil;
 Node:=nil;
 LineNumber:=0;
 Index:=-1;
end;

destructor TBESENParserLabel.Destroy;
begin
 Next:=nil;
 Node:=nil;
 LabelSet:=nil;
 Name:='';
 inherited Destroy;
end;

constructor TBESENParser.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 Lexer:=TBESENLexer.Create(Instance);
 ErrorProc:=nil;
 WarningProc:=nil;
end;

destructor TBESENParser.Destroy;
begin
 Lexer.Free;
 inherited Destroy;
end;

procedure TBESENParser.Init;
begin
 Lexer.NextChar;
end;

function TBESENParser.Parse(IsFunction,IsJSON:boolean):TBESENASTNode;
var CurrentToken:TBESENLexerToken;
    InForHeader:boolean;
    Labels:TBESENParserLabel;
    LabelSets,CurrentLabelSet:TBESENParserLabelSet;
    LabelSetList,LabelList:TBESENPointerList;
    IsInFunction,UseStrictAlreadyParsed:boolean;
 procedure AddError(const Msg:TBESENSTRING);
 begin
  TBESEN(Instance).LineNumber:=Lexer.LineNumber;
  raise EBESENSyntaxError.CreateUTF16(Msg);
 end;
 procedure AddWarning(const Msg:TBESENSTRING);
 begin
  if assigned(WarningProc) then begin
   WarningProc(Lexer.LineNumber,Msg);
  end;
 end;
 procedure NextToken;
 begin
  Lexer.GetToken(CurrentToken);
 end;
 procedure SkipToken(TokenType:TBESENLexerTokenType);
 begin
  if CurrentToken.TokenType<>TokenType then begin
   AddError('"'+BESENTokenStrings[TokenType]+'" expected');
  end;
  NextToken;
 end;
 procedure CleanUpLabels;
 var i:integer;
 begin
  for i:=0 to LabelSetList.Count-1 do begin
   TBESENParserLabelSet(LabelSetList[i]).Free;
   LabelSetList[i]:=nil;
  end;
  LabelSetList.Clear;
  for i:=0 to LabelList.Count-1 do begin
   TBESENParserLabelSet(LabelList[i]).Free;
   LabelList[i]:=nil;
  end;
  LabelList.Clear;
 end;
 function LabelSetCurrent:TBESENParserLabelSet;
 begin
  if not assigned(CurrentLabelSet) then begin
   CurrentLabelSet:=TBESENParserLabelSet.Create(Instance);
   CurrentLabelSet.Index:=LabelSetList.Add(CurrentLabelSet);
   if assigned(LabelSets) then begin
    CurrentLabelSet.Target:=LabelSets.Target+1;
   end else begin
    CurrentLabelSet.Target:=1;
   end;
   CurrentLabelSet.Next:=LabelSets;
   LabelSets:=CurrentLabelSet;
  end;
  result:=CurrentLabelSet;
 end;
 function LabelEnter(const Name:TBESENString='';const Node:TBESENASTNode=nil):TBESENParserLabel;
 var l:TBESENParserLabel;
 begin
  result:=nil;
  try
   if length(Name)<>0 then begin
    l:=Labels;
    while assigned(l) do begin
     if l.Name=Name then begin
      TBESEN(Instance).LineNumber:=Lexer.LineNumber;
      raise EBESENSyntaxError.CreateUTF16('Duplicate label "'+Name+'"');
     end;
     l:=l.Next;
    end;
   end;
   result:=TBESENParserLabel.Create(Instance);
   result.Name:=Name;
   result.LabelSet:=LabelSetCurrent;
   result.Node:=Node;
   result.LineNumber:=Lexer.LineNumber;
   result.Next:=Labels;
   result.Index:=LabelList.Add(result);
   Labels:=result;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 procedure LabelLeave;
 var OldLabels:TBESENParserLabel;
 begin
{$ifdef UseAssert}
  Assert(assigned(Labels));
{$endif}
  if assigned(Labels) then begin
   OldLabels:=Labels;
   Labels:=OldLabels.Next;
   LabelList[OldLabels.Index]:=niL;
   BESENFreeAndNil(OldLabels);
  end;
 end;
 function LabelTargetLookup(const Name:TBESENString='';const Continuable:boolean=false):TBESENTarget;
 var l:TBESENParserLabel;
 begin
  try
   l:=Labels;
   while assigned(l) do begin
    if l.Name=Name then begin
     if Continuable and not l.LabelSet.Continuable then begin
      if length(Name)=0 then begin
       continue;
      end;
      TBESEN(Instance).LineNumber:=Lexer.LineNumber;
      raise EBESENSyntaxError.CreateUTF16('Label name "'+Name+'" not suitable for continue');
     end;
     result:=l.LabelSet.Target;
     exit;
    end;
    l:=l.Next;
   end;
   if length(Name)<>0 then begin
    TBESEN(Instance).LineNumber:=Lexer.LineNumber;
    raise EBESENSyntaxError.CreateUTF16('Label name "'+Name+'" not defined, or not reachable');
   end else if Continuable then begin
    TBESEN(Instance).LineNumber:=Lexer.LineNumber;
    raise EBESENSyntaxError.Create('Continue statement not within a loop');
   end else begin
    TBESEN(Instance).LineNumber:=Lexer.LineNumber;
    raise EBESENSyntaxError.Create('Break statement not within a loop or switch');
   end;
  except
   raise;
  end;
 end;
 function ParseSourceElement:TBESENASTNodeStatement; forward;
 function ParseStatement(ResetCurrentLabelSet:boolean):TBESENASTNodeStatement; forward;
 function ParseAssignmentExpression(InFlag:boolean):TBESENASTNodeExpression; forward;
 function ParseExpression(InFlag:boolean):TBESENASTNodeExpression; forward;
 function NextIsSemicolon:boolean;
 begin
  result:=(CurrentToken.TokenType in [ltEOF,ltoCLOSEBRACE,ltoSEMICOLON]) or (CurrentToken.WasLineEnd and not InForHeader);
 end;
 procedure ParseOptionalSemicolon;
 begin
  case CurrentToken.TokenType of
   ltEOF:begin
   end;
   ltoCLOSEBRACE:begin
   end;
   ltoSEMICOLON:begin
    SkipToken(ltoSEMICOLON);
   end;
   else begin
    if not (CurrentToken.WasLineEnd and not InForHeader) then begin
     SkipToken(ltoSEMICOLON);
    end;
   end;
  end;
 end;
 procedure ParseDirective(var IsDirectivePrologue,FirstDirective:boolean;Body:TBESENASTNodeFunctionBody);
 var OldToken:TBESENLexerToken;
 begin
  if IsDirectivePrologue then begin
   if CurrentToken.TokenType=lttSTRING then begin
    OldToken:=CurrentToken;
    NextToken;
    if NextIsSemicolon then begin
     if (length(OldToken.StringValue)=10) and (copy(Lexer.Source,OldToken.OldPosition,10)='use strict') then begin
      if UseStrictAlreadyParsed then begin
       AddWarning('"use strict"/''use strict'' already seen!');
      end else begin
       UseStrictAlreadyParsed:=true;
       if not TBESEN(Instance).IsStrict then begin
        TBESEN(Instance).IsStrict:=true;
        Body.IsStrict:=true;
        if not FirstDirective then begin
         raise EBESENUseStrict.Create('Wrong "use strict"/''use strict'' position!');
        end;
       end else begin
        TBESEN(Instance).IsStrict:=true;
       end;
      end;
     end;
     FirstDirective:=false;
    end else begin
     IsDirectivePrologue:=false;
    end;
    Lexer.Restore(OldToken);
    NextToken;
   end else begin
    IsDirectivePrologue:=false;
   end;
  end;
 end;
 function ParseIdentifier(AllowEvalArguments:boolean):TBESENASTNodeIdentifier;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeIdentifier.Create(Instance);
   result.Location.LineNumber:=CurrentToken.LineNumber;
   if CurrentToken.TokenType=lttIDENTIFIER then begin
    result.Name:=CurrentToken.Name;
   end else begin
    AddError('identifier literal expected');
   end;
   if (TBESEN(Instance).IsStrict and not AllowEvalArguments) and ((result.Name='eval') or (result.Name='arguments')) then begin
    AddError('"'+result.Name+'" not allowed here');
   end;
   NextToken;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseIdentifierName(AllowEvalArguments:boolean):TBESENASTNodeIdentifier;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeIdentifier.Create(Instance);
   result.Location.LineNumber:=CurrentToken.LineNumber;
   if CurrentToken.TokenType in BESENLexerKeywordsTokens then begin
    result.Name:=BESENTokenStrings[CurrentToken.TokenType];
   end else if CurrentToken.TokenType=lttIDENTIFIER then begin
    result.Name:=CurrentToken.Name;
   end else begin
    AddError('identifier literal expected');
   end;
   if (TBESEN(Instance).IsStrict and not AllowEvalArguments) and ((result.Name='eval') or (result.Name='arguments')) then begin
    AddError('"'+result.Name+'" not allowed here');
   end;
   NextToken;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseFunctionLiteral(WithFunction,WithName:boolean):TBESENASTNodeFunctionLiteral;
 var i,j:integer;
     OldIsInFunction,OldIsStrict,OldUseStrictAlreadyParsed:boolean;
     OldToken:TBesenLexerToken;
  procedure ParseFunctionStatements;
  var IsDirectivePrologue,FirstDirective:boolean;
      Statements,i:integer;
  begin
   Statements:=0;
   try
    IsDirectivePrologue:=true;
    FirstDirective:=true;
    UseStrictAlreadyParsed:=false;
    while (CurrentToken.TokenType<>ltoCLOSEBRACE) and not Lexer.IsEOF do begin
     if IsDirectivePrologue then begin
      ParseDirective(IsDirectivePrologue,FirstDirective,result.Body);
     end;
     if Statements>=length(result.Body.Statements) then begin
      SetLength(result.Body.Statements,Statements+256);
     end;
     result.Body.Statements[Statements]:=ParseSourceElement;
     inc(Statements);
    end;
    SetLength(result.Body.Statements,Statements);
    SkipToken(ltoCLOSEBRACE);
   except
    for i:=0 to Statements-1 do begin
     BesenFreeAndNil(result.Body.Statements[i]);
    end;
    SetLength(result.Body.Statements,0);
    Statements:=0;
    raise;
   end;
  end;
  function ParseFakeReturnStatement:TBESENASTNodeReturnStatement;
  begin
   result:=nil;
   try
    result:=TBESENASTNodeReturnStatement.Create(Instance);
    result.Location.LineNumber:=Lexer.LineNumber;
    if not NextIsSemicolon then begin
     result.Expression:=ParseExpression(true);
    end;
    if NextIsSemicolon then begin
     ParseOptionalSemicolon;
    end;
   except
    BESENFreeAndNil(result);
    raise;
   end;
  end;
 var FakeReturnStatement:TBESENASTNodeReturnStatement;
 begin
  result:=nil;
  try
   OldUseStrictAlreadyParsed:=UseStrictAlreadyParsed;
   OldIsInFunction:=IsInFunction;
   OldIsStrict:=TBESEN(Instance).IsStrict;
   IsInFunction:=true;
   try
    result:=TBESENASTNodeFunctionLiteral.Create(Instance);
    result.Location.LineNumber:=CurrentToken.LineNumber;

    if WithFunction then begin
     SkipToken(ltkFUNCTION);
    end;

    if WithName or (CurrentToken.TokenType<>ltoOPENPAREN) then begin
     result.Name:=ParseIdentifier(not TBESEN(Instance).IsStrict);
    end;

    SkipToken(ltoOPENPAREN);
    i:=0;
    if (CurrentToken.TokenType<>ltoCLOSEPAREN) and not Lexer.IsEOF then begin
     while true do begin
      if i>=length(result.Body.Parameters) then begin
       SetLength(result.Body.Parameters,i+256);
      end;
      result.Body.Parameters[i]:=ParseIdentifier(not TBESEN(Instance).IsStrict);
      if TBESEN(Instance).IsStrict and (result.Body.Parameters[i] is TBESENASTNodeIdentifier) then begin
       for j:=0 to i-1 do begin
        if TBESENASTNodeIdentifier(result.Body.Parameters[i]).Name=TBESENASTNodeIdentifier(result.Body.Parameters[j]).Name then begin
         AddError('Duplicate parameter "'+TBESENASTNodeIdentifier(result.Body.Parameters[i]).Name+'"');
        end;
       end;
      end;
      inc(i);
      if Lexer.IsEOF or (CurrentToken.TokenType=ltoCLOSEPAREN) then begin
       break;
      end else if CurrentToken.TokenType=ltoCOMMA then begin
       SkipToken(ltoCOMMA);
      end;
     end;
    end;
    SetLength(result.Body.Parameters,i);
    SkipToken(ltoCLOSEPAREN);

    if ((TBESEN(Instance).Compatibility and COMPAT_JS)<>0) and (CurrentToken.TokenType<>ltoOPENBRACE) then begin
     UseStrictAlreadyParsed:=false;
     result.Body.IsStrict:=TBESEN(Instance).IsStrict;
     if CurrentToken.WasLineEnd then begin
      CurrentToken.WasLineEnd:=false;
     end;
     FakeReturnStatement:=ParseFakeReturnStatement;
     SetLength(result.Body.Statements,1);
     result.Body.Statements[0]:=FakeReturnStatement;
    end else begin
     SkipToken(ltoOPENBRACE);
     if IsFunction then begin
      IsFunction:=false;
      Lexer.LineNumber:=1;
     end;
     OldToken:=CurrentToken;
     result.Body.IsStrict:=TBESEN(Instance).IsStrict;
     try
      ParseFunctionStatements;
     except
      on e:EBESENUseStrict do begin
       result.Body.IsStrict:=TBESEN(Instance).IsStrict;
       Lexer.Restore(OldToken);
       NextToken;
       ParseFunctionStatements;
      end;
     end;
    end;
   finally
    IsInFunction:=OldIsInFunction;
    TBESEN(Instance).IsStrict:=OldIsStrict;
    UseStrictAlreadyParsed:=OldUseStrictAlreadyParsed;
   end;
   if result.Body.IsStrict then begin
    if assigned(result.Name) and ((result.Name.Name='eval') or (result.Name.Name='arguments')) then begin
     AddError('"'+result.Name.Name+'" not allowed here as function parameter name');
    end;
    for i:=0 to length(result.Body.Parameters)-1 do begin
     if (result.Body.Parameters[i].Name='eval') or (result.Body.Parameters[i].Name='arguments') then begin
      AddError('"'+result.Body.Parameters[i].Name+'" not allowed here as function parameter name');
     end;
     for j:=0 to i-1 do begin
      if result.Body.Parameters[i].Name=result.Body.Parameters[j].Name then begin
       AddError('Duplicate parameter "'+TBESENASTNodeIdentifier(result.Body.Parameters[i]).Name+'"');
      end;
     end;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseFunctionDeclaration:TBESENASTNodeFunctionDeclaration;
 var Literal:TBESENASTNodeFunctionLiteral;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeFunctionDeclaration.Create(Instance);
   result.Location.LineNumber:=CurrentToken.LineNumber;
   Literal:=ParseFunctionLiteral(true,true);
   result.Container:=Literal.Container;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseFunctionExpression(WithFunction,WithName:boolean):TBESENASTNodeFunctionExpression;
 var Literal:TBESENASTNodeFunctionLiteral;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeFunctionExpression.Create(Instance);
   result.Location.LineNumber:=CurrentToken.LineNumber;
   Literal:=ParseFunctionLiteral(WithFunction,WithName);
   result.Container:=Literal.Container;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseStatementList(InSwitch:boolean):TBESENASTNodeStatements;
 var Count:integer;
 begin
  result:=nil;
  try
   Count:=0;
   while (not ((CurrentToken.TokenType=ltoCLOSEBRACE) or (InSwitch and (CurrentToken.TokenType=ltkCASE) or (CurrentToken.TokenType=ltkDEFAULT)))) and not Lexer.IsEOF do begin
    if Count>=length(result) then begin
     SetLength(result,Count+256);
    end;
    result[Count]:=ParseStatement(true);
    inc(Count);
    if CurrentToken.TokenType=ltkFUNCTION then begin
     if (TBESEN(Instance).Compatibility and COMPAT_JS)=0 then begin
      break;
     end;
    end;
   end;
   SetLength(result,Count);
  except
   for Count:=0 to length(result)-1 do begin
    BESENFreeAndNil(result[Count]);
   end;
   SetLength(result,0);
   raise;
  end;
 end;
 function ParseBlockStatement:TBESENASTNodeBlockStatement;
 begin
  result:=nil;
  try
   SkipToken(ltoOPENBRACE);
   result:=TBESENASTNodeBlockStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   result.Statements:=ParseStatementList(false);
   SkipToken(ltoCLOSEBRACE);
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseDebuggerStatement:TBESENASTNodeDebuggerStatement;
 begin
  result:=nil;
  try
   SkipToken(ltkDEBUGGER);
   result:=TBESENASTNodeDebuggerStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   ParseOptionalSemicolon;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseBreakStatement:TBESENASTNodeBreakStatement;
 begin
  result:=nil;
  try
   SkipToken(ltkBREAK);
   result:=TBESENASTNodeBreakStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   if NextIsSemicolon then begin
    result.Target:=LabelTargetLookup('',false);
   end else begin
    if CurrentToken.TokenType=lttIDENTIFIER then begin
     if CurrentToken.WasLineEnd then begin
      AddError('Illegal line terminator after break');
     end;
     result.Identifier:=ParseIdentifier(true);
     if assigned(result.Identifier) then begin
      result.Target:=LabelTargetLookup(result.Identifier.Name,false);
     end else begin
      result.Target:=LabelTargetLookup('',false);
     end;
    end;
    ParseOptionalSemicolon;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseContinueStatement:TBESENASTNodeContinueStatement;
 begin
  result:=nil;
  try
   SkipToken(ltkCONTINUE);
   result:=TBESENASTNodeContinueStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   if NextIsSemicolon then begin
    result.Target:=LabelTargetLookup('',true);
   end else begin
    if CurrentToken.TokenType=lttIDENTIFIER then begin
     if CurrentToken.WasLineEnd then begin
      AddError('Illegal line terminator after continue');
     end;
     result.Identifier:=ParseIdentifier(true);
     if assigned(result.Identifier) then begin
      result.Target:=LabelTargetLookup(result.Identifier.Name,true);
     end else begin
      result.Target:=LabelTargetLookup('',true);
     end;
    end;
    ParseOptionalSemicolon;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseExpression(InFlag:boolean):TBESENASTNodeExpression;
 var Left:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseAssignmentExpression(InFlag);
   while not Lexer.IsEOF do begin
    case CurrentToken.TokenType of
     ltoCOMMA:begin
      SkipToken(ltoCOMMA);
      Left:=result;
      result:=TBESENASTNodeBinaryCommaExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryCommaExpression(result).LeftExpression:=Left;
      TBESENASTNodeBinaryCommaExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
     end;
     else begin
      break;
     end;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseDoStatement:TBESENASTNodeDoStatement;
 var LabelSet:TBESENParserLabelSet;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeDoStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   LabelSet:=LabelSetCurrent;
   LabelSet.Continuable:=true;
   LabelEnter('',result);
   result.Target:=LabelSet.Target;
   SkipToken(ltkDO);
   result.Statement:=ParseStatement(true);
   SkipToken(ltkWHILE);
   result.Expression:=ParseExpression(true);
   ParseOptionalSemicolon;
   LabelLeave;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseRegExpLiteral:TBESENASTNodeRegExpLiteral;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeRegExpLiteral.Create(Instance);
   result.Location.LineNumber:=CurrentToken.LineNumber;
   if CurrentToken.TokenType in [ltoDIVIDE,ltoDIVIDEASSIGNMENT] then begin
    Lexer.Restore(CurrentToken);
    if not Lexer.ParseRegExpLiteral(CurrentToken,result.Source,result.Flags) then begin
     AddError('regex literal expected');
    end;
   end else begin
    AddError('regex literal expected');
   end;
   NextToken;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseStringLiteral:TBESENASTNodeStringLiteral;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeStringLiteral.Create(Instance);
   result.Location.LineNumber:=CurrentToken.LineNumber;
   if CurrentToken.TokenType=lttSTRING then begin
    result.Value:=CurrentToken.StringValue;
   end else begin
    AddError('string literal expected');
   end;
   NextToken;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseNumbericLiteral:TBESENASTNodeNumberLiteral;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeNumberLiteral.Create(Instance);
   result.Location.LineNumber:=CurrentToken.LineNumber;
   case CurrentToken.TokenType of
    lttINTEGER:begin
     result.Value:=CurrentToken.IntValue;
    end;
    lttFLOAT:begin
     result.Value:=CurrentToken.FloatValue;
    end;
    else begin
     AddError('numberic literal expected');
    end;
   end;
   NextToken;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseObjectLiteralProperty(const Parent:TBESENASTNodeObjectLiteral;const Count:integer):TBESENASTNodeObjectLiteralProperty;
 var PropertyType:TBESENASTNodeObjectLiteralPropertyType;
     PropertyAccessorType:TBESENASTNodeObjectLiteralPropertyAccessorType;
     OldToken:TBESENLexerToken;
     i:integer;
     Key:TBESENString;
  function ParseObjectLiteralPropertyName:TBESENString;
  begin
   case CurrentToken.TokenType of
    lttIDENTIFIER:begin
     result:=CurrentToken.Name;
     NextToken;
    end;
    lttSTRING:begin
     result:=CurrentToken.StringValue;
     NextToken;
    end;
    lttINTEGER:begin
     result:=TBESEN(Instance).ToStr(BESENNumberValue(CurrentToken.IntValue));
     NextToken;
    end;
    lttFLOAT:begin
     result:=TBESEN(Instance).ToStr(BESENNumberValue(CurrentToken.FloatValue));
     NextToken;
    end;
    else begin
     if CurrentToken.TokenType in BESENLexerKeywordsTokens then begin
      result:=BESENTokenStrings[CurrentToken.TokenType];
      NextToken;
     end else begin
      AddError('identifier or numeric literal expected');
     end;
    end;
   end;
  end;
 var Literal:TBESENASTNodeFunctionLiteral;
 begin
  result:=nil;
  try
   PropertyType:=banolptDATA;
   PropertyAccessorType:=banolpatNONE;
   if (CurrentToken.TokenType=lttIDENTIFIER) and ((CurrentToken.Name='get') or (CurrentToken.Name='set')) then begin
    OldToken:=CurrentToken;
    if CurrentToken.Name='get' then begin
     PropertyType:=banolptACCESSOR;
     PropertyAccessorType:=banolpatGET;
    end else if CurrentToken.Name='set' then begin
     PropertyType:=banolptACCESSOR;
     PropertyAccessorType:=banolpatSET;
    end;
    if PropertyType=banolptACCESSOR then begin
     NextToken;
     if not (CurrentToken.TokenType in [lttIDENTIFIER,lttSTRING,lttINTEGER,lttFLOAT]) then begin
      PropertyType:=banolptDATA;
      Lexer.Restore(OldToken);
      NextToken;
     end;
    end;
   end;
   Key:=ParseObjectLiteralPropertyName;
   case PropertyType of
    banolptACCESSOR:begin
     for i:=0 to Count-1 do begin
      if Key=Parent.Properties[i].Name then begin
       case Parent.Properties[i].PropertyType of
        banolptACCESSOR:begin
         if Parent.Properties[i].PropertyAccessorType=PropertyAccessorType then begin
          case PropertyAccessorType of
           banolpatGET:begin
            AddError('Duplicate accessor property getter "'+Key+'"');
           end;
           banolpatSET:begin
            AddError('Duplicate accessor property setter "'+Key+'"');
           end;
           else begin
            AddError('Invalid property "'+Key+'"');
           end;
          end;
         end;
        end;
        banolptDATA:begin
         AddError('Invalid property "'+Key+'"');
        end;
       end;
      end;
     end;
     result:=TBESENASTNodeObjectLiteralProperty.Create(Instance);
     result.PropertyType:=PropertyType;
     result.PropertyAccessorType:=PropertyAccessorType;
     result.Name:=Key;
     Literal:=ParseFunctionLiteral(false,false);
     result.Container:=Literal.Container;
     if not ((PropertyAccessorType in [banolpatGET,banolpatSET]) and (assigned(Literal) and assigned(Literal.Body))) then begin
      AddError('Invalid property "'+Key+'"');
     end else begin
      case PropertyAccessorType of
       banolpatGET:begin
        if length(Literal.Body.Parameters)>0 then begin
         AddError('Getter must have no parameters');
        end;
       end;
       banolpatSET:begin
        if length(Literal.Body.Parameters)<>1 then begin
         AddError('Setter must have only one parameter');
        end;
       end;
       else begin
        AddError('Invalid property "'+Key+'"');
       end;
      end;
     end;
    end;
    banolptDATA:begin
     for i:=0 to Count-1 do begin
      if Key=Parent.Properties[i].Name then begin
       case Parent.Properties[i].PropertyType of
        banolptACCESSOR:begin
         AddError('Invalid property "'+Key+'"');
        end;
        banolptDATA:begin
         if TBESEN(Instance).IsStrict then begin
          AddError('Duplicate data property "'+Key+'"');
         end;
        end;
       end;
      end;
     end;
     result:=TBESENASTNodeObjectLiteralProperty.Create(Instance);
     result.PropertyType:=banolptDATA;
     result.Location.LineNumber:=CurrentToken.LineNumber;
     result.Name:=Key;
     SkipToken(ltoCOLON);
     result.Value:=ParseAssignmentExpression(true);
    end;
    else begin
     AddError('Invalid property "'+Key+'"');
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseObjectLiteral:TBESENASTNodeObjectLiteral;
 var Count:integer;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeObjectLiteral.Create(Instance);
   result.Location.LineNumber:=CurrentToken.LineNumber;
   result.Properties:=nil;
   Count:=0;
   SkipToken(ltoOPENBRACE);
   while (CurrentToken.TokenType<>ltoCLOSEBRACE) and not Lexer.IsEOF do begin
    if Count>=length(result.Properties) then begin
     SetLength(result.Properties,Count+256);
    end;
    result.Properties[Count]:=ParseObjectLiteralProperty(result,Count);
    inc(Count);
    if CurrentToken.TokenType<>ltoCLOSEBRACE then begin
     if CurrentToken.TokenType=ltoCOMMA then begin
      SkipToken(ltoCOMMA);
     end else begin
      AddError(''','' or ''}'' expected');
      break;
     end;
    end;
   end;
   SetLength(result.Properties,Count);
   SkipToken(ltoCLOSEBRACE);
  except
   for Count:=0 to length(result.Properties)-1 do begin
    BESENFreeAndNil(result.Properties[Count]);
   end;
   SetLength(result.Properties,0);
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseArrayLiteral:TBESENASTNodeArrayLiteral;
 var Count:integer;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeArrayLiteral.Create(Instance);
   result.Location.LineNumber:=CurrentToken.LineNumber;
   SkipToken(ltoOPENSQUARE);
   result.Elements:=nil;
   Count:=0;
   while (CurrentToken.TokenType<>ltoCLOSESQUARE) and not Lexer.IsEOF do begin
    if Count>=length(result.Elements) then begin
     SetLength(result.Elements,Count+256);
    end;
    if CurrentToken.TokenType=ltoCOMMA then begin
     result.Elements[Count]:=nil;
    end else begin
     result.Elements[Count]:=ParseAssignmentExpression(true);
    end;
    inc(Count);
    if CurrentToken.TokenType<>ltoCLOSESQUARE then begin
     if CurrentToken.TokenType=ltoCOMMA then begin
      SkipToken(ltoCOMMA);
     end else begin
      AddError(''','' or '']'' expected');
      break;
     end;
    end;
   end;
   SetLength(result.Elements,Count);
   SkipToken(ltoCLOSESQUARE);
  except
   for Count:=0 to length(result.Elements)-1 do begin
    BESENFreeAndNil(result.Elements[Count]);
   end;
   SetLength(result.Elements,0);
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParsePrimaryExpression:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   case CurrentToken.TokenType of
    ltkTHIS:begin
     result:=TBESENASTNodeThisLiteral.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltkTHIS);
    end;
    ltkNULL:begin
     result:=TBESENASTNodeNullLiteral.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltkNULL);
    end;
    ltkTRUE:begin
     result:=TBESENASTNodeBooleanLiteral.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeBooleanLiteral(result).Value:=true;
     SkipToken(ltkTRUE);
    end;
    ltkFALSE:begin
     result:=TBESENASTNodeBooleanLiteral.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeBooleanLiteral(result).Value:=false;
     SkipToken(ltkFALSE);
    end;
    ltoOPENPAREN:begin
     SkipToken(ltoOPENPAREN);
     result:=ParseExpression(true);
     SkipToken(ltoCLOSEPAREN);
    end;
    ltoOPENBRACE:begin
     result:=ParseObjectLiteral;
    end;
    ltoOPENSQUARE:begin
     result:=ParseArrayLiteral;
    end;
    lttIDENTIFIER:begin
     result:=ParseIdentifier(true);
    end;
    ltoDIVIDE,ltoDIVIDEASSIGNMENT:begin
     result:=ParseRegExpLiteral;
    end;
    lttSTRING:begin
     result:=ParseStringLiteral;
    end;
    lttINTEGER,lttFLOAT:begin
     result:=ParseNumbericLiteral;
    end;
    else begin
     AddError('identifier or literal expected');
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseArgumentList:TBESENASTNodeExpressions;
 var Count:integer;  
 begin
  result:=nil;
  try
   Count:=0;
   SkipToken(ltoOPENPAREN);
   if (CurrentToken.TokenType<>ltoCLOSEPAREN) and not Lexer.IsEOF then begin
    while true do begin
     if Count>=length(result) then begin
      SetLength(result,Count+256);
     end;
     result[Count]:=ParseAssignmentExpression(true);
     inc(Count);
     if Lexer.IsEOF or (CurrentToken.TokenType=ltoCLOSEPAREN) then begin
      break;
     end else if CurrentToken.TokenType=ltoCOMMA then begin
      SkipToken(ltoCOMMA);
     end else begin
      break;
     end;
    end;
   end;
   SkipToken(ltoCLOSEPAREN);
   SetLength(result,Count);
  except
   for Count:=0 to length(result)-1 do begin
    BESENFreeAndNil(result[Count]);
   end;
   SetLength(result,0);
   raise;
  end;
 end;
 function ParseLeftHandSideAndMemberExpression(IsLeftHandSideExpression:boolean=true):TBESENASTNodeExpression;
 var Expression:TBESENASTNodeExpression;
     Identifier:TBESENASTNodeIdentifier;
 begin
  result:=nil;
  try
   case CurrentToken.TokenType of
    ltkNEW:begin
     SkipToken(ltkNEW);
     result:=TBESENASTNodeNewExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeNewExpression(result).TheFunction:=ParseLeftHandSideAndMemberExpression(false);
     if CurrentToken.TokenType=ltoOPENPAREN then begin
      TBESENASTNodeNewExpression(result).Arguments:=ParseArgumentList;
     end else begin
      TBESENASTNodeNewExpression(result).Arguments:=nil;
     end;
    end;
    ltkFUNCTION:begin
     result:=ParseFunctionExpression(true,false);
    end;
    else begin
     result:=ParsePrimaryExpression;
    end;
   end;
   while not Lexer.IsEOF do begin
    if IsLeftHandSideExpression and (CurrentToken.TokenType=ltoOPENPAREN) then begin
     Expression:=result;
     result:=TBESENASTNodeCallExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeCallExpression(result).TheFunction:=Expression;
     TBESENASTNodeCallExpression(result).Arguments:=ParseArgumentList;
    end else if CurrentToken.TokenType=ltoOPENSQUARE then begin
     Expression:=result;
     result:=TBESENASTNodePropertyExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodePropertyExpression(result).Dot:=false;
     TBESENASTNodePropertyExpression(result).LeftExpression:=Expression;
     SkipToken(ltoOPENSQUARE);
     TBESENASTNodePropertyExpression(result).RightExpression:=ParseExpression(true);
     if TBESEN(Instance).IsStrict then begin
      if assigned(TBESENASTNodePropertyExpression(result).RightExpression) then begin
       if TBESENASTNodePropertyExpression(result).RightExpression is TBESENASTNodeStringLiteral then begin
        if ((TBESENASTNodeStringLiteral(TBESENASTNodePropertyExpression(result).RightExpression).Value='eval') or (TBESENASTNodeStringLiteral(TBESENASTNodePropertyExpression(result).RightExpression).Value='arguments')) then begin
         AddError('"'+TBESENASTNodeStringLiteral(TBESENASTNodePropertyExpression(result).RightExpression).Value+'" not allowed here');
        end;
       end;
      end;
     end;
     SkipToken(ltoCLOSESQUARE);
    end else if CurrentToken.TokenType=ltoDOT then begin
     Expression:=result;
     result:=TBESENASTNodePropertyExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodePropertyExpression(result).Dot:=true;
     TBESENASTNodePropertyExpression(result).LeftExpression:=Expression;
     SkipToken(ltoDOT);
     TBESENASTNodePropertyExpression(result).RightExpression:=TBESENASTNodeStringLiteral.Create(Instance);
     Identifier:=ParseIdentifierName(true);
     TBESENASTNodeStringLiteral(TBESENASTNodePropertyExpression(result).RightExpression).Value:=Identifier.Name;
     Identifier.Free;
    end else begin
     break;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParsePostfixExpression:TBESENASTNodeExpression;
 var Expression:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseLeftHandSideAndMemberExpression;
   case CurrentToken.TokenType of
    ltoPLUSPLUS:begin
     if CurrentToken.WasLineEnd then begin
      AddError('Illegal line terminator before postfix increment');
     end;
     Expression:=result;
     result:=TBESENASTNodePostfixIncExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltoPLUSPLUS);
     TBESENASTNodePostfixIncExpression(result).SubExpression:=Expression;
    end;
    ltoMINUSMINUS:begin
     if CurrentToken.WasLineEnd then begin
      AddError('Illegal line terminator before postfix decrement');
     end;       
     Expression:=result;
     result:=TBESENASTNodePostfixDecExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltoMINUSMINUS);
     TBESENASTNodePostfixDecExpression(result).SubExpression:=Expression;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseUnaryExpression:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   case CurrentToken.TokenType of
    ltoPLUSPLUS:begin
     result:=TBESENASTNodePrefixIncExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltoPLUSPLUS);
     TBESENASTNodePrefixIncExpression(result).SubExpression:=ParseUnaryExpression;
    end;
    ltoMINUSMINUS:begin
     result:=TBESENASTNodePrefixDecExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltoMINUSMINUS);
     TBESENASTNodePrefixdecExpression(result).SubExpression:=ParseUnaryExpression;
    end;
    ltoPLUS:begin
     result:=TBESENASTNodeUnaryPlusExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltoPLUS);
     TBESENASTNodeUnaryPlusExpression(result).SubExpression:=ParseUnaryExpression;
    end;
    ltoMINUS:begin
     result:=TBESENASTNodeUnaryMinusExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltoMINUS);
     TBESENASTNodeUnaryMinusExpression(result).SubExpression:=ParseUnaryExpression;
    end;
    ltoBITWISENOT:begin
     result:=TBESENASTNodeUnaryBitwiseNotExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltoBITWISENOT);
     TBESENASTNodeUnaryBitwiseNotExpression(result).SubExpression:=ParseUnaryExpression;
    end;
    ltoLOGICALNOT:begin
     result:=TBESENASTNodeUnaryLogicalNotExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltoLOGICALNOT);
     TBESENASTNodeUnaryLogicalNotExpression(result).SubExpression:=ParseUnaryExpression;
    end;
    ltkVOID:begin
     result:=TBESENASTNodeUnaryVoidExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltkVOID);
     TBESENASTNodeUnaryVoidExpression(result).SubExpression:=ParseUnaryExpression;
    end;
    ltkTYPEOF:begin
     result:=TBESENASTNodeUnaryTypeOfExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltkTYPEOF);
     TBESENASTNodeUnaryTypeOfExpression(result).SubExpression:=ParseUnaryExpression;
    end;
    ltkDELETE:begin
     result:=TBESENASTNodeDeleteExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     SkipToken(ltkDELETE);
     TBESENASTNodeDeleteExpression(result).SubExpression:=ParseUnaryExpression;
    end;
    else begin
     result:=ParsePostfixExpression;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseMultiplyExpression:TBESENASTNodeExpression;
 var Left:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseUnaryExpression;
   while not Lexer.IsEOF do begin
    case CurrentToken.TokenType of
     ltoDIVIDE:begin
      Left:=result;
      result:=TBESENASTNodeBinaryDivideExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryDivideExpression(result).LeftExpression:=Left;
      SkipToken(ltoDIVIDE);
      TBESENASTNodeBinaryDivideExpression(result).RightExpression:=ParseUnaryExpression;
     end;
     ltoMODULO:begin
      Left:=result;
      result:=TBESENASTNodeBinaryModuloExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryModuloExpression(result).LeftExpression:=Left;
      SkipToken(ltoMODULO);
      TBESENASTNodeBinaryModuloExpression(result).RightExpression:=ParseUnaryExpression;
     end;
     ltoMULTIPLY:begin
      Left:=result;
      result:=TBESENASTNodeBinaryMultiplyExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryMultiplyExpression(result).LeftExpression:=Left;
      SkipToken(ltoMULTIPLY);
      TBESENASTNodeBinaryMultiplyExpression(result).RightExpression:=ParseUnaryExpression;
     end;
     else begin
      break;
     end;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseAdditionExpression:TBESENASTNodeExpression;
 var Left:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseMultiplyExpression;
   while not Lexer.IsEOF do begin
    case CurrentToken.TokenType of
     ltoPLUS:begin
      Left:=result;
      result:=TBESENASTNodeBinaryPlusExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryPlusExpression(result).LeftExpression:=Left;
      SkipToken(ltoPLUS);
      TBESENASTNodeBinaryPlusExpression(result).RightExpression:=ParseMultiplyExpression;
     end;
     ltoMINUS:begin
      Left:=result;
      result:=TBESENASTNodeBinaryMinusExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryMinusExpression(result).LeftExpression:=Left;
      SkipToken(ltoMINUS);
      TBESENASTNodeBinaryMinusExpression(result).RightExpression:=ParseMultiplyExpression;
     end;
     else begin
      break;
     end;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseShiftExpression:TBESENASTNodeExpression;
 var Left:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseAdditionExpression;
   while not Lexer.IsEOF do begin
    case CurrentToken.TokenType of
     ltoSHIFTLEFT:begin
      Left:=result;
      result:=TBESENASTNodeBinaryShiftLeftExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryShiftLeftExpression(result).LeftExpression:=Left;
      SkipToken(ltoSHIFTLEFT);
      TBESENASTNodeBinaryShiftLeftExpression(result).RightExpression:=ParseAdditionExpression;
     end;
     ltoSHIFTRIGHT:begin
      Left:=result;
      result:=TBESENASTNodeBinaryShiftRightExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryShiftRightExpression(result).LeftExpression:=Left;
      SkipToken(ltoSHIFTRIGHT);
      TBESENASTNodeBinaryShiftRightExpression(result).RightExpression:=ParseAdditionExpression;
     end;
     ltoSHIFTRIGHTUNSIGNED:begin
      Left:=result;
      result:=TBESENASTNodeBinaryShiftRightUnsignedExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryShiftRightUnsignedExpression(result).LeftExpression:=Left;
      SkipToken(ltoSHIFTRIGHTUNSIGNED);
      TBESENASTNodeBinaryShiftRightUnsignedExpression(result).RightExpression:=ParseAdditionExpression;
     end;
     else begin
      break;
     end;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseRelationalExpression(InFlag:boolean):TBESENASTNodeExpression;
 var Left:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseShiftExpression;
   while not Lexer.IsEOF do begin
    case CurrentToken.TokenType of
     ltoGREATERTHAN:begin
      Left:=result;
      result:=TBESENASTNodeBinaryGreaterThanExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryGreaterThanExpression(result).LeftExpression:=Left;
      SkipToken(ltoGREATERTHAN);
      TBESENASTNodeBinaryGreaterThanExpression(result).RightExpression:=ParseShiftExpression;
     end;
     ltoGREATERTHANOREQUAL:begin
      Left:=result;
      result:=TBESENASTNodeBinaryGreaterThanOrEqualExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryGreaterThanOrEqualExpression(result).LeftExpression:=Left;
      SkipToken(ltoGREATERTHANOREQUAL);
      TBESENASTNodeBinaryGreaterThanOrEqualExpression(result).RightExpression:=ParseShiftExpression;
     end;
     ltoLESSTHAN:begin
      Left:=result;
      result:=TBESENASTNodeBinaryLessThanExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryLessThanExpression(result).LeftExpression:=Left;
      SkipToken(ltoLESSTHAN);
      TBESENASTNodeBinaryLessThanExpression(result).RightExpression:=ParseShiftExpression;
     end;
     ltoLESSTHANOREQUAL:begin
      Left:=result;
      result:=TBESENASTNodeBinaryLessThanOrEqualExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryLessThanOrEqualExpression(result).LeftExpression:=Left;
      SkipToken(ltoLESSTHANOREQUAL);
      TBESENASTNodeBinaryLessThanOrEqualExpression(result).RightExpression:=ParseShiftExpression;
     end;
     ltkINSTANCEOF:begin
      Left:=result;
      result:=TBESENASTNodeBinaryInstanceOfExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryInstanceOfExpression(result).LeftExpression:=Left;
      SkipToken(ltkINSTANCEOF);
      TBESENASTNodeBinaryInstanceOfExpression(result).RightExpression:=ParseShiftExpression;
     end;
     ltkIN:begin
      if InFlag then begin
       Left:=result;
       result:=TBESENASTNodeBinaryInExpression.Create(Instance);
       result.Location.LineNumber:=CurrentToken.LineNumber;
       TBESENASTNodeBinaryInExpression(result).LeftExpression:=Left;
       SkipToken(ltkIN);
       TBESENASTNodeBinaryInExpression(result).RightExpression:=ParseShiftExpression;
      end else begin
       break;
      end;
     end;
     else begin
      break;
     end;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseEqualityExpression(InFlag:boolean):TBESENASTNodeExpression;
 var Left:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseRelationalExpression(InFlag);
   while not Lexer.IsEOF do begin
    case CurrentToken.TokenType of
     ltoEQUALEQUAL:begin
      Left:=result;
      result:=TBESENASTNodeBinaryEqualEqualExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryEqualEqualExpression(result).LeftExpression:=Left;
      SkipToken(ltoEQUALEQUAL);
      TBESENASTNodeBinaryEqualEqualExpression(result).RightExpression:=ParseRelationalExpression(InFlag);
     end;
     ltoEQUALEQUALEQUAL:begin
      Left:=result;
      result:=TBESENASTNodeBinaryEqualEqualEqualExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryEqualEqualEqualExpression(result).LeftExpression:=Left;
      SkipToken(ltoEQUALEQUALEQUAL);
      TBESENASTNodeBinaryEqualEqualEqualExpression(result).RightExpression:=ParseRelationalExpression(InFlag);
     end;
     ltoNOTEQUAL:begin
      Left:=result;
      result:=TBESENASTNodeBinaryNotEqualExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryNotEqualExpression(result).LeftExpression:=Left;
      SkipToken(ltoNOTEQUAL);
      TBESENASTNodeBinaryNotEqualExpression(result).RightExpression:=ParseRelationalExpression(InFlag);
     end;
     ltoNOTEQUALEQUAL:begin
      Left:=result;
      result:=TBESENASTNodeBinaryNotEqualEqualExpression.Create(Instance);
      result.Location.LineNumber:=CurrentToken.LineNumber;
      TBESENASTNodeBinaryNotEqualEqualExpression(result).LeftExpression:=Left;
      SkipToken(ltoNOTEQUALEQUAL);
      TBESENASTNodeBinaryNotEqualEqualExpression(result).RightExpression:=ParseRelationalExpression(InFlag);
     end;
     else begin
      break;
     end;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseBitwiseAndExpression(InFlag:boolean):TBESENASTNodeExpression;
 var Left:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseEqualityExpression(InFlag);
   while not Lexer.IsEOF do begin
    if CurrentToken.TokenType=ltoBITWISEAND then begin
     Left:=result;
     result:=TBESENASTNodeBinaryBitwiseAndExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeBinaryBitwiseAndExpression(result).LeftExpression:=Left;
     SkipToken(ltoBITWISEAND);
     TBESENASTNodeBinaryBitwiseAndExpression(result).RightExpression:=ParseEqualityExpression(InFlag);
    end else begin
     break;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseBitwiseXorExpression(InFlag:boolean):TBESENASTNodeExpression;
 var Left:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseBitwiseAndExpression(InFlag);
   while not Lexer.IsEOF do begin
    if CurrentToken.TokenType=ltoBITWISEXOR then begin
     Left:=result;
     result:=TBESENASTNodeBinaryBitwiseXorExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeBinaryBitwiseXorExpression(result).LeftExpression:=Left;
     SkipToken(ltoBITWISEXOR);
     TBESENASTNodeBinaryBitwiseXorExpression(result).RightExpression:=ParseBitwiseAndExpression(InFlag);
    end else begin
     break;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseBitwiseOrExpression(InFlag:boolean):TBESENASTNodeExpression;
 var Left:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseBitwiseXorExpression(InFlag);
   while not Lexer.IsEOF do begin
    if CurrentToken.TokenType=ltoBITWISEOR then begin
     Left:=result;
     result:=TBESENASTNodeBinaryBitwiseOrExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeBinaryBitwiseOrExpression(result).LeftExpression:=Left;
     SkipToken(ltoBITWISEOR);
     TBESENASTNodeBinaryBitwiseOrExpression(result).RightExpression:=ParseBitwiseXorExpression(InFlag);
    end else begin
     break;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseLogicalAndExpression(InFlag:boolean):TBESENASTNodeExpression;
 var Left:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseBitwiseOrExpression(InFlag);
   while not Lexer.IsEOF do begin
    if CurrentToken.TokenType=ltoLOGICALAND then begin
     Left:=result;
     result:=TBESENASTNodeLogicalAndExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeLogicalAndExpression(result).LeftExpression:=Left;
     SkipToken(ltoLOGICALAND);
     TBESENASTNodeLogicalAndExpression(result).RightExpression:=ParseBitwiseOrExpression(InFlag);
    end else begin
     break;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseLogicalOrExpression(InFlag:boolean):TBESENASTNodeExpression;
 var Left:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseLogicalAndExpression(InFlag);
   while not Lexer.IsEOF do begin
    if CurrentToken.TokenType=ltoLOGICALOR then begin
     Left:=result;
     result:=TBESENASTNodeLogicalOrExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeLogicalOrExpression(result).LeftExpression:=Left;
     SkipToken(ltoLOGICALOR);
     TBESENASTNodeLogicalOrExpression(result).RightExpression:=ParseLogicalAndExpression(InFlag);
    end else begin
     break;
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseConditionalExpression(InFlag:boolean):TBESENASTNodeExpression;
 var Expression:TBESENASTNodeExpression;
 begin
  result:=nil;
  try
   result:=ParseLogicalOrExpression(InFlag);
   if CurrentToken.TokenType=ltoCONDITIONAL then begin
    Expression:=result;
    result:=TBESENASTNodeConditionalExpression.Create(Instance);
    result.Location.LineNumber:=CurrentToken.LineNumber;
    TBESENASTNodeConditionalExpression(result).Expression:=Expression;
    SkipToken(ltoCONDITIONAL);
    TBESENASTNodeConditionalExpression(result).TrueExpression:=ParseAssignmentExpression(InFlag);
    SkipToken(ltoCOLON);
    TBESENASTNodeConditionalExpression(result).FalseExpression:=ParseAssignmentExpression(InFlag);
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseAssignmentExpression(InFlag:boolean):TBESENASTNodeExpression;
 var Left:TBESENASTNodeExpression;
 begin
  result:=nil;
  Left:=nil;
  try
   result:=ParseConditionalExpression(InFlag);
   case CurrentToken.TokenType of
    ltoASSIGNMENT:begin
     Left:=result;
     result:=TBESENASTNodeAssignmentExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeAssignmentExpression(result).LeftExpression:=Left;
     SkipToken(ltoASSIGNMENT);
     TBESENASTNodeAssignmentExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
    end;
    ltoMULTIPLYASSIGNMENT:begin
     Left:=result;
     result:=TBESENASTNodeAssignmentMultiplyExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeAssignmentMultiplyExpression(result).LeftExpression:=Left;
     SkipToken(ltoMULTIPLYASSIGNMENT);
     TBESENASTNodeAssignmentMultiplyExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
    end;
    ltoDIVIDEASSIGNMENT:begin
     Left:=result;
     result:=TBESENASTNodeAssignmentDivideExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeAssignmentDivideExpression(result).LeftExpression:=Left;
     SkipToken(ltoDIVIDEASSIGNMENT);
     TBESENASTNodeAssignmentDivideExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
    end;
    ltoMODULOASSIGNMENT:begin
     Left:=result;
     result:=TBESENASTNodeAssignmentModuloExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeAssignmentModuloExpression(result).LeftExpression:=Left;
     SkipToken(ltoMODULOASSIGNMENT);
     TBESENASTNodeAssignmentModuloExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
    end;
    ltoPLUSASSIGNMENT:begin
     Left:=result;
     result:=TBESENASTNodeAssignmentPlusExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeAssignmentPlusExpression(result).LeftExpression:=Left;
     SkipToken(ltoPLUSASSIGNMENT);
     TBESENASTNodeAssignmentPlusExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
    end;
    ltoMINUSASSIGNMENT:begin
     Left:=result;
     result:=TBESENASTNodeAssignmentMinusExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeAssignmentMinusExpression(result).LeftExpression:=Left;
     SkipToken(ltoMINUSASSIGNMENT);
     TBESENASTNodeAssignmentMinusExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
    end;
    ltoSHIFTLEFTASSIGNMENT:begin
     Left:=result;
     result:=TBESENASTNodeAssignmentShiftLeftExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeAssignmentShiftLeftExpression(result).LeftExpression:=Left;
     SkipToken(ltoSHIFTLEFTASSIGNMENT);
     TBESENASTNodeAssignmentShiftLeftExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
    end;
    ltoSHIFTRIGHTASSIGNMENT:begin
     Left:=result;
     result:=TBESENASTNodeAssignmentShiftRightExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeAssignmentShiftRightExpression(result).LeftExpression:=Left;
     SkipToken(ltoSHIFTRIGHTASSIGNMENT);
     TBESENASTNodeAssignmentShiftRightExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
    end;
    ltoSHIFTRIGHTUNSIGNEDASSIGNMENT:begin
     Left:=result;
     result:=TBESENASTNodeAssignmentShiftRightUnsignedExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeAssignmentShiftRightUnsignedExpression(result).LeftExpression:=Left;
     SkipToken(ltoSHIFTRIGHTUNSIGNEDASSIGNMENT);
     TBESENASTNodeAssignmentShiftRightUnsignedExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
    end;
    ltoBITWISEANDASSIGNMENT:begin
     Left:=result;
     result:=TBESENASTNodeAssignmentBitwiseAndExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeAssignmentBitwiseAndExpression(result).LeftExpression:=Left;
     SkipToken(ltoBITWISEANDASSIGNMENT);
     TBESENASTNodeAssignmentBitwiseAndExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
    end;
    ltoBITWISEXORASSIGNMENT:begin
     Left:=result;
     result:=TBESENASTNodeAssignmentBitwiseXorExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeAssignmentBitwiseXorExpression(result).LeftExpression:=Left;
     SkipToken(ltoBITWISEXORASSIGNMENT);
     TBESENASTNodeAssignmentBitwiseXorExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
    end;
    ltoBITWISEORASSIGNMENT:begin
     Left:=result;
     result:=TBESENASTNodeAssignmentBitwiseOrExpression.Create(Instance);
     result.Location.LineNumber:=CurrentToken.LineNumber;
     TBESENASTNodeAssignmentBitwiseOrExpression(result).LeftExpression:=Left;
     SkipToken(ltoBITWISEORASSIGNMENT);
     TBESENASTNodeAssignmentBitwiseOrExpression(result).RightExpression:=ParseAssignmentExpression(InFlag);
    end;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseVariableDeclaration(InFlag:boolean):TBESENASTNodeVariableDeclaration;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeVariableDeclaration.Create(Instance);
   result.Location.LineNumber:=CurrentToken.LineNumber;
   result.Identifier:=ParseIdentifier(not TBESEN(Instance).IsStrict);
   if CurrentToken.TokenType=ltoASSIGNMENT then begin
    SkipToken(ltoASSIGNMENT);
    result.Expression:=ParseAssignmentExpression(InFlag);
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseForStatement:TBESENASTNodeStatement;
 var OldInForHeader:boolean;
     State,i,ln:integer;
     Declaration:TBESENASTNodeVariableDeclaration;
     Initial,Expression,Variable:TBESENASTNodeExpression;
     VariableExpression:TBESENASTNodeVariableExpression;
     LabelSet:TBESENParserLabelSet;
     ALabel:TBESENParserLabel;
 begin
  ln:=Lexer.LineNumber;
  result:=nil;
  Initial:=nil;
  Declaration:=nil;
  Variable:=nil;
  Expression:=nil;
  try
   LabelSet:=LabelSetCurrent;
   LabelSet.Continuable:=true;
   ALabel:=LabelEnter('',nil);

   SkipToken(ltkFOR);

   SkipToken(ltoOPENPAREN);
   OldInForHeader:=InForHeader;
   InForHeader:=true;
   State:=0;
   while not Lexer.IsEOF do begin
    case State of
     0:begin
      if CurrentToken.TokenType=ltkVAR then begin
       State:=1;
      end else if CurrentToken.TokenType<>ltoSEMICOLON then begin
       State:=2;
      end else begin
       State:=5;
      end;
     end;
     1:begin
      SkipToken(ltkVAR);
      Declaration:=ParseVariableDeclaration(false);
      if (CurrentToken.TokenType=ltkIN) and (assigned(Declaration) and not assigned(Declaration.Expression)) then begin
       Variable:=Declaration;
       Declaration:=nil;
       State:=3;
      end else begin
       State:=4;
      end;
     end;
     2:begin
      Initial:=ParseExpression(false);
      if CurrentToken.TokenType=ltkIN then begin
       Variable:=Initial;
       Initial:=nil;
       State:=3;
      end else begin
       State:=5;
      end;
     end;
     3:begin
      SkipToken(ltkIN);
      Expression:=ParseExpression(true);
      SkipToken(ltoCLOSEPAREN);
      InForHeader:=OldInForHeader;
      result:=TBESENASTNodeForInStatement.Create(Instance);
      TBESENASTNodeForInStatement(result).Target:=LabelSet.Target;
      TBESENASTNodeForInStatement(result).Variable:=Variable;
      TBESENASTNodeForInStatement(result).Expression:=Expression;
      ALabel.Node:=result;
      Variable:=nil;
      Expression:=nil;
      break;
     end;
     4:begin
      VariableExpression:=TBESENASTNodeVariableExpression.Create(Instance);
      Initial:=VariableExpression;
      SetLength(VariableExpression.Declarations,1);
      VariableExpression.Declarations[0]:=Declaration;
      Declaration:=nil;
      i:=1;
      while (CurrentToken.TokenType=ltoCOMMA) and not Lexer.IsEOF do begin
       SkipToken(ltoCOMMA);
       if i>=length(VariableExpression.Declarations) then begin
        SetLength(VariableExpression.Declarations,i+256);
       end;
       VariableExpression.Declarations[i]:=ParseVariableDeclaration(false);
       inc(i);
      end;
      SetLength(VariableExpression.Declarations,i);
      State:=5;
     end;
     5:begin
      result:=TBESENASTNodeForStatement.Create(Instance);
      TBESENASTNodeForStatement(result).Initial:=Initial;
      TBESENASTNodeForStatement(result).Target:=LabelSet.Target;
      ALabel.Node:=result;
      Initial:=nil;
      SkipToken(ltoSEMICOLON);
      if CurrentToken.TokenType<>ltoSEMICOLON then begin
       TBESENASTNodeForStatement(result).Condition:=ParseExpression(true);
      end;
      SkipToken(ltoSEMICOLON);
      if CurrentToken.TokenType<>ltoCLOSEPAREN then begin
       TBESENASTNodeForStatement(result).Increment:=ParseExpression(true);
      end;
      SkipToken(ltoCLOSEPAREN);
      break;
     end;
    end;
   end;
   InForHeader:=OldInForHeader;
   if assigned(result) then begin
    if result is TBESENASTNodeForStatement then begin
     TBESENASTNodeForStatement(result).Statement:=ParseStatement(true);
    end else if result is TBESENASTNodeForInStatement then begin
     TBESENASTNodeForInStatement(result).Statement:=ParseStatement(true);
    end;
   end else begin
    result:=TBESENASTNodeEmptyStatement.Create(Instance);
   end;
   result.Location.LineNumber:=ln;
   LabelLeave;
  except
   BESENFreeAndNil(Initial);
   BESENFreeAndNil(Declaration);
   BESENFreeAndNil(Variable);
   BESENFreeAndNil(Expression);
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseIfStatement:TBESENASTNodeIfStatement;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeIfStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   SkipToken(ltkIF);
   SkipToken(ltoOPENPAREN);
   result.Expression:=ParseExpression(true);
   SkipToken(ltoCLOSEPAREN);
   result.TrueStatement:=ParseStatement(true);
   if CurrentToken.TokenType=ltkELSE then begin
    SkipToken(ltkELSE);
    result.FalseStatement:=ParseStatement(true);
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseReturnStatement:TBESENASTNodeReturnStatement;
 begin
  result:=nil;
  try
   if not IsInFunction then begin
    raise EBESENSyntaxError.Create('Return outside a function');
   end;
   result:=TBESENASTNodeReturnStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   SkipToken(ltkRETURN);
   if not NextIsSemicolon then begin
    if CurrentToken.WasLineEnd then begin
     AddError('Illegal line terminator after return');
    end;
    result.Expression:=ParseExpression(true);
   end;
   ParseOptionalSemicolon;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseThrowStatement:TBESENASTNodeThrowStatement;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeThrowStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   SkipToken(ltkTHROW);
   if CurrentToken.WasLineEnd then begin
    AddError('Illegal line terminator after throw');
   end;
   result.Expression:=ParseExpression(true);
   ParseOptionalSemicolon;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseTryStatement:TBESENASTNodeTryStatement;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeTryStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   SkipToken(ltkTRY);

   result.CatchBlock:=nil;
   result.FinallyBlock:=nil;
   result.CatchIdentifier:=nil;

   result.TryBlock:=ParseBlockStatement;

   if (CurrentToken.TokenType<>ltkCATCH) and (CurrentToken.TokenType<>ltkFINALLY) then begin
    AddError('catch or finally expected after try');
   end;

   if CurrentToken.TokenType=ltkCATCH then begin
    SkipToken(ltkCATCH);
    SkipToken(ltoOPENPAREN);
    result.CatchIdentifier:=ParseIdentifier(not TBESEN(Instance).IsStrict);
    SkipToken(ltoCLOSEPAREN);
    result.CatchBlock:=ParseBlockStatement;
   end;

   if CurrentToken.TokenType=ltkFINALLY then begin
    SkipToken(ltkFINALLY);
    result.FinallyBlock:=ParseBlockStatement;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseSwitchStatement:TBESENASTNodeSwitchStatement;
 var CaseStatements:integer;
     CaseStatement:TBESENASTNodeCaseStatement;
     DefaultSeen:boolean;
     LabelSet:TBESENParserLabelSet;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeSwitchStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   SkipToken(ltkSWITCH);

   LabelSet:=LabelSetCurrent;
   LabelSet.Continuable:=true;
   LabelEnter('',result);

   result.Target:=LabelSet.Target;

   DefaultSeen:=false;

   SkipToken(ltoOPENPAREN);
   result.Expression:=ParseExpression(true);
   SkipToken(ltoCLOSEPAREN);

   CaseStatements:=0;
   SkipToken(ltoOPENBRACE);
   while (CurrentToken.TokenType<>ltoCLOSEBRACE) and not Lexer.IsEOF do begin
    if CaseStatements>=length(result.CaseStatements) then begin
     SetLength(result.CaseStatements,CaseStatements+256);
    end;
    result.CaseStatements[CaseStatements]:=TBESENASTNodeCaseStatement.Create(Instance);
    result.CaseStatements[CaseStatements].Location.LineNumber:=Lexer.LineNumber;
    CaseStatement:=result.CaseStatements[CaseStatements];
    inc(CaseStatements);

    CaseStatement.Expression:=nil;

    if CurrentToken.TokenType=ltkCASE then begin
     SkipToken(ltkCASE);
     CaseStatement.Expression:=ParseExpression(true);
     SkipToken(ltoCOLON);
    end else begin
     if DefaultSeen then begin
      AddError('duplication default clause in switch statement');
     end else begin
      DefaultSeen:=true;
      SkipToken(ltkDEFAULT);
      SkipToken(ltoCOLON);
     end;
    end;

    CaseStatement.Statements:=ParseStatementList(true);
   end;
   SetLength(result.CaseStatements,CaseStatements);

   SkipToken(ltoCLOSEBRACE);
   LabelLeave;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseVariableStatement:TBESENASTNodeVariableStatement;
 var Count:integer;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeVariableStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   SkipToken(ltkVAR);

   result.Declarations:=niL;
   Count:=1;
   SetLength(result.Declarations,Count);
   result.Declarations[0]:=ParseVariableDeclaration(true);
   while (CurrentToken.TokenType=ltoCOMMA) and not Lexer.IsEOF do begin
    SkipToken(ltoCOMMA);
    if Count>=length(result.Declarations) then begin
     SetLength(result.Declarations,Count+256);
    end;
    result.Declarations[Count]:=ParseVariableDeclaration(true);
    inc(Count);
   end;
   SetLength(result.Declarations,Count);

   ParseOptionalSemicolon;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseWhileStatement:TBESENASTNodeWhileStatement;
 var LabelSet:TBESENParserLabelSet;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeWhileStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   LabelSet:=LabelSetCurrent;
   LabelSet.Continuable:=true;
   LabelEnter('',result);
   result.Target:=LabelSet.Target;
   SkipToken(ltkWHILE);

   SkipToken(ltoOPENPAREN);
   result.Expression:=ParseExpression(true);
   SkipToken(ltoCLOSEPAREN);

   result.Statement:=ParseStatement(true);
   LabelLeave;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseWithStatement:TBESENASTNodeWithStatement;
 begin
  result:=nil;
  try
   if TBESEN(Instance).IsStrict then begin
    AddError('WITH isn''t allowed in strict mode');
   end;
   result:=TBESENASTNodeWithStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   SkipToken(ltkWITH);

   SkipToken(ltoOPENPAREN);
   result.Expression:=ParseExpression(true);
   SkipToken(ltoCLOSEPAREN);

   result.Statement:=ParseStatement(true);
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseExpressionStatement:TBESENASTNodeStatement;
 var Expression:TBESENASTNodeExpression;
     LabelSet,OldLabelSet:TBESENParserLabelSet;
     ln,LabelCount:integer;
     OldToken:TBESENLexerToken;
 begin
  ln:=Lexer.LineNumber;
  result:=nil;
  Expression:=nil;
  try
   Expression:=ParseExpression(true);
   if (Expression is TBESENASTNodeIdentifier) and (CurrentToken.TokenType=ltoCOLON) then begin
    SkipToken(ltoCOLON);

    result:=TBESENASTNodeLabelledStatement.Create(Instance);
    result.Location.LineNumber:=ln;

    SetLength(TBESENASTNodeLabelledStatement(result).Identifiers,1);
    TBESENASTNodeLabelledStatement(result).Identifiers[0]:=TBESENASTNodeIdentifier(Expression);

    Expression:=nil;

    OldLabelSet:=CurrentLabelSet;
    CurrentLabelSet:=nil;

    LabelSet:=LabelSetCurrent;
    LabelSet.Continuable:=true;

    LabelCount:=1;
    LabelEnter(TBESENASTNodeLabelledStatement(result).Identifiers[0].Name,result);
    while true do begin
     OldToken:=CurrentToken;
     if CurrentToken.TokenType=lttIDENTIFIER then begin
      Expression:=ParseIdentifier(true);
      if assigned(Expression) and (Expression is TBESENASTNodeIdentifier) and (CurrentToken.TokenType=ltoCOLON) then begin
       SkipToken(ltoCOLON);
       if LabelCount>=length(TBESENASTNodeLabelledStatement(result).Identifiers) then begin
        SetLength(TBESENASTNodeLabelledStatement(result).Identifiers,LabelCount+256);
       end;
       TBESENASTNodeLabelledStatement(result).Identifiers[LabelCount]:=TBESENASTNodeIdentifier(Expression);
       LabelEnter(TBESENASTNodeLabelledStatement(result).Identifiers[LabelCount].Name,result);
       inc(LabelCount);
      end else begin
       if assigned(Expression) then begin
        BESENFreeAndNil(Expression);
       end;
       Lexer.Restore(OldToken);
       NextToken;
       break;
      end;
     end else begin
      break;
     end;
    end;
    SetLength(TBESENASTNodeLabelledStatement(result).Identifiers,LabelCount);

    TBESENASTNodeLabelledStatement(result).Target:=LabelSet.Target;
    TBESENASTNodeLabelledStatement(result).Statement:=ParseStatement(not (CurrentToken.TokenType in [ltkFOR,ltkWHILE,ltkDO]));

    while LabelCount>0 do begin
     dec(LabelCount);
     LabelLeave;
    end;

    if assigned(CurrentLabelSet) then begin
     LabelSetList[CurrentLabelSet.Index]:=nil;
     BESENFreeAndNil(CurrentLabelSet);
    end;
    CurrentLabelSet:=OldLabelSet;

   end else begin
    ParseOptionalSemicolon;
    result:=TBESENASTNodeExpressionStatement.Create(Instance);
    result.Location.LineNumber:=ln;
    TBESENASTNodeExpressionStatement(result).Expression:=Expression;
    Expression:=nil;
   end;
  except
   BESENFreeAndNil(result);
   BESENFreeAndNil(Expression);
   raise;
  end;
 end;
 function ParseFunctionStatement:TBESENASTNodeStatement;
 var Expression:TBESENASTNodeAssignmentExpression;
 begin
  result:=nil;
  try
   result:=TBESENASTNodeExpressionStatement.Create(Instance);
   result.Location.LineNumber:=Lexer.LineNumber;
   TBESENASTNodeExpressionStatement(result).Expression:=TBESENASTNodeAssignmentExpression.Create(Instance);
   Expression:=TBESENASTNodeAssignmentExpression(TBESENASTNodeExpressionStatement(result).Expression);
   Expression.LeftExpression:=ParseIdentifier(not TBESEN(Instance).IsStrict);
   Expression.RightExpression:=ParseFunctionExpression(false,false);
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseStatement(ResetCurrentLabelSet:boolean):TBESENASTNodeStatement;
 var LineNumber:integer;
     OldToken:TBESENLexerToken;
 begin
  result:=nil;
  try
   if ResetCurrentLabelSet then begin
    CurrentLabelSet:=nil;
   end;
   LineNumber:=CurrentToken.LineNumber;
   case CurrentToken.TokenType of
    ltoSEMICOLON:begin
     SkipToken(ltoSEMICOLON);
     result:=TBESENASTNodeEmptyStatement.Create(Instance);
    end;
    ltoOPENBRACE:begin
     result:=ParseBlockStatement;
    end;
    ltkDEBUGGER:begin
     result:=ParseDebuggerStatement;
    end;
    ltkBREAK:begin
     result:=ParseBreakStatement;
    end;
    ltkCONTINUE:begin
     result:=ParseContinueStatement;
    end;
    ltkDO:begin
     result:=ParseDoStatement;
    end;
    ltkFOR:begin
     result:=ParseForStatement;
    end;
    ltkIF:begin
     result:=ParseIfStatement;
    end;
    ltkRETURN:begin
     result:=ParseReturnStatement;
    end;
    ltkTHROW:begin
     result:=ParseThrowStatement;
    end;
    ltkTRY:begin
     result:=ParseTryStatement;
    end;
    ltkSWITCH:begin
     result:=ParseSwitchStatement;
    end;
    ltkVAR:begin
     result:=ParseVariableStatement;
    end;
    ltkWHILE:begin
     result:=ParseWhileStatement;
    end;
    ltkWITH:begin
     result:=ParseWithStatement;
    end;
    else begin
     if CurrentToken.TokenType=ltkFUNCTION then begin
      OldToken:=CurrentToken;
      SkipToken(ltkFUNCTION);
      if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
       if CurrentToken.TokenType<>ltoOPENPAREN then begin
        result:=ParseFunctionStatement;
       end else begin
        Lexer.Restore(OldToken);
        NextToken;
        result:=ParseExpressionStatement;
       end;
      end else begin
       AddError('function keyword not allowed here');
       result:=nil;
      end;
     end else begin
      result:=ParseExpressionStatement;
     end;
    end;
   end;
   if assigned(result) then begin
    result.Location.LineNumber:=LineNumber;
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 function ParseSourceElement:TBESENASTNodeStatement;
 begin
  result:=nil;
  try
   if CurrentToken.TokenType=ltkFUNCTION then begin
    result:=ParseFunctionDeclaration;
   end else begin
    result:=ParseStatement(true);
   end;
  except
   BESENFreeAndNil(result);
   raise;
  end;
 end;
 procedure CheckJSON(Source:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENUTF8STRING{$endif});
 var Position:integer;
     CurrentChar:TBESENUTF32CHAR;
     CharEOF:boolean;
  procedure NextChar;
{$ifdef BESENSingleStringType}
  var Len:integer;
{$else}
  var b:byte;
{$endif}
  begin
   if (Position>=1) and (Position<=length(Source)) then begin
{$ifdef BESENSingleStringType}
{$ifdef BESENEmbarcaderoNextGen}
    CurrentChar:=Char.ConvertToUTF32(s,Position-1,Len);
    inc(Position,Len);
{$else}
    CurrentChar:=ord(Source[Position]);
    inc(Position);
    if (Position<=length(Source)) and ((SizeOf(Source[Position])=SizeOf(word)) and (((CurrentChar and $fc00)=$d800) and ((ord(Source[Position]) and $fc00)=$dc00))) then begin
     // UTF16
     CurrentChar:=(((CurrentChar and $3ff) shl 10) or (ord(Source[Position]) and $3ff))+$10000;
     inc(Position);
    end;
{$endif}
{$else}
    b:=byte(Source[Position]);
    if (b and $80)=0 then begin
     CurrentChar:=b;
     inc(Position);
    end else if ((Position+1)<=length(Source)) and ((b and $e0)=$c0) and ((byte(Source[Position+1]) and $c0)=$80) then begin
     CurrentChar:=((byte(Source[Position]) and $1f) shl 6) or (byte(Source[Position+1]) and $3f);
     inc(Position,2);
    end else if ((Position+2)<=length(Source)) and ((b and $f0)=$e0) and ((byte(Source[Position+1]) and $c0)=$80) and ((byte(Source[Position+2]) and $c0)=$80) then begin
     CurrentChar:=((byte(Source[Position]) and $0f) shl 12) or ((byte(Source[Position+1]) and $3f) shl 6) or (byte(Source[Position+2]) and $3f);
     inc(Position,3);
    end else if ((Position+3)<=length(Source)) and ((b and $f8)=$f0) and ((byte(Source[Position+1]) and $c0)=$80) and ((byte(Source[Position+2]) and $c0)=$80) and ((byte(Source[Position+3]) and $c0)=$80) then begin
     CurrentChar:=((byte(Source[Position]) and $07) shl 18) or ((byte(Source[Position+1]) and $3f) shl 12) or ((byte(Source[Position+2]) and $3f) shl 6) or (byte(Source[Position+3]) and $3f);
     inc(Position,4);
    end else if {$ifdef strictutf8}((TBESEN(Instance).Compatibility and COMPAT_UTF8_UNSAFE)<>0) and{$endif} ((Position+4)<=length(Source)) and ((b and $fc)=$f8) and ((byte(Source[Position+1]) and $c0)=$80) and ((byte(Source[Position+2]) and $c0)=$80) and ((byte(Source[Position+3]) and $c0)=$80) and ((byte(Source[Position+4]) and $c0)=$80) then begin
     CurrentChar:=((byte(Source[Position]) and $03) shl 24) or ((byte(Source[Position+1]) and $3f) shl 18) or ((byte(Source[Position+2]) and $3f) shl 12) or ((byte(Source[Position+3]) and $3f) shl 6) or (byte(Source[Position+4]) and $3f);
     inc(Position,5);
    end else if {$ifdef strictutf8}((TBESEN(Instance).Compatibility and COMPAT_UTF8_UNSAFE)<>0) and{$endif} ((Position+5)<=length(Source)) and ((b and $fe)=$fc) and ((byte(Source[Position+1]) and $c0)=$80) and ((byte(Source[Position+2]) and $c0)=$80) and ((byte(Source[Position+3]) and $c0)=$80) and ((byte(Source[Position+4]) and $c0)=$80) and ((byte(Source[Position+5]) and $c0)=$80) then begin
     CurrentChar:=((byte(Source[Position]) and $01) shl 30) or ((byte(Source[Position+1]) and $3f) shl 24) or ((byte(Source[Position+2]) and $3f) shl 18) or ((byte(Source[Position+3]) and $3f) shl 12) or ((byte(Source[Position+4]) and $3f) shl 6) or (byte(Source[Position+5]) and $3f);
     inc(Position,6);
    end else begin
     CurrentChar:=$fffd;
     inc(Position);
    end;
{$endif}
   end else begin
    CurrentChar:=0;
    CharEOF:=true;
   end;
  end;
  procedure JSONError;
  begin
   raise EBESENSyntaxError.Create('JSON.parse');
  end;
  procedure SkipWhite;
  begin
   while (not CharEOF) and ((CurrentChar=$0009) or (CurrentChar=$000a) or (CurrentChar=$000d) or (CurrentChar=$0020)) do begin
    NextChar;
   end;
  end;
  function IsChar(c:widechar):boolean;
  begin
   result:=(not CharEOF) and (CurrentChar=word(c));
  end;
  procedure ExpectChar(c:widechar);
  begin
   if IsChar(c) then begin
    NextChar;
   end else begin
    JSONError;
   end;
  end;
  procedure CheckValue;
   procedure CheckString;
   begin
    if IsChar('"') then begin
     NextChar;
     while not (CharEOF or IsChar('"')) do begin
      case CurrentChar of
       $0000..$001f:begin
        JSONError;
       end;
       ord('\'):begin
        NextChar;
        case CurrentChar of
         ord('"'),ord('\'),ord('/'),ord('b'),ord('f'),ord('n'),ord('r'),ord('t'):begin
          NextChar;
         end;
         ord('u'):begin
          NextChar;
          if (((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) or ((CurrentChar>=ord('a')) and (CurrentChar<=ord('f'))) or ((CurrentChar>=ord('A')) and (CurrentChar<=ord('F')))) and not CharEOF then begin
           NextChar;
           if (((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) or ((CurrentChar>=ord('a')) and (CurrentChar<=ord('f'))) or ((CurrentChar>=ord('A')) and (CurrentChar<=ord('F')))) and not CharEOF then begin
            NextChar;
            if (((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) or ((CurrentChar>=ord('a')) and (CurrentChar<=ord('f'))) or ((CurrentChar>=ord('A')) and (CurrentChar<=ord('F')))) and not CharEOF then begin
             NextChar;
             if (((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) or ((CurrentChar>=ord('a')) and (CurrentChar<=ord('f'))) or ((CurrentChar>=ord('A')) and (CurrentChar<=ord('F')))) and not CharEOF then begin
              NextChar;
             end else begin
              JSONError;
             end;
            end else begin
             JSONError;
            end;
           end else begin
            JSONError;
           end;
          end else begin
           JSONError;
          end;
         end;
         else begin
          JSONError;
         end;
        end;
       end;
       else begin
        NextChar;
       end;
      end;
     end;
     ExpectChar('"');
    end else begin
     JSONError;
    end;
    SkipWhite;
   end;
   procedure CheckNumber;
   begin
    if CharEOF then begin
     JSONError;
    end;
    case CurrentChar of
     ord('-'),ord('0')..ord('9'):begin
      if IsChar('-') then begin
       NextChar;
       if CharEOF or not ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) then begin
        JSONError;
       end;
      end;
      if (not CharEOF) and (CurrentChar=ord('0')) then begin
       NextChar;
       if (not CharEOF) and ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) then begin
        JSONError;
       end;
      end else begin
       while (not CharEOF) and ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) do begin
        NextChar;
       end;
      end;
      if IsChar('.') then begin
       NextChar;
       if CharEOF or not ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) then begin
        JSONError;
       end;
       while (not CharEOF) and ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) do begin
        NextChar;
       end;
      end;
      if (not CharEOF) and ((CurrentChar=ord('e')) or (CurrentChar=ord('E'))) then begin
       NextChar;
       if (not CharEOF) and ((CurrentChar=ord('-')) or (CurrentChar=ord('+'))) then begin
        NextChar;
       end;
       if CharEOF or not ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) then begin
        JSONError;
       end;
       while (not CharEOF) and ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) do begin
        NextChar;
       end;
      end;
     end else begin
      JSONError;
     end;
    end;
   end;
   procedure CheckObjectProperty;
   begin
    CheckString;
    SkipWhite;
    ExpectChar(':');
    SkipWhite;
    CheckValue;
   end;
   procedure CheckObject;
   begin
    ExpectChar('{');
    SkipWhite;
    while not (CharEOF or IsChar('}')) do begin
     CheckObjectProperty;
     SkipWhite;
     if IsChar(',') then begin
      NextChar;
      SkipWhite;
      if (not CharEOF) and IsChar('}') then begin
       JSONError;
      end;
     end;
    end;
    ExpectChar('}');
   end;
   procedure CheckArray;
   begin
    ExpectChar('[');
    SkipWhite;
    while not (CharEOF or IsChar(']')) do begin
     CheckValue;
     SkipWhite;
     if IsChar(',') then begin
      NextChar;
      SkipWhite;
      if (not CharEOF) and IsChar(']') then begin
       JSONError;
      end;
     end;
    end;
    ExpectChar(']');
   end;
   procedure CheckKeyword(const Keyword:TBESENString);
   var i:integer;
   begin
    for i:=1 to length(Keyword) do begin
     ExpectChar(Keyword[i]);
    end;
   end;
  begin
   SkipWhite;
   if CharEOF then begin
    JSONError;
   end;
   case CurrentChar of
    ord('{'):begin
     CheckObject;
    end;
    ord('['):begin
     CheckArray;
    end;
    ord('"'):begin
     CheckString;
    end;
    ord('-'),ord('0')..ord('9'):begin
     CheckNumber;
    end;
    ord('t'):begin
     CheckKeyword('true');
    end;
    ord('f'):begin
     CheckKeyword('false');
    end;
    ord('n'):begin
     CheckKeyword('null');
    end;
    else begin
     JSONError;
    end;
   end;
  end;
 begin
  CharEOF:=false;
  Position:=1;
  NextChar;
  CheckValue;
  SkipWhite;
  if not CharEOF then begin
   JSONError;
  end;
 end;
var Statement:TBESENASTNodeStatement;
    OldIsStrict:boolean;
    OldToken:TBesenLexerToken;
 procedure ParseJSONStatements;
 var i:integer;
 begin
  try
   SetLength(TBESENASTNodeProgram(result).Body.Statements,0);
   if not Lexer.IsEOF then begin
    Statement:=ParseExpressionStatement;
    if assigned(Statement) then begin
     SetLength(TBESENASTNodeProgram(result).Body.Statements,1);
     TBESENASTNodeProgram(result).Body.Statements[0]:=Statement;
    end else begin
     AddError('Fatal error');
    end;
   end;
  except
   for i:=0 to length(TBESENASTNodeProgram(result).Body.Statements)-1 do begin
    BesenFreeAndNil(TBESENASTNodeProgram(result).Body.Statements[i]);
   end;
   SetLength(TBESENASTNodeProgram(result).Body.Statements,0);
   raise;
  end;
 end;
 procedure ParseProgramStatements;
 var IsDirectivePrologue,FirstDirective:boolean;
     Statements,i:integer;
 begin
  Statements:=0;
  try
   IsDirectivePrologue:=true;
   FirstDirective:=true;
   UseStrictAlreadyParsed:=false;
   while not Lexer.IsEOF do begin
    if IsDirectivePrologue then begin
     ParseDirective(IsDirectivePrologue,FirstDirective,TBESENASTNodeProgram(result).Body);
    end;
    Statement:=ParseSourceElement;
    if assigned(Statement) then begin
     if Statements>=length(TBESENASTNodeProgram(result).Body.Statements) then begin
      SetLength(TBESENASTNodeProgram(result).Body.Statements,Statements+256);
     end;
     TBESENASTNodeProgram(result).Body.Statements[Statements]:=Statement;
     inc(Statements);
    end else begin
     AddError('Fatal error');
     break;
    end;
   end;
   SetLength(TBESENASTNodeProgram(result).Body.Statements,Statements);
  except
   for i:=0 to Statements-1 do begin
    BesenFreeAndNil(TBESENASTNodeProgram(result).Body.Statements[i]);
   end;
   SetLength(TBESENASTNodeProgram(result).Body.Statements,0);
   Statements:=0;
   raise;
  end;
 end;
begin
 Labels:=nil;
 LabelSets:=nil;
 CurrentLabelSet:=nil;
 LabelSetList:=TBESENPointerList.Create;
 LabelList:=TBESENPointerList.Create;
 OldIsStrict:=TBESEN(Instance).IsStrict;
 try
  NextToken;
  InForHeader:=false;
  if IsFunction then begin
   IsInFunction:=true;
   try
    result:=ParseFunctionExpression(true,false);
   except
    result:=nil;
    raise;
   end;
  end else if IsJSON then begin
   IsInFunction:=false;
   result:=TBESENASTNodeProgram.Create(Instance);
   try
    CheckJSON(Lexer.Source);
    TBESEN(Instance).IsStrict:=false;
    TBESENASTNodeProgram(result).Body.IsStrict:=TBESEN(Instance).IsStrict;
    ParseJSONStatements;
   except
    SetLength(TBESENASTNodeProgram(result).Body.Statements,0);
    raise;
   end;
  end else begin
   IsInFunction:=false;
   result:=TBESENASTNodeProgram.Create(Instance);
   try
    OldToken:=CurrentToken;
    TBESENASTNodeProgram(result).Body.IsStrict:=TBESEN(Instance).IsStrict;
    try
     ParseProgramStatements;
    except
     on e:EBESENUseStrict do begin
      TBESENASTNodeProgram(result).Body.IsStrict:=TBESEN(Instance).IsStrict;
      Lexer.Restore(OldToken);
      NextToken;
      ParseProgramStatements;
     end;
    end;
    TBESENASTNodeProgram(result).Body.IsStrict:=TBESEN(Instance).IsStrict;
   except
    SetLength(TBESENASTNodeProgram(result).Body.Statements,0);
    raise;
   end;
  end;
 finally
  CurrentToken.Name:='';
  CurrentToken.StringValue:='';
  CleanUpLabels;
  TBESEN(Instance).IsStrict:=OldIsStrict;
  BESENFreeAndNil(LabelSetList);
  BESENFreeAndNil(LabelList);
 end;
end;

end.
