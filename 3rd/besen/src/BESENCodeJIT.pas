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
unit BESENCodeJIT;
{$i BESEN.inc}

interface

uses {$ifdef windows}Windows,MMSystem,{$endif}{$ifdef unix}dl,BaseUnix,Unix,
     UnixType,{$endif}BESENConstants,BESENTypes;

function BESENGenerateNativeCode(ACodeContext:TObject):TBESENBoolean;
function BESENExecuteNativeCode(ACodeContext:TObject):TBESENBoolean; {$ifdef UseRegister}register;{$endif}

implementation

{

Base JIT design concept:

  1. It is mostly a damn simple method-based native-code-template-concatenation JIT mostly without any
     fancy optimizations just as CSE, SSA, and so on.

  2. The native code execution can be continued from any correspondent byte code instruction, so it
     is possible to jump to any byte-code-instruction-mapped native-code-sub-block from any each
     other in the same code context and in the same code instande.

  3. The from the JIT generated native code can call the correspondent byte code instruction dispatcher
     function from the byte code interpreter at every time, if it needed, for example, for complex byte
     code instructions which are more trouble than it's worth to compile these to native code.

  4. The virtual byte code interpreter VM registers are used instead the native platform target CPU
     registers, for to keep the fallback-to-byte-code-instruction-dispatcher-function-stuff simple just
     much as possible.

  5. The from the JIT generated native code can be self-modifying, for example at the monomorphic and
     polymorphic inline caching based byte code instructions, by modifying some index constant values
     at some native code instruction opcode arguments. But this is optional, since the byte code itself
     is also self-modifying in this monomorphic and polymorphic inline caching context. It's mostly a
     trade-off, which way performs better on which target platform CPU architecture.

  6. A byte-code-instruction-offset <-> native-code-instruction-offset map must be generated and used
     for the jump-in-to-random-code-position-usage and for the effective use of the fallback-to-byte-
     code-instruction-dispatcher-function-stuff.

}

uses BESEN,BESENValue,BESENASTNodes,BESENCode,BESENCodeContext,BESENContext,BESENOpcodes,
     BESENGarbageCollector,BESENNumberUtils,BESENLexicalEnvironment,
     BESENDeclarativeEnvironmentRecord,BESENNativeCodeMemoryManager;

function BESENGenerateNativeCode(ACodeContext:TObject):TBESENBoolean;
begin
 result:=false;
end;

function BESENExecuteNativeCode(ACodeContext:TObject):TBESENBoolean; {$ifdef UseRegister}register;{$endif}
begin
 result:=false;
end;

end.
