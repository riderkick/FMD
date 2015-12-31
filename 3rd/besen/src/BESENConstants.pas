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
unit BESENConstants;
{$i BESEN.inc}

interface

uses Math;

{$ifndef BESENNoFPU}
const BESENFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];

      BESENFPUPrecisionMode:TFPUPrecisionMode={$ifdef HAS_TYPE_EXTENDED}pmEXTENDED{$else}pmDOUBLE{$endif};
{$endif}

{$ifndef fpc}
const FPC_VERSION=2;
      FPC_RELEASE=5;
      FPC_PATCH=1;
{$endif}

const COMPAT_UTF8_UNSAFE=1 shl 1; // accept 'valid but insecure' UTF8
      COMPAT_SGMLCOM=1 shl 2; // treat '<!--' as a '//' comment
      COMPAT_BESEN=1 shl 3; // BESEN-specific extensions
      COMPAT_JS=1 shl 4; // JavaScript-specific extensions

      BESEN_GC_MARKFACTOR:integer=25;
      BESEN_GC_TRIGGERCOUNT_PER_COLLECT:integer=1;

      bimMAXIDENTS=1 shl 24;

      BESENHashItemsPerBucketsThreshold:longword=5;
      BESENHashMaxSize:longword=1 shl 16;

      BESENMaxCountOfFreeCodeContexts:integer=16;

      BESENMaxCountOfFreeContexts:integer=16;

      BESEN_JIT_LOOPCOMPILETHRESHOLD:longword=1000;

      BESEN_CMWCRND_A=18782;
      BESEN_CMWCRND_M=longword($fffffffe);
      BESEN_CMWCRND_BITS=12;
      BESEN_CMWCRND_SIZE=1 shl BESEN_CMWCRND_BITS;
      BESEN_CMWCRND_MASK=BESEN_CMWCRND_SIZE-1;

      bncmmMINBLOCKCONTAINERSIZE=1048576;

      bcttNOTARGET=0;

      BESENEvalCacheSize:integer=256;
      BESENEvalCacheMaxSourceLength:integer=256;

      bncfZERO=1 shl 0;
      bncfNAN=1 shl 1;
      bncfINFINITE=1 shl 2;
      bncfNEGATIVE=1 shl 3;

      BESENObjectStructureIDManagerHashSize=65536;
      BESENObjectStructureIDManagerHashSizeMask=BESENObjectStructureIDManagerHashSize-1;

      BESENPolymorphicInlineCacheSize=8;
      BESENPolymorphicInlineCacheSizeMask=BESENPolymorphicInlineCacheSize-1;

      BESENPolymorphicInlineCacheStartItemPositions=longword((0 shl 0) or (1 shl 4) or (2 shl 8) or (3 shl 12) or (4 shl 16) or (5 shl 20) or (6 shl 24) or (7 shl 28));

      BESENLongBooleanValues:array[boolean] of longbool=(false,true);

      BESENNumberBooleanValues:array[boolean] of integer=(0,1);

      BESENInt32BooleanValues:array[boolean] of integer=(0,longint(longword($ffffffff)));

implementation

end.
