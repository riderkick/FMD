{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Version defines for ICS (Internet Component Suite)
Creation:     2003
Version:      1.09
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2006 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
May 31, 2004 V1.01 Correctly defined COMPILERX_UP symbols
Sep 07, 2004 V1.02 Updated for Delphi 9/W32
Mar 11, 2005 V1.03 Marco (marcov@stack.nl) added some defines for better FPC
                   support
Mar 13, 2005 V1.04 Changed conditional symbol for streams supporting 64 bits
                   seek.
Jul 30, 2005 V1.05 Updated for Delphi 10. Not tested !
Sept 2, 2005 V1.06 64-bit streams are Delphi 6 and later
Oct 05, 2008 V1.09 USE_LINE for COMPILER10_UP, removed UNICODE check for .NET.


  SYMBOL           Compiler version
  ===============  ======================================
  DELPHI1          Delphi 1
  DELPHI2          Delphi 2
  DELPHI3          Delphi 3
  DELPHI4          Delphi 4
  DELPHI5          Delphi 5
  DELPHI6          Delphi 6
  DELPHI7          Delphi 7
  DELPHI9          Delphi 2005
  DELPHI10         Delphi 2006
  DELPHI1_UP       Delphi 1  or higher
  DELPHI2_UP       Delphi 2  or higher
  DELPHI3_UP       Delphi 3  or higher
  DELPHI4_UP       Delphi 4  or higher
  DELPHI5_UP       Delphi 5  or higher
  DELPHI6_UP       Delphi 6  or higher
  DELPHI7_UP       Delphi 7  or higher  (Delphi 8 is .NET only. No way here)
  DELPHI9_UP       Delphi 9  or higher  (Delphi 2005 has W32 compiler)
  DELPHI10_UP      Delphi 10 or higher  (Delphi 2006)
  DELPHI11_UP      Delphi 11 or higher  (Delphi 2007 - Highlander)
  DELPHI12_UP      Delphi 12 or higher  (Tiburon)

  BCB1             C++ Builder 1
  BCB3             C++ Builder 3
  BCB4             C++ Builder 4
  BCB5             C++ Builder 5
  BCB6             C++ Builder 6
  BCB7             C++ Builder 7
  BCB1_UP          C++ Builder 1  or higher
  BCB3_UP          C++ Builder 3  or higher
  BCB4_UP          C++ Builder 4  or higher
  BCB5_UP          C++ Builder 5  or higher
  BCB6_UP          C++ Builder 6  or higher
  BCB7_UP          C++ Builder 7  or higher
  BCB10_UP         C++ Builder 10 or higher
  BCB11_UP         C++ Builder 11 or higher
  BCB12_UP         C++ Builder 12 or higher

  COMPILER1        Delphi 1
  COMPILER2        Delphi 2  or C++ Builder 1
  COMPILER3        Delphi 3
  COMPILER35       C++ Builder 3
  COMPILER4        Delphi 4  or C++ Builder 4
  COMPILER5        Delphi 5  or C++ Builder 5
  COMPILER6        Delphi 6  or C++ Builder 6
  COMPILER1_UP     Delphi 1  or higher
  COMPILER2_UP     Delphi 2  or C++ Builder 1 or higher
  COMPILER3_UP     Delphi 3  or higher
  COMPILER35_UP    C++ Builder 3 or higher
  COMPILER4_UP     Delphi 4  or C++ Builder 4 or higher
  COMPILER5_UP     Delphi 5  or C++ Builder 5 or higher
  COMPILER6_UP     Delphi 6  or C++ Builder 6 or higher
  COMPILER7_UP     Delphi 7  or higher
  COMPILER9_UP     Delphi 9  or higher
  COMPILER10_UP    Delphi 10 or C++ Builder 10 or higher
  COMPILER11_UP    Delphi or C++ Builder 2007 or higher (Highlander)
  COMPILER12_UP    Delphi or C++ Builder 12 or higher (Tiburon)

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ User defines : enable or disable these defines depending on how you want your 
  ICS Packages to be compiled }
  
{#$DEFINE USE_SSL}
{$DEFINE ZLIB_OBJ}

{ Dont not change anything below this line ! }

{$IFDEF FPC}
  {$MODE Delphi}                           { delphi mode            }
  {$H+}                                    { Use long strings       }
  {$ASMMODE Intel}                         { Intel syntax assembler }
  {$DEFINE USEWINDOWS}                     { Use "Windows" unit.    }
  {$DEFINE NOFORMS}                        { No unit forms.         }
  {$DEFINE SOCKETULONGMUSTMATCH}
  // With FPC, relying directly on Delphi version is evil.
  // (http://www.stack.nl/~marcov/porting.pdf)
  // However FPC 1.9.x is more D6/D7 than previous, so
  // we have to define something, sigh!
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}

  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE COMPILER12}
  {$IFDEF BCB}
    {$DEFINE BCB12}
  {$ELSE}
    {$DEFINE DELPHI12}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

(*
{$IFDEF VER190}   {Some preliminary Tiburon version use that symbol and .NET 2007}
  {$DEFINE COMPILER12}
  {$IFDEF BCB}
    {$DEFINE BCB12}
  {$ELSE}
    {$DEFINE DELPHI12}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}
*)

{$IFDEF VER185}
  {$DEFINE COMPILER11}
  {$IFDEF BCB}
    {$DEFINE BCB11}
  {$ELSE}
    {$DEFINE DELPHI11}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE COMPILER10}
  {$IFDEF BCB}
    {$DEFINE BCB10}
  {$ELSE}
    {$DEFINE DELPHI10}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE COMPILER9}
  {$IFDEF BCB}
    {$DEFINE BCB9}
  {$ELSE}
    {$DEFINE DELPHI9}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE COMPILER7}
  {$IFDEF BCB}
    {$DEFINE BCB7}
  {$ELSE}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE COMPILER6}
  {$IFDEF BCB}
    {$DEFINE BCB6}
  {$ELSE}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE COMPILER5}
  {$IFDEF BCB}
    {$DEFINE BCB5}
  {$ELSE}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE COMPILER4}
  {$DEFINE BCB4}
  {$DEFINE BCB}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE COMPILER4}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE COMPILER35}
  {$DEFINE BCB3}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE COMPILER3}
  {$DEFINE DELPHI3}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE COMPILER2}
  {$DEFINE BCB1}
  {$DEFINE BCB}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE COMPILER2}
  {$DEFINE DELPHI2}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER80}
  {$DEFINE COMPILER1}
  {$DEFINE DELPHI1}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF DELPHI12}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI11}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI10}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI9}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI7}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI6}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI5}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI4}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI3}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI2}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI1}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF COMPILER12}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER11}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER10}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER9}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER7}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER6}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER5}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER4}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER35}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER3}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER2}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER1}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF BCB12}
  {$DEFINE BCB12_UP}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB7_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB11}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB7_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB10}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB7_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB7}
  {$DEFINE BCB7_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB6}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB5}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB4}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB3}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB1}
  {$DEFINE BCB1_UP}
{$ENDIF}

{ TStream class support 64 bit seek }
{$IFNDEF FPC}             { Not for FPC !              }
  {$IFDEF COMPILER6_UP}   { OK for Delphi/BCB 6 and up }
    {$DEFINE STREAM64}
  {$ENDIF}
{$ENDIF}

{$IFNDEF DELPHI1}                          { Only Delphi1 lack windows unit }
  {$DEFINE USEWINDOWS}                     { Use "Windows" unit.            }
{$ENDIF}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

{$IFDEF DELPHI7_UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE VCL}
{$ENDIF}

{$IFDEF COMPILER10_UP}
    {$DEFINE USE_INLINE}
{$ENDIF}

{$IFDEF COMPILER12_UP}
  {$IFNDEF BCB}
    {$STRINGCHECKS OFF}
  {$ENDIF}
{$ENDIF}

{ ICS SSL code don't work on FPC }
{$IFDEF FPC}
  {$UNDEF USE_SSL}
{$ENDIF}

{ ICS SSL code don't work on .NET }
{$IFNDEF VCL}
  {$UNDEF USE_SSL}  
{$ENDIF}
