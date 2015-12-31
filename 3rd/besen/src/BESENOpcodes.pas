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
unit BESENOpcodes;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes;

const bopSTOP=0;
      bopNEW=1;
      bopCALL=2;
      bopEND=3;
      bopVREF=4;
      bopLREF=5;
      bopNOP=6;
      bopCOPY=7;
      bopNEQ=8;
      bopNSEQ=9;
      bopAREF=10;
      bopTHROW=11;
      bopSETC=12;
      bopGETC=13;
      bopTHIS=14;
      bopOBJECT=15;
      bopARRAY=16;
      bopREGEXP=17;
      bopREF=18;
      bopGETVALUE=19;
      bopLOOKUP=20;
      bopPUTVALUE=21;
      bopDELETE=22;
      bopTYPEOF=23;
      bopTOOBJECT=24;
      bopTONUMBER=25;
      bopTOBOOLEAN=26;
      bopTOSTRING=27;
      bopTOPRIMITIVE=28;
      bopNEG=29;
      bopINV=30;
      bopNOT=31;
      bopMUL=32;
      bopDIV=33;
      bopMOD=34;
      bopADD=35;
      bopADDNUM=36;
      bopSUB=37;
      bopSHL=38;
      bopSHR=39;
      bopUSHR=40;
      bopLT=41;
      bopGT=42;
      bopLE=43;
      bopGE=44;
      bopINSTANCEOF=45;
      bopIN=46;
      bopEQ=47;
      bopSEQ=48;
      bopBAND=49;
      bopBXOR=50;
      bopBOR=51;
      bopSENUM=52;
      bopSWITH=53;
      bopSCATCH=54;
      bopENDF=55;
      bopJMP=56;
      bopJZ=57;
      bopJNZ=58;
      bopJNULL=59;
      bopLOOPENUM=60;
      bopSTRYC=61;
      bopSTRYF=62;
      bopLITERAL=63;
      bopLITERALUNDEF=64;
      bopLITERALNULL=65;
      bopLITERALBOOL=66;
      bopLITERALNUM=67;
      bopLITERALSTR=68;
      bopLITERALOBJ=69;
      bopFUNC=70;
      bopLINE=71;
      bopGC=72;
      bopSTRICT=73;
      bopSTRICTCHECKREF=74;
      bopDEBUGGER=75;
      bopCHECKOBJECTCOERCIBLE=76;
      bopPUTOBJVALUE=77;
      bopPUTOBJGET=78;
      bopPUTOBJSET=79;
      bopINC=80;
      bopDEC=81;
      bopCOPYBOOL=82;
      bopCOPYNUM=83;
      bopCOPYSTR=84;
      bopCOPYOBJ=85;
      bopCOPYREF=86;
      bopCOPYLOCAL=87;
      bopGETVALUEREF=88;
      bopPUTVALUEREF=89;
      bopGETVALUELOCAL=90;
      bopPUTVALUELOCAL=91;
      bopGETVALUELOCALFAST=92;
      bopPUTVALUELOCALFAST=93;
      bopGETVALUELOCALBOOL=94;
      bopPUTVALUELOCALBOOL=95;
      bopGETVALUELOCALNUM=96;
      bopPUTVALUELOCALNUM=97;
      bopGETVALUELOCALSTR=98;
      bopPUTVALUELOCALSTR=99;
      bopGETVALUELOCALOBJ=100;
      bopPUTVALUELOCALOBJ=101;
      bopGETVALUELOCALINDEX=102;
      bopPUTVALUELOCALINDEX=103;
      bopGETVALUELOCALINDEXBOOL=104;
      bopPUTVALUELOCALINDEXBOOL=105;
      bopGETVALUELOCALINDEXNUM=106;
      bopPUTVALUELOCALINDEXNUM=107;
      bopGETVALUELOCALINDEXSTR=108;
      bopPUTVALUELOCALINDEXSTR=109;
      bopGETVALUELOCALINDEXOBJ=110;
      bopPUTVALUELOCALINDEXOBJ=111;
      bopLOOPINITCOUNT=112;
      bopLOOPADDCOUNT=113;
      bopTRACE=114;
      bopLTBOOL=115;
      bopGTBOOL=116;
      bopLEBOOL=117;
      bopGEBOOL=118;
      bopEQBOOL=119;
      bopNEQBOOL=120;
      bopLTNUM=121;
      bopGTNUM=122;
      bopLENUM=123;
      bopGENUM=124;
      bopEQNUM=125;
      bopNEQNUM=126;
      bopLTSTR=127;
      bopGTSTR=128;
      bopLESTR=129;
      bopGESTR=130;
      bopEQSTR=131;
      bopNEQSTR=132;
      bopSHLBOOl=133;
      bopSHRBOOL=134;
      bopBANDBOOL=135;
      bopBXORBOOL=136;
      bopBORBOOL=137;
      bopSHLNUM=138;
      bopSHRNUM=139;
      bopUSHRNUM=140;
      bopBANDNUM=141;
      bopBXORNUM=142;
      bopBORNUM=143;
      bopSETCUNDEF=144;
      bopSETCNULL=145;
      bopSETCBOOL=146;
      bopSETCNUM=147;
      bopSETCSTR=148;
      bopSETCOBJ=149;
      bopTRACENEW=150;
      bopTRACECALL=151;
      bopLTNUMCONST=152;
      bopGTNUMCONST=153;
      bopLENUMCONST=154;
      bopGENUMCONST=155;
      bopEQNUMCONST=156;
      bopNEQNUMCONST=157;
      bopJZERO=158;
      bopJNZERO=159;

      bopFIRST=0;
      bopLAST=159;

      OpcodeNames:array[bopFIRST..bopLAST] of TBESENString=('STOP',
                                                            'NEW',
                                                            'CALL',
                                                            'END',
                                                            'VREF',
                                                            'LREF',
                                                            'NOP',
                                                            'COPY',
                                                            'NEQ',
                                                            'NSEQ',
                                                            'AREF',
                                                            'THROW',
                                                            'SETC',
                                                            'GETC',
                                                            'THIS',
                                                            'OBJECT',
                                                            'ARRAY',
                                                            'REGEXP',
                                                            'REF',
                                                            'GETVALUE',
                                                            'LOOKUP',
                                                            'PUTVALUE',
                                                            'DELETE',
                                                            'TYPEOF',
                                                            'TOOBJECT',
                                                            'TONUMBER',
                                                            'TOBOOLEAN',
                                                            'TOSTRING',
                                                            'TOPRIMITIVE',
                                                            'NEG',
                                                            'INV',
                                                            'NOT',
                                                            'MUL',
                                                            'DIV',
                                                            'MOD',
                                                            'ADD',
                                                            'ADDNUM',
                                                            'SUB',
                                                            'SHL',
                                                            'SHR',
                                                            'USHR',
                                                            'LT',
                                                            'GT',
                                                            'LE',
                                                            'GE',
                                                            'INSTANCEOF',
                                                            'IN',
                                                            'EQ',
                                                            'SEQ',
                                                            'BAND',
                                                            'BXOR',
                                                            'BOR',
                                                            'SENUM',
                                                            'SWITH',
                                                            'SCATCH',
                                                            'ENDF',
                                                            'JMP',
                                                            'JZ',
                                                            'JNZ',
                                                            'JNULL',
                                                            'LOOPENUM',
                                                            'STRYC',
                                                            'STRYF',
                                                            'LITERAL',
                                                            'LITERALUNDEF',
                                                            'LITERALNULL',
                                                            'LITERALBOOL',
                                                            'LITERALNUM',
                                                            'LITERALSTR',
                                                            'LITERALOBJ',
                                                            'FUNC',
                                                            'LINE',
                                                            'GC',
                                                            'STRICT',
                                                            'STRICTCHECKREF',
                                                            'DEBUGGER',
                                                            'CHECKOBJECTCOERCIBLE',
                                                            'PUTOBJVALUE',
                                                            'PUTOBJGET',
                                                            'PUTOBJSET',
                                                            'INC',
                                                            'DEC',
                                                            'COPYBOOL',
                                                            'COPYNUM',
                                                            'COPYSTR',
                                                            'COPYOBJ',
                                                            'COPYREF',
                                                            'COPYLOCAL',
                                                            'GETVALUEREF',
                                                            'PUTVALUEREF',
                                                            'GETVALUELOCAL',
                                                            'PUTVALUELOCAL',
                                                            'GETVALUELOCALFAST',
                                                            'PUTVALUELOCALFAST',
                                                            'GETVALUELOCALBOOL',
                                                            'PUTVALUELOCALBOOL',
                                                            'GETVALUELOCALNUM',
                                                            'PUTVALUELOCALNUM',
                                                            'GETVALUELOCALSTR',
                                                            'PUTVALUELOCALSTR',
                                                            'GETVALUELOCALOBJ',
                                                            'PUTVALUELOCALOBJ',
                                                            'GETVALUELOCALINDEX',
                                                            'PUTVALUELOCALINDEX',
                                                            'GETVALUELOCALINDEXBOOL',
                                                            'PUTVALUELOCALINDEXBOOL',
                                                            'GETVALUELOCALINDEXNUM',
                                                            'PUTVALUELOCALINDEXNUM',
                                                            'GETVALUELOCALINDEXSTR',
                                                            'PUTVALUELOCALINDEXSTR',
                                                            'GETVALUELOCALINDEXOBJ',
                                                            'PUTVALUELOCALINDEXOBJ',
                                                            'LOOPINITCOUNT',
                                                            'LOOPADDCOUNT',
                                                            'TRACE',
                                                            'LTBOOL',
                                                            'GTBOOL',
                                                            'LEBOOL',
                                                            'GEBOOL',
                                                            'EQBOOL',
                                                            'NEQBOOL',
                                                            'LTNUM',
                                                            'GTNUM',
                                                            'LENUM',
                                                            'GENUM',
                                                            'EQNUM',
                                                            'NEQNUM',
                                                            'LTSTR',
                                                            'GTSTR',
                                                            'LESTR',
                                                            'GESTR',
                                                            'EQSTR',
                                                            'NEQSTR',
                                                            'SHLBOOl',
                                                            'SHRBOOL',
                                                            'BANDBOOL',
                                                            'BXORBOOL',
                                                            'BORBOOL',
                                                            'SHLNUM',
                                                            'SHRNUM',
                                                            'USHRNUM',
                                                            'BANDNUM',
                                                            'BXORNUM',
                                                            'BORNUM',
                                                            'SETCUNDEF',
                                                            'SETCNULL',
                                                            'SETCBOOL',
                                                            'SETCNUM',
                                                            'SETCSTR',
                                                            'SETCOBJ',
                                                            'TRACENEW',
                                                            'TRACECALL',
                                                            'LTNUMCONST',
                                                            'GTNUMCONST',
                                                            'LENUMCONST',
                                                            'GENUMCONST',
                                                            'EQNUMCONST',
                                                            'NEQNUMCONST',
                                                            'JZERO',
                                                            'JNZERO');

implementation

end.
