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
unit BESENObjectPropertyDescriptor;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENValue;

type TBESENObjectPropertyDescriptorAttribute=(bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE);

     TBESENObjectPropertyDescriptorAttributes=set of TBESENObjectPropertyDescriptorAttribute;

     TBESENObjectPropertyDescriptorPresent=(boppVALUE,boppGETTER,boppSETTER,boppWRITABLE,boppENUMERABLE,boppCONFIGURABLE,boppPROTO);

     TBESENObjectPropertyDescriptorPresents=set of TBESENObjectPropertyDescriptorPresent;

     PBESENObjectPropertyDescriptor=^TBESENObjectPropertyDescriptor;
     TBESENObjectPropertyDescriptor=record
      Value:TBESENValue;
      Getter:TObject;
      Setter:TObject;
      Attributes:TBESENObjectPropertyDescriptorAttributes;
      Presents:TBESENObjectPropertyDescriptorPresents;
     end;

     TBESENObjectPropertyDescriptors=array of TBESENObjectPropertyDescriptor;

var BESENUndefinedPropertyDescriptor:TBESENObjectPropertyDescriptor;

function BESENAccessorPropertyDescriptor(const Getter,Setter:TObject;const Attributes:TBESENObjectPropertyDescriptorAttributes):TBESENObjectPropertyDescriptor; {$ifdef caninline}inline;{$endif}
function BESENDataPropertyDescriptor(const Value:TBESENValue;const Attributes:TBESENObjectPropertyDescriptorAttributes):TBESENObjectPropertyDescriptor; {$ifdef caninline}inline;{$endif}
function BESENPropertyDescriptor(const Value:TBESENValue;const Getter,Setter:TObject;const Attributes:TBESENObjectPropertyDescriptorAttributes):TBESENObjectPropertyDescriptor; {$ifdef caninline}inline;{$endif}

function BESENIsUndefinedDescriptor(const Descriptor:TBESENObjectPropertyDescriptor):boolean; {$ifdef caninline}inline;{$endif}
function BESENIsAccessorDescriptor(const Descriptor:TBESENObjectPropertyDescriptor):boolean; {$ifdef caninline}inline;{$endif}
function BESENIsDataDescriptor(const Descriptor:TBESENObjectPropertyDescriptor):boolean; {$ifdef caninline}inline;{$endif}
function BESENIsGenericDescriptor(const Descriptor:TBESENObjectPropertyDescriptor):boolean; {$ifdef caninline}inline;{$endif}
function BESENIsInconsistentDescriptor(const Descriptor:TBESENObjectPropertyDescriptor):boolean; {$ifdef caninline}inline;{$endif}

implementation

uses BESEN;

function BESENAccessorPropertyDescriptor(const Getter,Setter:TObject;const Attributes:TBESENObjectPropertyDescriptorAttributes):TBESENObjectPropertyDescriptor; {$ifdef caninline}inline;{$endif}
begin
 result.Value.ValueType:=bvtUNDEFINED;
 result.Getter:=Getter;
 result.Setter:=Setter;
 result.Attributes:=Attributes*[bopaENUMERABLE,bopaCONFIGURABLE];
 result.Presents:=[boppENUMERABLE,boppCONFIGURABLE];
 if assigned(result.Getter) then begin
  result.Presents:=result.Presents+[boppGETTER];
 end;
 if assigned(result.Setter) then begin
  result.Presents:=result.Presents+[boppSETTER];
 end;
end;

function BESENDataPropertyDescriptor(const Value:TBESENValue;const Attributes:TBESENObjectPropertyDescriptorAttributes):TBESENObjectPropertyDescriptor; {$ifdef caninline}inline;{$endif}
begin
 BESENCopyValue(result.Value,Value);
 result.Getter:=nil;
 result.Setter:=nil;
 result.Attributes:=Attributes;
 result.Presents:=[boppVALUE,boppWRITABLE,boppENUMERABLE,boppCONFIGURABLE];
end;

function BESENPropertyDescriptor(const Value:TBESENValue;const Getter,Setter:TObject;const Attributes:TBESENObjectPropertyDescriptorAttributes):TBESENObjectPropertyDescriptor; {$ifdef caninline}inline;{$endif}
begin
 BESENCopyValue(result.Value,Value);
 result.Getter:=Getter;
 result.Setter:=Setter;
 result.Attributes:=Attributes;
 result.Presents:=[boppVALUE,boppGETTER,boppSETTER,boppWRITABLE,boppENUMERABLE,boppCONFIGURABLE];
end;

function BESENIsUndefinedDescriptor(const Descriptor:TBESENObjectPropertyDescriptor):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=Descriptor.Presents=[];
end;

function BESENIsAccessorDescriptor(const Descriptor:TBESENObjectPropertyDescriptor):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=([boppGETTER,boppSETTER]*Descriptor.Presents)<>[];
end;

function BESENIsDataDescriptor(const Descriptor:TBESENObjectPropertyDescriptor):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=([boppVALUE,boppWRITABLE]*Descriptor.Presents)<>[];
end;

function BESENIsGenericDescriptor(const Descriptor:TBESENObjectPropertyDescriptor):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=not (BESENIsAccessorDescriptor(Descriptor) or BESENIsDataDescriptor(Descriptor));
end;

function BESENIsInconsistentDescriptor(const Descriptor:TBESENObjectPropertyDescriptor):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(([boppGETTER,boppSETTER]*Descriptor.Presents)<>[]) and (([boppVALUE,boppWRITABLE]*Descriptor.Presents)<>[]);
end;

procedure InitBESEN;
begin
 fillchar(BESENUndefinedPropertyDescriptor,sizeof(TBESENObjectPropertyDescriptor),#0);
 BESENUndefinedPropertyDescriptor.Presents:=[];
end;

procedure DoneBESEN;
begin
end;

initialization
 InitBESEN;
finalization
 DoneBESEN;
end.

