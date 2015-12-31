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
unit BESENCollectorObject;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENBaseObject;

type TBESENCollectorObject=class(TBESENBaseObject)
      public
       CollectorPrevious,CollectorNext:TBESENCollectorObject;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

implementation

uses BESEN;

constructor TBESENCollectorObject.Create(AInstance:TObject);
begin
 inherited Create;
 Instance:=AInstance;
 CollectorPrevious:=TBESEN(Instance).Collector.Last;
 CollectorNext:=nil;
 if assigned(CollectorPrevious) then begin
  CollectorPrevious.CollectorNext:=self;
 end else begin
  TBESEN(Instance).Collector.First:=self;
 end;
 TBESEN(Instance).Collector.Last:=self;
end;

destructor TBESENCollectorObject.Destroy;
begin
 if assigned(CollectorPrevious) then begin
  CollectorPrevious.CollectorNext:=CollectorNext;
 end else if TBESEN(Instance).Collector.First=self then begin
  TBESEN(Instance).Collector.First:=CollectorNext;
 end;
 if assigned(CollectorNext) then begin
  CollectorNext.CollectorPrevious:=CollectorPrevious;
 end else if TBESEN(Instance).Collector.Last=self then begin
  TBESEN(Instance).Collector.Last:=CollectorPrevious;
 end;
 CollectorNext:=nil;
 CollectorPrevious:=nil;
 inherited Destroy;
end;

end.
