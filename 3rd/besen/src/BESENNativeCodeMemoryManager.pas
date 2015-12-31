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
unit BESENNativeCodeMemoryManager;
{$i BESEN.inc}

interface

uses {$ifdef windows}Windows,MMSystem,{$endif}{$ifdef unix}dl,BaseUnix,Unix,UnixType,{$endif}
     SysUtils,Classes,BESENConstants,BESENTypes;

type PBESENNativeCodeMemoryManagerBlock=^TBESENNativeCodeMemoryManagerBlock;
     TBESENNativeCodeMemoryManagerBlock=packed record
      Signature:ptruint;
      Previous:PBESENNativeCodeMemoryManagerBlock;
      Next:PBESENNativeCodeMemoryManagerBlock;
      Size:ptruint;
     end;

     PBESENNativeCodeMemoryManagerBlockContainer=^TBESENNativeCodeMemoryManagerBlockContainer;
     TBESENNativeCodeMemoryManagerBlockContainer=record
      Previous:PBESENNativeCodeMemoryManagerBlockContainer;
      Next:PBESENNativeCodeMemoryManagerBlockContainer;
      Base:pointer;
      Size:ptruint;
      Used:ptruint;
      First:PBESENNativeCodeMemoryManagerBlock;
      Last:PBESENNativeCodeMemoryManagerBlock;
     end;

     TBESENNativeCodeMemoryManager=class
      private
       PageSize:ptruint;
       Alignment:ptruint;
       function AllocateBlockContainer(BlockContainerSize:ptruint):PBESENNativeCodeMemoryManagerBlockContainer;
       procedure FreeBlockContainer(BlockContainer:PBESENNativeCodeMemoryManagerBlockContainer);
      public
       First,Last:PBESENNativeCodeMemoryManagerBlockContainer;
       constructor Create;
       destructor Destroy; override;
       function GetMemory(Size:ptruint):pointer;
       procedure FreeMemory(p:pointer);
       function ReallocMemory(p:pointer;Size:ptruint):pointer;
     end;

{$ifdef HasJIT}
{$ifdef unix}
var fpmprotect:function(__addr:pointer;__len:cardinal;__prot:longint):longint; cdecl;// external 'c' name 'mprotect';
{$endif}
{$endif}

implementation

uses BESENUtils;

const bncmmMemoryBlockSignature:ptruint={$ifdef cpu64}$1337bab3deadc0d3{$else}$deadc0d3{$endif};

constructor TBESENNativeCodeMemoryManager.Create;
{$ifdef windows}
var SystemInfo:TSystemInfo;
{$else}
{$ifdef unix}
{$endif}
{$endif}
begin
 inherited Create;
{$ifdef windows}
 GetSystemInfo(SystemInfo);
 PageSize:=BESENRoundUpToPowerOfTwo(SystemInfo.dwPageSize);
{$else}
{$ifdef unix}
 PageSize:=4096;
{$else}
 PageSize:=4096;
{$endif}
{$endif}
{$ifdef cpu386}
 Alignment:=16;
{$else}
{$ifdef cpuamd64}
 Alignment:=16;
{$else}
{$ifdef cpuarm}
 Alignment:=16;
{$else}
 Alignment:=PageSize;
{$endif}
{$endif}
{$endif}
 First:=nil;
 Last:=nil;
end;

destructor TBESENNativeCodeMemoryManager.Destroy;
begin
 while assigned(First) do begin
  FreeBlockContainer(First);
 end;
 inherited Destroy;
end;

function TBESENNativeCodeMemoryManager.AllocateBlockContainer(BlockContainerSize:ptruint):PBESENNativeCodeMemoryManagerBlockContainer;
var Size:ptruint;
    Block:PBESENNativeCodeMemoryManagerBlock;
begin
 if BlockContainerSize>0 then begin
  Size:=BESENRoundUpToMask(BlockContainerSize,PageSize);
  New(result);
{$ifdef windows}
  result^.Base:=VirtualAlloc(nil,Size,MEM_COMMIT,PAGE_EXECUTE_READWRITE);
{$else}
{$ifdef unix}
  result^.Base:=fpmmap(nil,Size,PROT_READ or PROT_WRITE or PROT_EXEC,MAP_PRIVATE or MAP_ANONYMOUS,-1,0);
{$else}
  GetMem(result^.Base,Size);
{$endif}
{$endif}
  result^.Size:=Size;
  result^.Used:=sizeof(TBESENNativeCodeMemoryManagerBlock)*2;
  if assigned(Last) then begin
   Last^.Next:=result;
   result^.Previous:=Last;
   Last:=result;
   result^.Next:=nil;
  end else begin
   First:=result;
   Last:=result;
   result^.Previous:=nil;
   result^.Next:=nil;
  end;
  FillChar(result^.Base^,result^.Size,#0);
  result^.First:=result^.Base;
  result^.Last:=pointer(@PBESENByteArray(result^.Base)[result^.Size-sizeof(TBESENNativeCodeMemoryManagerBlock)]);
  Block:=result^.First;
  Block^.Signature:=bncmmMemoryBlockSignature;
  Block^.Previous:=nil;
  Block^.Next:=result^.Last;
  Block^.Size:=0;
  Block:=result^.Last;
  Block^.Signature:=bncmmMemoryBlockSignature;
  Block^.Previous:=result^.First;
  Block^.Next:=nil;
  Block^.Size:=0;
 end else begin
  result:=nil;
 end;
end;

procedure TBESENNativeCodeMemoryManager.FreeBlockContainer(BlockContainer:PBESENNativeCodeMemoryManagerBlockContainer);
begin
 if assigned(BlockContainer^.Previous) then begin
  BlockContainer^.Previous^.Next:=BlockContainer^.Next;
 end else begin
  First:=BlockContainer^.Next;
 end;
 if assigned(BlockContainer^.Next) then begin
  BlockContainer^.Next^.Previous:=BlockContainer^.Previous;
 end else begin
  Last:=BlockContainer^.Previous;
 end;
{$ifdef windows}
 VirtualFree(BlockContainer^.Base,0,MEM_RELEASE);
{$else}
{$ifdef unix}
 fpmunmap(BlockContainer^.Base,BlockContainer^.Size);
{$else}
 FreeMem(BlockContainer^.Base);
{$endif}
{$endif}
 Dispose(BlockContainer);
end;

function TBESENNativeCodeMemoryManager.GetMemory(Size:ptruint):pointer;
var BlockContainer:PBESENNativeCodeMemoryManagerBlockContainer;
    CurrentBlock,NewBlock:PBESENNativeCodeMemoryManagerBlock;
    DestSize,BlockContainerSize:ptruint;
begin
 result:=nil;
 if Size>0 then begin
  DestSize:=Size+sizeof(TBESENNativeCodeMemoryManagerBlock);
  BlockContainer:=First;
  while true do begin
   while assigned(BlockContainer) do begin
    if (BlockContainer^.Used+DestSize)<=BlockContainer^.Size then begin
     CurrentBlock:=BlockContainer^.First;
     while assigned(CurrentBlock) and (CurrentBlock^.Signature=bncmmMemoryBlockSignature) and assigned(CurrentBlock^.Next) do begin
      NewBlock:=pointer(ptruint(BESENRoundUpToMask(ptruint(pointer(@PBESENByteArray(CurrentBlock)[(sizeof(TBESENNativeCodeMemoryManagerBlock)*2)+CurrentBlock^.Size])),Alignment)-sizeof(TBESENNativeCodeMemoryManagerBlock)));
      if (ptruint(CurrentBlock^.Next)-ptruint(NewBlock))>=DestSize then begin
       NewBlock^.Signature:=bncmmMemoryBlockSignature;
       NewBlock^.Previous:=CurrentBlock;
       NewBlock^.Next:=CurrentBlock^.Next;
       NewBlock^.Size:=Size;
       CurrentBlock^.Next^.Previous:=NewBlock;
       CurrentBlock^.Next:=NewBlock;
       result:=pointer(@PBESENByteArray(NewBlock)[sizeof(TBESENNativeCodeMemoryManagerBlock)]);
       inc(BlockContainer^.Used,DestSize);
       exit;
      end else begin
       CurrentBlock:=CurrentBlock^.Next;
      end;
     end;
    end;
    BlockContainer:=BlockContainer^.Next;
   end;
   if DestSize<=bncmmMINBLOCKCONTAINERSIZE then begin
    BlockContainerSize:=bncmmMINBLOCKCONTAINERSIZE;
   end else begin
    BlockContainerSize:=BESENRoundUpToPowerOfTwo(DestSize);
   end;
   BlockContainer:=AllocateBlockContainer(BlockContainerSize);
   if not assigned(BlockContainer) then begin
    break;
   end;
  end;
 end;
end;

procedure TBESENNativeCodeMemoryManager.FreeMemory(p:pointer);
var BlockContainer:PBESENNativeCodeMemoryManagerBlockContainer;
    CurrentBlock:PBESENNativeCodeMemoryManagerBlock;
begin
 BlockContainer:=First;
 while assigned(BlockContainer) do begin
  if ((ptruint(BlockContainer^.Base)+sizeof(TBESENNativeCodeMemoryManagerBlock))<=ptruint(p)) and ((ptruint(p)+sizeof(TBESENNativeCodeMemoryManagerBlock))<(ptruint(BlockContainer^.Base)+BlockContainer^.Size)) then begin
   CurrentBlock:=pointer(ptruint(ptruint(p)-sizeof(TBESENNativeCodeMemoryManagerBlock)));
   if (CurrentBlock^.Signature=bncmmMemoryBlockSignature) and (CurrentBlock<>BlockContainer^.First) and (CurrentBlock<>BlockContainer^.Last) then begin
    dec(BlockContainer^.Used,CurrentBlock^.Size+sizeof(TBESENNativeCodeMemoryManagerBlock));
    CurrentBlock^.Signature:=0;
    CurrentBlock^.Previous^.Next:=CurrentBlock^.Next;
    CurrentBlock^.Next^.Previous:=CurrentBlock^.Previous;
    if (assigned(BlockContainer^.First) and (BlockContainer^.First^.Next=BlockContainer^.Last)) or not assigned(BlockContainer^.First) then begin
     FreeBlockContainer(BlockContainer);
    end;
    exit;
   end;
  end;
  BlockContainer:=BlockContainer^.Next;
 end;
end;

function TBESENNativeCodeMemoryManager.ReallocMemory(p:pointer;Size:ptruint):pointer;
var BlockContainer:PBESENNativeCodeMemoryManagerBlockContainer;
    CurrentBlock:PBESENNativeCodeMemoryManagerBlock;
    DestSize:ptruint;
begin
 result:=nil;
 if assigned(p) then begin
  if Size=0 then begin
   FreeMemory(p);
  end else begin
   DestSize:=Size+sizeof(TBESENNativeCodeMemoryManagerBlock);
   BlockContainer:=First;
   while assigned(BlockContainer) do begin
    if ((ptruint(BlockContainer^.Base)+sizeof(TBESENNativeCodeMemoryManagerBlock))<=ptruint(p)) and ((ptruint(p)+sizeof(TBESENNativeCodeMemoryManagerBlock))<(ptruint(BlockContainer^.Base)+BlockContainer^.Size)) then begin
     CurrentBlock:=pointer(ptruint(ptruint(p)-sizeof(TBESENNativeCodeMemoryManagerBlock)));
     if (CurrentBlock^.Signature=bncmmMemoryBlockSignature) and (CurrentBlock<>BlockContainer^.First) and (CurrentBlock<>BlockContainer^.Last) then begin
      if (ptruint(CurrentBlock^.Next)-ptruint(CurrentBlock))>=DestSize then begin
       CurrentBlock^.Size:=Size;
       result:=p;
       exit;
      end else begin
       result:=GetMemory(Size);
       if assigned(result) then begin
        if CurrentBlock^.Size<Size then begin
         Move(p^,result^,CurrentBlock^.Size);
        end else begin
         Move(p^,result^,Size);
        end;
       end;
       FreeMemory(p);
       exit;
      end;
     end;
    end;
    BlockContainer:=BlockContainer^.Next;
   end;
  end;
  FreeMemory(p);
 end else if Size<>0 then begin
  result:=GetMemory(Size);
 end;
end;

procedure InitBESEN;
begin
{$ifdef HasJIT}
{$ifdef unix}
{$ifdef darwin}
 fpmprotect:=dlsym(dlopen('libc.dylib',RTLD_NOW),'mprotect');
{$else}
 fpmprotect:=dlsym(dlopen('libc.so',RTLD_NOW),'mprotect');
{$endif}
 if not assigned(fpmprotect) then begin
  raise Exception.Create('Importing of mprotect from libc.so failed!');
 end;
{$endif}
{$endif}
end;

procedure DoneBESEN;
begin
end;

initialization
 InitBESEN;
finalization
 DoneBESEN;
end.
