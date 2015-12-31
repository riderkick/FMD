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
unit BESENStringTree;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENStringUtils;

type TBESENStringTreeData=record
      case boolean of
       false:(i:int64);
       true:(p:pointer);
     end;

     PBESENStringTreeNode=^TBESENStringTreeNode;
     TBESENStringTreeNode=record
      TheChar:widechar;
      Data:TBESENStringTreeData;
      DataExist:longbool;
      Previous,Next,Up,Down:PBESENStringTreeNode;
     end;

     TBESENStringTree=class
      public
       Root:PBESENStringTreeNode;
       function CreateBESENStringTreeNode(AChar:widechar):PBESENStringTreeNode;
       procedure DestroyBESENStringTreeNode(Node:PBESENStringTreeNode);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       procedure DumpTree;
       procedure DumpList;
       procedure AppendTo(DestBESENStringTree:TBESENStringTree);
       procedure Optimize(DestBESENStringTree:TBESENStringTree);
       function Add(Content:TBESENString;const Data:TBESENStringTreeData;Replace:boolean=false):boolean;
       function Delete(Content:TBESENString):boolean;
       function Find(Content:TBESENString;var Data:TBESENStringTreeData):boolean;
     end;

implementation

constructor TBESENStringTree.Create;
begin
 inherited Create;
 Root:=nil;
 Clear;
end;

destructor TBESENStringTree.Destroy;
begin
 Clear;
 inherited Destroy;
end;

function TBESENStringTree.CreateBESENStringTreeNode(AChar:widechar):PBESENStringTreeNode;
begin
 getmem(result,sizeof(TBESENStringTreeNode));
 fillchar(result^.Data,sizeof(TBESENStringTreeData),#0);
 result^.TheChar:=AChar;
 result^.DataExist:=false;
 result^.Previous:=nil;
 result^.Next:=nil;
 result^.Up:=nil;
 result^.Down:=nil;
end;

procedure TBESENStringTree.DestroyBESENStringTreeNode(Node:PBESENStringTreeNode);
begin
 if not assigned(Node) then exit;
 DestroyBESENStringTreeNode(Node^.Next);
 DestroyBESENStringTreeNode(Node^.Down);
 freemem(Node);
end;

procedure TBESENStringTree.Clear;
begin
 DestroyBESENStringTreeNode(Root);
 Root:=nil;
end;

procedure TBESENStringTree.DumpTree;
var Ident:integer;
 procedure DumpNode(Node:PBESENStringTreeNode);
 var SubNode:PBESENStringTreeNode;
     IdentCounter,IdentOld:integer;
 begin
  for IdentCounter:=1 to Ident do write(' ');
  write(Node^.TheChar);
  IdentOld:=Ident;
  SubNode:=Node^.Next;
  while assigned(SubNode) do begin
   write(SubNode^.TheChar);
   if not assigned(SubNode^.Next) then break;
   inc(Ident);
   SubNode:=SubNode^.Next;
  end;
  writeln;
  inc(Ident);
  while assigned(SubNode) and (SubNode<>Node) do begin
   if assigned(SubNode^.Down) then DumpNode(SubNode^.Down);
   SubNode:=SubNode^.Previous;
   dec(Ident);
  end;
  Ident:=IdentOld;
  if assigned(Node^.Down) then DumpNode(Node^.Down);
 end;
begin
 Ident:=0;
 DumpNode(Root);
end;

procedure TBESENStringTree.DumpList;
 procedure DumpNode(Node:PBESENStringTreeNode;const ParentStr:TBESENString);
 var s:TBESENString;
 begin
  if not assigned(Node) then exit;
  if Node^.DataExist then begin
   s:=copy(ParentStr,0,length(ParentStr))+Node^.TheChar;
   writeln({$ifndef BESENSingleStringType}BESENUTF16ToUTF8({$endif}s{$ifndef BESENSingleStringType}){$endif});
  end;
  if assigned(Node^.Next) then begin
   s:=copy(ParentStr,0,length(ParentStr))+Node^.TheChar;
   DumpNode(Node^.Next,s);
  end;
  if assigned(Node^.Down) then DumpNode(Node^.Down,ParentStr);
 end;
begin
 if not assigned(Root) then exit;
 DumpNode(Root,'');
end;

procedure TBESENStringTree.AppendTo(DestBESENStringTree:TBESENStringTree);
 procedure DumpNode(Node:PBESENStringTreeNode;const ParentStr:TBESENString);
 var s:TBESENString;
 begin
  if not assigned(Node) then exit;
  if Node^.DataExist then begin
   s:=copy(ParentStr,0,length(ParentStr))+Node^.TheChar;
   DestBESENStringTree.Add(s,Node^.Data);
  end;
  if assigned(Node^.Next) then begin
   s:=copy(ParentStr,0,length(ParentStr))+Node^.TheChar;
   DumpNode(Node^.Next,s);
  end;
  if assigned(Node^.Down) then DumpNode(Node^.Down,ParentStr);
 end;
begin
 if not assigned(DestBESENStringTree) then exit;
 if not assigned(Root) then exit;
 DumpNode(Root,'');
end;

procedure TBESENStringTree.Optimize(DestBESENStringTree:TBESENStringTree);
 procedure DumpNode(Node:PBESENStringTreeNode;ParentStr:TBESENString);
 var s:TBESENString;
 begin
  if not assigned(Node) then exit;
  ParentStr:=ParentStr;
  if Node^.DataExist then begin
   s:=copy(ParentStr,0,length(ParentStr))+Node^.TheChar;
   DestBESENStringTree.Add(s,Node^.Data);
  end;
  if assigned(Node^.Next) then begin
   s:=copy(ParentStr,0,length(ParentStr))+Node^.TheChar;
   DumpNode(Node^.Next,s);
  end;
  if assigned(Node^.Down) then DumpNode(Node^.Down,ParentStr);
 end;
begin
 if not assigned(DestBESENStringTree) then exit;
 DestBESENStringTree.Clear;
 if not assigned(Root) then exit;
 DumpNode(Root,'');
end;

function TBESENStringTree.Add(Content:TBESENString;const Data:TBESENStringTreeData;Replace:boolean=false):boolean;
var StringLength,Position,PositionCounter:integer;
    NewNode,LastNode,Node:PBESENStringTreeNode;
    StringChar,NodeChar:widechar;
begin
 result:=false;
 StringLength:=length(Content);
 if StringLength>0 then begin
  LastNode:=nil;
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    if NodeChar=StringChar then begin
     LastNode:=Node;
     Node:=Node^.Next;
   end else begin
     while (NodeChar<StringChar) and assigned(Node^.Down) do begin
      Node:=Node^.Down;
      NodeChar:=Node^.TheChar;
     end;
     if NodeChar=StringChar then begin
      LastNode:=Node;
      Node:=Node^.Next;
     end else begin
      NewNode:=CreateBESENStringTreeNode(StringChar);
      if NodeChar<StringChar then begin
       NewNode^.Down:=Node^.Down;
       NewNode^.Up:=Node;
       if assigned(NewNode^.Down) then begin
        NewNode^.Down^.Up:=NewNode;
       end;
       NewNode^.Previous:=Node^.Previous;
       Node^.Down:=NewNode;
      end else if NodeChar>StringChar then begin
       NewNode^.Down:=Node;
       NewNode^.Up:=Node^.Up;
       if assigned(NewNode^.Up) then begin
        NewNode^.Up^.Down:=NewNode;
       end;
       NewNode^.Previous:=Node^.Previous;
       if not assigned(NewNode^.Up) then begin
        if assigned(NewNode^.Previous) then begin
         NewNode^.Previous^.Next:=NewNode;
        end else begin
         Root:=NewNode;
        end;
       end;
       Node^.Up:=NewNode;
      end;
      LastNode:=NewNode;
      Node:=LastNode^.Next;
     end;
    end;
   end else begin
    for PositionCounter:=Position to StringLength do begin
     NewNode:=CreateBESENStringTreeNode(Content[PositionCounter]);
     if assigned(LastNode) then begin
      NewNode^.Previous:=LastNode;
      LastNode^.Next:=NewNode;
      LastNode:=LastNode^.Next;
     end else begin
      if not assigned(Root) then begin
       Root:=NewNode;
       LastNode:=Root;
      end;
     end;
    end;
    break;
   end;
  end;
  if assigned(LastNode) then begin
   if Replace or not LastNode^.DataExist then begin
    LastNode^.Data:=Data;
    LastNode^.DataExist:=true;
    result:=true;
   end;
  end;
 end;
end;

function TBESENStringTree.Delete(Content:TBESENString):boolean;
var StringLength,Position:integer;
    Node:PBESENStringTreeNode;
    StringChar,NodeChar:widechar;
begin
 result:=false;
 StringLength:=length(Content);
 if StringLength>0 then begin
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    while (NodeChar<>StringChar) and assigned(Node^.Down) do begin
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    end;
    if NodeChar=StringChar then begin
     if (Position=StringLength) and Node^.DataExist then begin
      Node^.DataExist:=false;
      result:=true;
      exit;
     end;
     Node:=Node^.Next;
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end;
end;

function TBESENStringTree.Find(Content:TBESENString;var Data:TBESENStringTreeData):boolean;
var StringLength,Position:integer;
    Node:PBESENStringTreeNode;
    StringChar,NodeChar:TBESENString;
begin
 result:=false;
 StringLength:=length(Content);
 if StringLength>0 then begin
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    while (NodeChar<>StringChar) and assigned(Node^.Down) do begin
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    end;
    if NodeChar=StringChar then begin
     if (Position=StringLength) and Node^.DataExist then begin
      Data:=Node^.Data;
      result:=true;
      exit;
     end;
     Node:=Node^.Next;
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end;
end;

end.
