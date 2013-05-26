{$MODE Delphi}
{
 This is a FastHTMLParser unit to parse HTML
 (disect html into its tags and text.)
 
  - Modified for use as a pure command line unit (no dialogs) for freepascal.
  - Also added UPPERCASE tags so that when you check for <font> it returns all
    tags like <FONT> and <FoNt> and <font>

 Regards,
  Lars aka L505
  http://z505.com
  

 Use it to:
    -Make your own web browsers,
    -make your own text web browsers (something like Lynx for linux maybe)
    -Parse websites
    -Grab content from websites -without- using regular expressions
    -Seems to be MUCH MUCH FASTER than regular expressions, from what I tested.
    -convert website tables into spreadsheets (you have to do the work after parsing it)
    -convert websites into text files (you have to do the work after parsing it)
    -convert website tables into CSV/Database (you have to do the work after parsing it)
    -find certain info from a web page.. i.e. all the table cells, all the bold text.
    -Parse websites remotely from a CGI app using something like Synapse and SynWrap
     to first get the HTML site, then parse it with these fasthtmlparse units.
     This would allow you to dynamically parse info from websites and display
     data on your site in real time.
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// TITLE        : Fast HTML Parser                                            //
// CLASS        : TjsFastHTMLParser                                           //
// VERSION      : 0.4                                                         //
// AUTHOR       : James Azarja                                                //
// WEBSITE      : http://www.jazarsoft.com/                                   //
// LEGAL        : Copyright (C) 2004 Jazarsoft, All Rights Reserved.          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// This code may be used and modified by anyone so long as  this header and   //
// copyright  information remains intact.                                     //
//                                                                            //
// The code is provided "AS-IS" and without WARRANTY OF ANY KIND,             //
// expressed, implied or otherwise, including and without limitation, any     //
// warranty of merchantability or fitness for a  particular purpose.        //
//                                                                            //
// In no event shall the author be liable for any special, incidental,        //
// indirect or consequential damages whatsoever (including, without           //
// limitation, damages for loss of profits, business interruption, loss       //
// of information, or any other loss), whether or not advised of the          //
// possibility of damage, and on any theory of liability, arising out of      //
// or in connection with the use or inability to use this software.         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// HISTORY:                                                                   //
// 0.1 - [o] Initial Development                                              //
//            mostly based on Peter Irlam works & ideas                       //
// 0.2 - [*] Some minor bug has fixed                                         //
// 0.3 - [*] Some jsHTMLUtil function bug has been fixed                      //
// 0.4 - [*] jsHTMLUtil Tag Attributes bug has been fixed                     //
//            thanks to Dmitry [mail@vader.ru]                                //
//                                                                            //
// LEGEND:                                                                    //
// [o] : Information                                                          //
// [+] : Add something                                                        //
// [-] : Remove something                                                     //
// [*] : Fix                                                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
{ $Id: jsFastHTMLParser.pas,v 1.2 2004/09/10 02:25:56 jazar Exp $ }

unit FastHTMLParser;

interface

Uses SysUtils;

type
  TOnFoundTag  = procedure(Tag: string) of object;
  TOnFoundText = procedure(Text: string) of object;
  
  TjsFastHTMLParser = class(TObject)
    public
      OnFoundTag   : TOnFoundTag;
      OnFoundText  : TOnFoundText;
      Raw          : Pchar;
      constructor Create(sRaw:String);overload;
      constructor Create(pRaw:PChar);overload;
      destructor Destroy; override;
      procedure Exec;
      procedure SlowExec;
      procedure ExecUpCase; //same as Exec, but all tags are now converted to UPPERCASE (consistent)
  end;

implementation

function CopyBuffer(StartIndex: PChar;Length:Integer):String;
Var
  S : String;
begin
  SetLength(S, Length);
  StrLCopy(@S[1], StartIndex, Length);
  Result := S;
  S:= '';
end;

constructor TjsFastHTMLParser.Create(sRaw:String);
begin
  Raw := Pchar(sRaw);
  inherited Create;
end;

constructor TjsFastHTMLParser.Create(pRaw:Pchar);
begin
  Raw := pRaw;
  inherited Create;
end;

destructor  TjsFastHTMLParser.Destroy;
begin
  Raw:= '';
  inherited Destroy;
end;

procedure TjsFastHTMLParser.Exec;
Var
  L     : Integer;
  TL    : Integer;
  I     : Integer;
  Done  : Boolean;
  TagStart,
  TextStart,
  P     : PChar;   // Pointer to current char.
  C     : Char;
begin
  TL := StrLen(Raw);
  I  := 0;
  P  := Raw;
  Done := False;
  if P<>nil then
  begin
    TagStart := nil;
    repeat
      TextStart := P;
      { Get next tag position }
      while Not (P^ in [ '<', #0 ]) do
      begin
        Inc(P);Inc(I);
        if I>=TL then
        begin
          Done := True;
          Break;
        end;
      end;
      if Done then Break;
      
      { Is there any text before ? }
      if (TextStart<>nil) and (P>TextStart) then
      begin

        L := P-TextStart;
        { Yes, copy to buffer }
        
        if (assigned(OnFoundText)) then
          OnFoundText(CopyBuffer( TextStart, L ));

      end else
      begin
        TextStart:=nil;
      end;
      { No }

      TagStart := P;
      while Not (P^ in [ '>', #0]) do
      begin

        // Find string in tag
        if (P^='"') or (P^='''') then
        begin
          C:= P^;
          Inc(P);Inc(I); // Skip current char " or '

          // Skip until string end
          while Not (P^ in [C, #0]) do
          begin
            Inc(P);Inc(I);
          end;
        end;

        Inc(P);Inc(I);
        if I>=TL then
        begin
          Done := True;
          Break;
        end;
      end;
      if Done then Break;
      { Copy this tag to buffer }
      L := P-TagStart+1;

      if (Assigned(OnFoundTag)) then
        OnFoundTag(CopyBuffer( TagStart, L ));

      Inc(P);Inc(I);
      if I>=TL then Break;

    until (Done);
  end;
end;

procedure TjsFastHTMLParser.SlowExec;
Var
  L     : Integer;
  TL    : Integer;
  I     : Integer;
  Done  : Boolean;
  TagStart,
  TextStart,
  P     : PChar;   // Pointer to current char.
  C     : Char;
begin
  TL := StrLen(Raw);
  I  := 0;
  P  := Raw;
  Done := False;
  if P<>nil then
  begin
    TagStart := nil;
    repeat
      TextStart := P;
      { Get next tag position }
      while Not (P^ in [ '<', #0 ]) do
      begin
        Inc(P);Inc(I);
        if I>=TL then
        begin
          Done := True;
          Break;
        end;
      end;
      if Done then Break;

      { Is there any text before ? }
      if (TextStart<>nil) and (P>TextStart) then
      begin

        L := P-TextStart;
        { Yes, copy to buffer }

        if (assigned(OnFoundText)) then
          OnFoundText(CopyBuffer( TextStart, L ));

      end else
      begin
        TextStart:=nil;
      end;
      { No }

      TagStart := P;
      while Not (P^ in [ '>', #0]) do
      begin

        Inc(P);Inc(I);
        if I>=TL then
        begin
          Done := True;
          Break;
        end;
      end;
      if Done then Break;
      { Copy this tag to buffer }
      L := P-TagStart+1;

      if (Assigned(OnFoundTag)) then
        OnFoundTag(CopyBuffer( TagStart, L ));

      Inc(P);Inc(I);
      if I>=TL then Break;

    until (Done);
  end;
end;


{L505: Added this function so we can parse HTML converting alltags to UPPERCASE
       This makes it easier in those cases where an html file has mixed <b> and
       <B> and <FONT> and <font>}
procedure TjsFastHTMLParser.ExecUpCase;
Var
  L     : Integer;
  TL    : Integer;
  I     : Integer;
  Done  : Boolean;
  TagStart,
  TextStart,
  P     : PChar;   // Pointer to current char.
  C     : Char;
begin
  TL := StrLen(Raw);
  I  := 0;
  P  := Raw;
  Done := False;
  if P<>nil then
  begin
    TagStart := nil;
    repeat
      TextStart := P;
      { Get next tag position }
      while Not (P^ in [ '<', #0 ]) do
      begin
        Inc(P);Inc(I);
        if I>=TL then
        begin
          Done := True;
          Break;
        end;
      end;
      if Done then Break;

      { Is there any text before ? }
      if (TextStart<>nil) and (P>TextStart) then
      begin

        L := P-TextStart;
        { Yes, copy to buffer }

        if (assigned(OnFoundText)) then
          OnFoundText(CopyBuffer( TextStart, L ));

      end else
      begin
        TextStart:=nil;
      end;
      { No }

      TagStart := P;
      while Not (P^ in [ '>', #0]) do
      begin

        // Find string in tag
        if (P^='"') or (P^='''') then
        begin
          C:= P^;
          Inc(P);Inc(I); // Skip current char " or '

          // Skip until string end
          while Not (P^ in [C, #0]) do
          begin
            Inc(P);Inc(I);
          end;
        end;

        Inc(P);Inc(I);
        if I>=TL then
        begin
          Done := True;
          Break;
        end;
      end;
      if Done then Break;
      { Copy this tag to buffer }
      L := P-TagStart+1;

      if (Assigned(OnFoundTag)) then
        OnFoundTag(uppercase(CopyBuffer( TagStart, L )));//L505: added upppercase

      Inc(P);Inc(I);
      if I>=TL then Break;

    until (Done);
  end;
end;

end.
