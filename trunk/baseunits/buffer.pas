{
        File: buffer.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit buffer;

{$mode delphi}

interface

uses
  baseunit, SysUtils, Classes, HTTPSend, graphics, genericlib, IniFiles, blcksock;

type
  THTTPBuffer = class(THTTPSend)
  public
    fileSize: Cardinal;
    constructor Create;
    procedure   OnHookStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
    procedure   OnHookMonitor(Sender: TObject; Writing: Boolean; const Buffer: TMemory; Len: Integer);
    procedure   SyncDocToBuffer;
  end;

implementation

constructor THTTPBuffer.Create;
begin
  inherited;
  fileSize:= 0;
end;

procedure   THTTPBuffer.OnHookStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
var
  LineHeader, Location: String;
  I: Integer;
begin
  case Reason of
    HR_ReadCount:
      begin
        if fileSize = 0 then
          fileSize:= self.DownloadSize;
      end;

    HR_SocketClose:
      begin
        if ResultCode = 500 then
          exit;
        if ResultCode = 302 then
        begin
          fileSize := 0;
          Location := '' ;
          I        := 0 ;

          while (Location = '') and (I < self.Headers.Count) do
          begin
            LineHeader := self.Headers[I];
            if pos('Location: ',LineHeader) = 1 then
               Location := copy(LineHeader, 11, Length(LineHeader));
            Inc(I);
          end;
          exit;
        end;

       { if Assigned(fFileStream) then
        begin
          SyncDocToFile;
          FreeAndNil(fFileStream);
        end; }
      end;
  end;
end;

procedure   THTTPBuffer.OnHookMonitor(Sender: TObject; Writing: Boolean; const Buffer: TMemory; Len: Integer);
begin

end;

procedure   THTTPBuffer.SyncDocToBuffer;
var
  Diff   : Integer;
  DocSize: Integer;
begin
 { try
    DocSize:= HTTP.Document.Size;
    if DocSize = 0 then
      exit;

    Diff:= DocSize - self.Size;
    if Diff > 0 then
    begin
      HTTP.Document.Seek(-Diff, soFromEnd);
      self.CopyFrom(HTTP.Document, Diff);
    end;
  finally
    HTTP.Document.Seek(0, soFromEnd);
  end; }
end;

end.

