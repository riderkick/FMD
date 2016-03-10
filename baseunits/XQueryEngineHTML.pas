unit XQueryEngineHTML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery, xquery_json, simplehtmltreeparser;

type

  { TXQueryEngineHTML }

  TXQueryEngineHTML = class
  private
    FEngine: TXQueryEngine;
    FTreeParser: TTreeParser;
    function Eval(Expression: String; isCSS: Boolean = False; Tree: TTreeNode = nil): IXQValue;
  public
    constructor Create(HTML: String = ''); overload;
    constructor Create(HTMLStream: TStream); overload;
    destructor Destroy; override;
    procedure ParseHTML(HTML: String); overload;
    procedure ParseHTML(HTMLStream: TStream); overload;
    function XPath(Expression: String; Tree: TTreeNode = nil): IXQValue; inline;
    function XPathString(Expression: String; Tree: TTreeNode = nil): String; inline;
    function CSS(Expression: String; Tree: TTreeNode = nil): IXQValue; inline;
    function CSSString(Expression: String; Tree: TTreeNode = nil): String; inline;
    property Engine: TXQueryEngine read FEngine;
  end;

  IXQValue = xquery.IXQValue;

implementation

function StreamToString(const Stream: TStream): string;
var
  x: Integer;
begin
  Stream.Position := 0;
  Setlength(Result, Stream.Size);
  x := Stream.Read(PChar(Result)^, Stream.Size);
  SetLength(Result, x);
end;

{ TXQueryEngineHTML }

function TXQueryEngineHTML.Eval(Expression: String; isCSS: Boolean; Tree: TTreeNode): IXQValue;
var
  t: TTreeNode;
begin
  Result := xqvalue();
  t := Tree;
  if t = nil then t := FTreeParser.getLastTree;
  if Expression = '' then Exit;
  try
    if isCSS then Result := FEngine.evaluateCSS3(Expression, t)
    else Result := FEngine.evaluateXPath3(Expression, t);
  except
  end;
end;

constructor TXQueryEngineHTML.Create(HTML: String);
begin
  FEngine := TXQueryEngine.create;
  FTreeParser := TTreeParser.Create;
  with FTreeParser do begin
    parsingModel := pmHTML;
    repairMissingStartTags := True;
    repairMissingEndTags := True;
    trimText := False;
    readComments := False;
    readProcessingInstructions := False;
    autoDetectHTMLEncoding := False;
    if HTML <> '' then parseTree(HTML);
  end;
end;

constructor TXQueryEngineHTML.Create(HTMLStream: TStream);
begin
  if Assigned(HTMLStream) then
    Create(StreamToString(HTMLStream))
  else
    Create;
end;

destructor TXQueryEngineHTML.Destroy;
begin
  FEngine.Free;
  FTreeParser.Free;
  inherited Destroy;
end;

procedure TXQueryEngineHTML.ParseHTML(HTML: String);
begin
  if HTML <> '' then FTreeParser.parseTree(HTML);
end;

procedure TXQueryEngineHTML.ParseHTML(HTMLStream: TStream);
begin
  ParseHTML(StreamToString(HTMLStream));
end;

function TXQueryEngineHTML.XPath(Expression: String; Tree: TTreeNode): IXQValue;
begin
  Result := Eval(Expression, False, Tree);
end;

function TXQueryEngineHTML.XPathString(Expression: String; Tree: TTreeNode
  ): String;
begin
  Result := Eval(Expression, False, Tree).toString;
end;

function TXQueryEngineHTML.CSS(Expression: String; Tree: TTreeNode): IXQValue;
begin
  Result := Eval(Expression, True, Tree);
end;

function TXQueryEngineHTML.CSSString(Expression: String; Tree: TTreeNode
  ): String;
begin
  Result := Eval(Expression, True, Tree).toString;
end;

end.

