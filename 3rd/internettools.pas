{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit internettools;

interface

uses
  bbutils, extendedhtmlparser, simpleinternet, internetaccess, simplehtmlparser, simplehtmltreeparser, simplexmlparser, xquery, 
  synapseinternetaccess, w32internetaccess, simplexmltreeparserfpdom, xquery_json, mockinternetaccess, xquery__regex, xquery__parse, 
  xquery_module_math, xquery__functions, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('internettools', @Register);
end.
