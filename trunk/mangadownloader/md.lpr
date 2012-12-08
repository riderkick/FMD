{$define MANGADOWNLOADER}

uses
 // {$IFDEF UNIX}{$IFDEF UseCThreads}
 // cthreads,
 // {$ENDIF}{$ENDIF}
  {$DEFINE UseCThreads}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainunit, virtualtreeview_package, richmemopackage, zip, updatelist;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

