{$define MANGADOWNLOADER}

uses
  {$DEFINE UseCThreads}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainunit, virtualtreeview_package, richmemopackage, zip,
  updatelist, subthreads, updatedb, silentthreads, curlcore;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
 // Application.CreateForm(TTNewChapter, TNewChapter);
 // Application.CreateForm(TLog, Log);
  Application.Run;
end.

