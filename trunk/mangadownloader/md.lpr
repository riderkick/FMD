{$define MANGADOWNLOADER}

uses
  {$DEFINE UseCThreads}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmMain, virtualtreeview_package, richmemopackage, uUpdateThread,
  uSubThread, uUpdateDBThread, uSilentThread, frmImportList, uFMDThread,
  uPacker, uOption;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, MainForm);
  Application.CreateForm(TfrmImportList, ImportList);
 // Application.CreateForm(TTNewChapter, TNewChapter);
 // Application.CreateForm(TLog, Log);
  Application.Run;
end.

