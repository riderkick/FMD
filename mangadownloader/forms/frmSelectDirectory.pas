unit frmSelectDirectory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  Buttons, StdCtrls;

type

  { TSelectDirectoryForm }

  TSelectDirectoryForm = class(TForm)
    btOK: TBitBtn;
    dePath: TDirectoryEdit;
    lbDescription: TLabel;
  private

  public

  end;

var
  SelectDirectoryForm: TSelectDirectoryForm;

implementation

{$R *.lfm}

end.

