unit frmWebsiteOptionCustom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Spin, WebsiteModules;

type

  { TCheckBoxBindValue }

  TCheckBoxBindValue = class(TCheckBox)
  private
    FBindValue: PBoolean;
    procedure SetBindValue(AValue: PBoolean);
  protected
    procedure ValueChange(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    property BindValue: PBoolean read FBindValue write SetBindValue;
  end;

  { TEditBindValue }

  TEditBindValue = class(TEdit)
  private
    FBindValue: PString;
    procedure SetBindValue(AValue: PString);
  protected
    procedure ValueChange(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    property BindValue: PString read FBindValue write SetBindValue;
  end;

  { TSpinEditBindValue }

  TSpinEditBindValue = class(TSpinEdit)
  private
    FBindValue: PInteger;
    procedure SetBindValue(AValue: PInteger);
  protected
    procedure ValueChange(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    property BindValue: PInteger read FBindValue write SetBindValue;
  end;

  { TComboBoxBindValue }

  TComboBoxBindValue = class(TComboBox)
  private
    FBindValue: PInteger;
    procedure SetBindValue(AValue: PInteger);
  protected
    procedure ValueChange(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    property BindValue: PInteger read FBindValue write SetBindValue;
  end;

  { TCustomOptionForm }

  TCustomOptionForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    function AddOptionItem(const AOptionItemType: TWebsiteOptionType;
      const AName, ACaption, AGroup, AGroupCaption: String): TWinControl;
  public
    { public declarations }
    function AddCheckbox(const ABindValue: PBoolean;
      const AName, ACaption, AGroup, AGroupCaption: String): TWinControl;
    function AddEdit(const ABindValue: PString;
      const AName, ACaption, AGroup, AGroupCaption: String): TWinControl;
    function AddSpinEdit(const ABindValue: PInteger;
      AName, ACaption, AGroup, AGroupCaption: String): TWinControl;
    function AddComboBox(const ABindValue: PInteger;
      AName, ACaption, AGroup, AGroupCaption, AItems: String): TWinControl;
    procedure CreateWebsiteOption;
  end;

var
  WebsiteOptionCustomForm: TCustomOptionForm;
  downer: TComponent;
  dparent: TWinControl;
  tbspace: Cardinal = 6;
  lrspace: Cardinal = 6;
  hspace: Cardinal = 4;
  vspace: Cardinal = 4;

const
  TWebsiteOptionItemTypeStr: array[TWebsiteOptionType] of String =
    ('ack', 'ae', 'ase', 'acb');

implementation

{$R *.lfm}

{ TCheckBoxBindValue }

procedure TCheckBoxBindValue.SetBindValue(AValue: PBoolean);
begin
  if FBindValue = AValue then Exit;
  FBindValue := AValue;
  if Assigned(FBindValue) then
    Checked := FBindValue^;
end;

procedure TCheckBoxBindValue.ValueChange(Sender: TObject);
begin
  if Assigned(FBindValue) then
    FBindValue^ := Checked;
end;

constructor TCheckBoxBindValue.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnChange := @ValueChange;
end;

{ TEditBindValue }

procedure TEditBindValue.SetBindValue(AValue: PString);
begin
  if FBindValue = AValue then Exit;
  FBindValue := AValue;
  if Assigned(FBindValue) then
    Text := FBindValue^;
end;

procedure TEditBindValue.ValueChange(Sender: TObject);
begin
  if Assigned(FBindValue) then
    FBindValue^ := Text;
end;

constructor TEditBindValue.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnChange := @ValueChange;
end;

{ TSpinEditBindValue }

procedure TSpinEditBindValue.SetBindValue(AValue: PInteger);
begin
  if FBindValue = AValue then Exit;
  FBindValue := AValue;
  if Assigned(FBindValue) then
    Value := FBindValue^;
end;

procedure TSpinEditBindValue.ValueChange(Sender: TObject);
begin
  if Assigned(FBindValue) then
    FBindValue^ := Value;
end;

constructor TSpinEditBindValue.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  MinValue := 0;
  MaxValue := 10000;
  OnChange := @ValueChange;
end;

{ TComboBoxBindValue }

procedure TComboBoxBindValue.SetBindValue(AValue: PInteger);
begin
  if FBindValue = AValue then Exit;
  FBindValue := AValue;
  if Assigned(FBindValue) then
    if FBindValue^ < Items.Count then
      ItemIndex := FBindValue^;
end;

procedure TComboBoxBindValue.ValueChange(Sender: TObject);
begin
  if Assigned(FBindValue) then
    FBindValue^ := ItemIndex;
end;

constructor TComboBoxBindValue.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnChange := @ValueChange;
  Style := csDropDownList;
end;

{ TCustomOptionForm }

procedure TCustomOptionForm.FormCreate(Sender: TObject);
begin
  downer := self;
  dparent := Self;
  with dparent.ChildSizing do
  begin
    TopBottomSpacing := tbspace;
    LeftRightSpacing := lrspace;
    HorizontalSpacing := hspace;
    VerticalSpacing := vspace;
  end;
end;

function TCustomOptionForm.AddOptionItem(const AOptionItemType: TWebsiteOptionType;
  const AName, ACaption, AGroup, AGroupCaption: String): TWinControl;
var
  i: Integer;
  compparent: TWinControl;
  compparentsibling, compsibling: TControl;
  lcomp, lcompcaption, lgroup, lgroupcaption: String;
  lb: TLabel;

  procedure SetControlProp(const AControl, ASibling: TControl;
  const AParent: TWinControl; const AName, ACaption: String);
  begin
    with AControl do
    begin
      if AParent <> nil then
        Parent := AParent;
      Name := AName;
      Caption := ACaption;
      AutoSize := True;
      Top := AParent.ChildSizing.TopBottomSpacing;
      AnchorParallel(akLeft, 0, AParent);
      if ASibling <> nil then
      begin
        Top := ASibling.Top + ASibling.Height;
        AnchorToNeighbour(akTop, 0, ASibling);
      end;
    end;
  end;

begin
  Result := nil;
  lcomp := CleanOptionName(AName);
  if lcomp = '' then Exit;
  lcompcaption := Trim(ACaption);
  lgroup := CleanOptionName(AGroup);
  lgroupcaption := Trim(AGroupCaption);
  compparent := nil;
  compparentsibling := nil;
  compsibling := nil;

  if (lcompcaption = '') and (lcomp <> '') then
    lcompcaption := Trim(AName);
  if (lgroupcaption = '') and (lgroup <> '') then
    lgroupcaption := Trim(AGroup);

  if lcomp <> '' then
  begin
    if lgroup <> '' then
      lcomp := lgroup + lcomp;
    lcomp := TWebsiteOptionItemTypeStr[AOptionItemType] + lcomp;
    if lgroup <> '' then
      lgroup := 'agb' + lgroup;
  end;

  with dparent do
    if ComponentCount > 0 then
      for i := ComponentCount - 1 downto 0 do
      begin
        if SameText(Components[i].Name, lcomp) then
        begin
          Result := TWinControl(Components[i]);
          Exit;
        end
        else
        if (Components[i] is TGroupBox) and SameText(Components[i].Name, lgroup) then
        begin
          compparent := TGroupBox(Components[i]);
          with compparent do
            if ControlCount > 0 then
              compsibling := TControl(Controls[ControlCount - 1]);
        end;
      end;

  with dparent do
    if ComponentCount > 0 then
    begin
      i := ComponentCount - 1;
      if TWinControl(Components[i]).Parent is TGroupBox then
        compparentsibling := TWinControl(Components[i]).Parent
      else
        compparentsibling := TWinControl(Components[i]);
    end;

  Self.BeginFormUpdate;
  try
    if compparent = nil then
      if lgroup <> '' then
      begin
        compparent := TGroupBox.Create(downer);
        SetControlProp(compparent, compparentsibling, dparent, lgroup, lgroupcaption);
        with compparent.ChildSizing do
        begin
          TopBottomSpacing := dparent.ChildSizing.TopBottomSpacing;
          LeftRightSpacing := dparent.ChildSizing.LeftRightSpacing;
          HorizontalSpacing := dparent.ChildSizing.HorizontalSpacing;
          VerticalSpacing := dparent.ChildSizing.VerticalSpacing;
        end;
        compparent.Align := alTop;
      end
      else
      begin
        compparent := dparent;
        if (compsibling = nil) and (compparentsibling <> nil) then
          compsibling := compparentsibling;
      end;

    case AOptionItemType of
      woCheckBox:
      begin
        Result := TCheckBoxBindValue.Create(downer);
        SetControlProp(Result, compsibling, compparent, lcomp, lcompcaption);
      end;

      woEdit, woComboBox:
      begin
        lb := TLabel.Create(downer);
        SetControlProp(lb, compsibling, compparent, lcomp + 'Lbl', lcompcaption);
        compsibling := lb;
        case AOptionItemType of
          woEdit    : Result := TEditBindValue.Create(downer);
          woComboBox: Result := TComboBoxBindValue.Create(downer);
        end;
        SetControlProp(Result, compsibling, compparent, lcomp, '');
        with Result do
        begin
          Width := compparent.Width - compparent.ChildSizing.LeftRightSpacing;
          Anchors := Anchors + [akRight];
          Text := '';
        end;
      end;

      woSpinEdit:
      begin
        Result := TSpinEditBindValue.Create(downer);
        SetControlProp(Result, compsibling, compparent, lcomp, lcompcaption);
        Result.Width := Result.Width + (Result.Width div 4);
        lb := TLabel.Create(downer);
        SetControlProp(lb, Result, compparent, lcomp + 'Lbl', lcompcaption);
        with lb do
        begin
          AnchorToNeighbour(akLeft, 0, Result);
          AnchorVerticalCenterTo(Result);
        end;
      end;
    end;
  finally
    Self.EndFormUpdate;
  end;
end;

function TCustomOptionForm.AddCheckbox(const ABindValue: PBoolean;
  const AName, ACaption, AGroup, AGroupCaption: String
  ): TWinControl;
begin
  Result := AddOptionItem(woCheckBox, AName, ACaption, AGroup, AGroupCaption);
  if (Result <> nil) and (Result is TCheckBoxBindValue) then
    TCheckBoxBindValue(Result).BindValue := ABindValue;
end;

function TCustomOptionForm.AddEdit(const ABindValue: PString;
  const AName, ACaption, AGroup, AGroupCaption: String
  ): TWinControl;
begin
  Result := AddOptionItem(woEdit, AName, ACaption, AGroup, AGroupCaption);
  if (Result <> nil) and (Result is TEditBindValue) then
    TEditBindValue(Result).BindValue := ABindValue;
end;

function TCustomOptionForm.AddSpinEdit(const ABindValue: PInteger;
  AName, ACaption, AGroup, AGroupCaption: String
  ): TWinControl;
begin
  Result := AddOptionItem(woSpinEdit, AName, ACaption, AGroup, AGroupCaption);
  if (Result <> nil) and (Result is TSpinEditBindValue) then
    TSpinEditBindValue(Result).BindValue := ABindValue;
end;

function TCustomOptionForm.AddComboBox(const ABindValue: PInteger; AName, ACaption,
  AGroup, AGroupCaption, AItems: String): TWinControl;
begin
  Result := AddOptionItem(woComboBox, AName, ACaption, AGroup, AGroupCaption);
  if (Result <> nil) and (Result is TComboBoxBindValue) then
    with TComboBoxBindValue(Result) do
    begin
      Items.Text := AItems;
      TComboBoxBindValue(Result).BindValue := ABindValue;
    end;
end;

procedure TCustomOptionForm.CreateWebsiteOption;
var
  i, j: Integer;
  cap: String;
begin
  if Modules = nil then Exit;
  dparent.DestroyComponents;
  if Modules.Count > 0 then
    for i := 0 to Modules.Count - 1 do
      with Modules.Module[i] do
        if Length(OptionList) > 0 then
          for j := Low(OptionList) to High(OptionList) do
            with OptionList[j] do
            begin
              if Assigned(Caption) then
                cap := Caption^
              else
                cap := '';
              case OptionType of
                woCheckBox: AddCheckbox(BindValue, Name, cap, Website, Website);
                woEdit: AddEdit(BindValue, Name, cap, Website, Website);
                woSpinEdit: AddSpinEdit(BindValue, Name, cap, Website, Website);
                woComboBox: AddComboBox(BindValue, Name, cap, Website, Website, Items^);
              end;
            end;
end;

end.
