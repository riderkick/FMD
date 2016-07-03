unit frmCustomColor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Forms,
  Graphics, Dialogs, ColorBox, ComCtrls, VirtualTrees, FMDOptions, IniFiles;

type
  TColorItem = record
    N: String;
    C: TColor;
  end;

  { TColorItems }

  TColorItems = class
  private
    FColors: array of TColorItem;
    function GetC(Index: Integer): TColor;
    function GetN(Index: Integer): String;
    procedure SetC(Index: Integer; AValue: TColor);
    procedure SetN(Index: Integer; AValue: String);
  public
    destructor Destroy; override;
  public
    function Count: Integer;
    procedure Add(const AName: String; const AColor: TColor);
    property N[Index: Integer]: String read GetN write SetN;
    property C[Index: Integer]: TColor read GetC write SetC; default;
  end;

  { TVirtualStringTree }

  TVirtualStringTree = class(VirtualTrees.TVirtualStringTree)
  private
    FCI: TColorItems;
    procedure SetCI(AValue: TColorItems);
  public
    property CI: TColorItems read FCI write SetCI;
  end;

  { TCustomColorForm }

  TCustomColorForm = class(TForm)
    CBColors: TColorBox;
    btColors: TColorButton;
    pcCustomColorList: TPageControl;
    tsChapterList: TTabSheet;
    tsMangaList: TTabSheet;
    tsFavoriteList: TTabSheet;
    tsBasicList: TTabSheet;
    VTBasicList: TVirtualStringTree;
    VTChapterList: TVirtualStringTree;
    VTMangaList: TVirtualStringTree;
    VTFavoriteList: TVirtualStringTree;
    procedure btColorsColorChanged(Sender: TObject);
    procedure CBColorsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VTBasicListDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VTBasicListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VTBasicListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VTBasicListPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
    { private declarations }
    procedure DrawBoxColorText(const TargetCanvas: TCanvas; const BoxColor: TColor;
      const CellText: String; CellRect: TRect);
    procedure SetSelectedColor(const AColor: TColor);
  public
    { public declarations }
  end;

  TVTList = specialize TFPGList<VirtualTrees.TVirtualStringTree>;

  { TVTApplyList }

  TVTApplyList = class
  private
    FVTList: TVTList;
    procedure VTOnPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure ApplyOnPaintText(Index: Integer);
    function GetItems(Index: Integer): VirtualTrees.TVirtualStringTree;
    procedure SetItems(Index: Integer; AValue: VirtualTrees.TVirtualStringTree);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AVT: VirtualTrees.TVirtualStringTree);
    procedure Remove(const AVT: VirtualTrees.TVirtualStringTree);
    function Count: Integer;
    property Items[Index: Integer]: VirtualTrees.TVirtualStringTree read GetItems write SetItems; default;
  end;

procedure AddVT(const AVT: VirtualTrees.TVirtualStringTree);
procedure RemoveVT(const AVT: VirtualTrees.TVirtualStringTree);
procedure Apply;
procedure LoadFromIniFile(const IniFile: TIniFile);
procedure SaveToIniFile(const IniFile: TIniFile);

var
  CustomColorForm: TCustomColorForm;

implementation

const
  TextStyleLeftCenter: TTextStyle = (
    Alignment: taLeftJustify;
    Layout: tlCenter;
    SingleLine: True;
    Clipping: False;
    ExpandTabs: True;
    ShowPrefix: False;
    Wordbreak: False;
    Opaque: False;
    SystemFont: False;
    RightToLeft: False;
    EndEllipsis: True);

var
  // color collection
  BasicListColors,
  MangaListColors,
  FavoriteListColors,
  ChapterListColor: TColorItems;

  // current selected color list
  SelectedColorList: TVirtualStringTree;

  // vt list to apply
  VTApplyList: TVTApplyList;

procedure DoInit;
begin
  BasicListColors := TColorItems.Create;
  with BasicListColors do
  begin
    Add('BackgroundColor', clWindow);
    Add('BorderColor', clBtnFace);
    Add('DisabledColor', clBtnShadow);
    Add('DropMarkColor', clHighlight);
    Add('DropTargetColor', clHighLight);
    Add('DropTargetBorderColor', clHotLight);
    Add('FocusedSelectionColor', clHighLight);
    Add('FocusedSelectionBorderColor', clHotLight);
    Add('GridLineColor', clBtnFace);
    Add('HeaderHotColor', clBtnShadow);
    Add('HotColor', clWindowText);
    Add('SelectionRectangleBlendColor', clHighlight);
    Add('SelectionRectangleBorderColor', clHotLight);
    Add('TreeLineColor', clBtnShadow);
    Add('UnfocusedSelectionColor', clBtnFace);
    Add('UnfocusedSelectionBorderColor', clBtnShadow);
    Add('NormalTextColor', clWindowText);
    Add('FocusedSelectionTextColor', clHighlightText);
    Add('UnfocusedSelectionTextColor', clWindowText);
  end;

  MangaListColors := TColorItems.Create;
  with MangaListColors do
  begin
    Add('NewMangaColor', CL_MNNewManga);
    Add('CompletedMangaColor', CL_MNCompletedManga);
  end;

  FavoriteListColors := TColorItems.Create;
  with FavoriteListColors do
  begin
    Add('BrokenFavoriteColor', CL_FVBrokenFavorite);
    Add('CheckingColor', CL_FVChecking);
    Add('NewChapterFoundColor', CL_FVNewChapterFound);
    Add('CompletedSeriesColor', CL_FVCompletedManga);
  end;

  ChapterListColor := TColorItems.Create;
  with ChapterListColor do
  begin
    Add('DownloadedColor', CL_CHDownloaded);
  end;

  SelectedColorList := nil;
  VTApplyList := TVTApplyList.Create;
end;

procedure DoFinal;
begin
  BasicListColors.Free;
  MangaListColors.Free;
  FavoriteListColors.Free;
  ChapterListColor.Free;
  VTApplyList.Free;
end;

procedure ApplyBasicColorToVT(const AVT: VirtualTrees.TVirtualStringTree);
begin
  with AVT.Colors do
  begin
    AVT.Color := BasicListColors[0];
    BorderColor := BasicListColors[1];
    DisabledColor := BasicListColors[2];
    DropMarkColor := BasicListColors[3];
    DropTargetColor := BasicListColors[4];
    DropTargetBorderColor := BasicListColors[5];
    FocusedSelectionColor := BasicListColors[6];
    FocusedSelectionBorderColor := BasicListColors[7];
    GridLineColor := BasicListColors[8];
    HeaderHotColor := BasicListColors[9];
    HotColor := BasicListColors[10];
    SelectionRectangleBlendColor := BasicListColors[11];
    SelectionRectangleBorderColor := BasicListColors[12];
    TreeLineColor := BasicListColors[13];
    UnfocusedSelectionColor := BasicListColors[14];
    UnfocusedSelectionBorderColor := BasicListColors[15];
    AVT.Repaint;
  end;
end;

procedure AddVT(const AVT: VirtualTrees.TVirtualStringTree);
begin
  VTApplyList.Add(AVT);
end;

procedure RemoveVT(const AVT: VirtualTrees.TVirtualStringTree);
begin
  VTApplyList.Remove(AVT);
end;

procedure ApplyToFMDOptions;
begin
  //basiclist
  CL_BSNormalText := BasicListColors[16];
  CL_BSFocusedSelectionText := BasicListColors[17];
  CL_BSUnfocesedSelectionText := BasicListColors[18];

  //mangalist
  CL_MNNewManga := MangaListColors[0];
  CL_MNCompletedManga := MangaListColors[1];

  //favoritelist
  CL_FVBrokenFavorite := FavoriteListColors[0];
  CL_FVChecking := FavoriteListColors[1];
  CL_FVNewChapterFound := FavoriteListColors[2];
  CL_FVCompletedManga := FavoriteListColors[3];

  //chapterlist
  CL_CHDownloaded := ChapterListColor[0];
end;

procedure Apply;
var
  i: Integer;
begin
  ApplyToFMDOptions;
  if VTApplyList.Count > 0 then
    for i := 0 to VTApplyList.Count - 1 do
      ApplyBasicColorToVT(VTApplyList[i]);
end;

procedure LoadFromIniFile(const IniFile: TIniFile);
var
  i: Integer;
begin
  with IniFile do
  begin
    //basiclist
    for i := 0 to BasicListColors.Count - 1 do
      BasicListColors[i] := StringToColor(ReadString('BasicListColors', BasicListColors.N[i], ColorToString(BasicListColors[i])));

    //mangalist
    for i := 0 to MangaListColors.Count - 1 do
      MangaListColors[i] := StringToColor(ReadString('MangaListColors', MangaListColors.N[i], ColorToString(MangaListColors[i])));

    //favoritelist
    for i := 0 to FavoriteListColors.Count - 1 do
      FavoriteListColors[i] := StringToColor(ReadString('FavoriteListColors', FavoriteListColors.N[i],
        ColorToString(FavoriteListColors[i])));

    //chapterlist
    for i := 0 to ChapterListColor.Count - 1 do
      ChapterListColor[i] := StringToColor(ReadString('ChapterListColor', ChapterListColor.N[i], ColorToString(ChapterListColor[i])));

    ApplyToFMDOptions;
  end;
end;

procedure SaveToIniFile(const IniFile: TIniFile);
var
  i: Integer;
begin
  with IniFile do
  begin
    //basiclist
    for i := 0 to BasicListColors.Count - 1 do
      WriteString('BasicListColors', BasicListColors.N[i], ColorToString(BasicListColors[i]));

    //mangalist
    for i := 0 to MangaListColors.Count - 1 do
      WriteString('MangaListColors', MangaListColors.N[i], ColorToString(MangaListColors[i]));

    //favoritelist
    for i := 0 to FavoriteListColors.Count - 1 do
      WriteString('FavoriteListColors', FavoriteListColors.N[i], ColorToString(FavoriteListColors[i]));

    //chapterlist
    for i := 0 to ChapterListColor.Count - 1 do
      WriteString('ChapterListColor', ChapterListColor.N[i], ColorToString(ChapterListColor[i]));
  end;
end;

{$R *.lfm}

{ TVTApplyList }

procedure TVTApplyList.VTOnPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  with TargetCanvas.Font do
  begin
    if Sender.Selected[Node] then
    begin
      if Sender.Focused then
        Color := CL_BSFocusedSelectionText
      else
        Color := CL_BSUnfocesedSelectionText;
    end
    else
      Color := CL_BSNormalText;
  end;
end;

procedure TVTApplyList.ApplyOnPaintText(Index: Integer);
begin
  if not Assigned(FVTList[Index].OnPaintText) then
    FVTList[Index].OnPaintText := @VTOnPaintText;
end;

function TVTApplyList.GetItems(Index: Integer): VirtualTrees.TVirtualStringTree;
begin
  Result := FVTList[Index];
end;

procedure TVTApplyList.SetItems(Index: Integer; AValue: VirtualTrees.TVirtualStringTree);
begin
  if FVTList[Index] <> AValue then
    FVTList[Index] := AValue;
end;

constructor TVTApplyList.Create;
begin
  FVTList := TVTList.Create;
end;

destructor TVTApplyList.Destroy;
begin
  FVTList.Free;
  inherited Destroy;
end;

procedure TVTApplyList.Add(const AVT: VirtualTrees.TVirtualStringTree);
begin
  if FVTList.IndexOf(AVT) = -1 then
    ApplyOnPaintText(FVTList.Add(AVT));
end;

procedure TVTApplyList.Remove(const AVT: VirtualTrees.TVirtualStringTree);
begin

end;

function TVTApplyList.Count: Integer;
begin
  Result := FVTList.Count;
end;

{ TVirtualStringTree }

procedure TVirtualStringTree.SetCI(AValue: TColorItems);
begin
  if FCI = AValue then Exit;
  FCI := AValue;
  RootNodeCount := FCI.Count;
end;

{ TColorItems }

function TColorItems.GetC(Index: Integer): TColor;
begin
  Result := FColors[Index].C;
end;

function TColorItems.GetN(Index: Integer): String;
begin
  Result := FColors[Index].N;
end;

procedure TColorItems.SetC(Index: Integer; AValue: TColor);
begin
  if FColors[Index].C <> AValue then
    FColors[Index].C := AValue;
end;

procedure TColorItems.SetN(Index: Integer; AValue: String);
begin
  if FColors[Index].N <> AValue then
    FColors[Index].N := AValue;
end;

destructor TColorItems.Destroy;
begin
  SetLength(FColors, 0);
  inherited Destroy;
end;

function TColorItems.Count: Integer;
begin
  Result := Length(FColors);
end;

procedure TColorItems.Add(const AName: String; const AColor: TColor);
begin
  SetLength(FColors, Length(FColors) + 1);
  with FColors[High(FColors)] do
  begin
    N := AName;
    C := AColor;
  end;
end;

{ TCustomColorForm }

procedure TCustomColorForm.FormCreate(Sender: TObject);
begin
  AddVT(VTBasicList);
  AddVT(VTMangaList);
  AddVT(VTFavoriteList);
  AddVT(VTChapterList);
  VTBasicList.CI := BasicListColors;
  VTMangaList.CI := MangaListColors;
  VTFavoriteList.CI := FavoriteListColors;
  VTChapterList.CI := ChapterListColor;
end;

procedure TCustomColorForm.CBColorsChange(Sender: TObject);
begin
  btColors.ButtonColor := CBColors.Selected;
  SetSelectedColor(CBColors.Selected);
end;

procedure TCustomColorForm.btColorsColorChanged(Sender: TObject);
begin
  CBColors.Selected := btColors.ButtonColor;
  SetSelectedColor(btColors.ButtonColor);
end;

procedure TCustomColorForm.VTBasicListDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
  const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := False;
  DrawBoxColorText(TargetCanvas, TVirtualStringTree(Sender).CI[Node^.Index],
    CellText, CellRect);
end;

procedure TCustomColorForm.VTBasicListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
  if SelectedColorList <> TVirtualStringTree(Sender) then
    SelectedColorList := TVirtualStringTree(Sender);
  CBColors.Selected := TVirtualStringTree(Sender).CI[Node^.Index];
  btColors.ButtonColor := CBColors.Selected;
end;

procedure TCustomColorForm.VTBasicListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  CellText := TVirtualStringTree(Sender).CI.N[Node^.Index];
end;

procedure TCustomColorForm.VTBasicListPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  with TargetCanvas.Font do
  begin
    if Sender.Selected[Node] then
    begin
      if Sender.Focused then
        Color := BasicListColors[17]
      else
        Color := BasicListColors[18];
    end
    else
      Color := BasicListColors[16];
  end;
end;

procedure TCustomColorForm.DrawBoxColorText(const TargetCanvas: TCanvas; const BoxColor: TColor;
  const CellText: String; CellRect: TRect);
var
  ABoxRect: TRect;
  ATextRect: TRect;
begin
  with TargetCanvas do
  begin
    // box color rect
    ABoxRect := CellRect;
    ABoxRect.Inflate(-2, -2);
    ABoxRect.Left := CellRect.Left;
    ABoxRect.Width := ABoxRect.Height;
    // text rect
    ATextRect := CellRect;
    ATextRect.Left := ABoxRect.Right + 4;
    // box color
    Brush.Style := bsSolid;
    Pen.Color := clGray;
    Brush.Color := BoxColor;
    Rectangle(ABoxRect);
    // extra border
    Brush.Style := bsClear;
    Pen.Color := clWhite;
    ABoxRect.Inflate(-1, -1);
    Rectangle(ABoxRect);
    // text
    TextRect(ATextRect, ATextRect.Left, 0, CellText, TextStyleLeftCenter);
  end;
end;

procedure TCustomColorForm.SetSelectedColor(const AColor: TColor);
begin
  if (SelectedColorList = nil) or (SelectedColorList.FocusedNode = nil) then Exit;
  if SelectedColorList.CI[SelectedColorList.FocusedNode^.Index] = AColor then Exit;
  SelectedColorList.CI[SelectedColorList.FocusedNode^.Index] := AColor;
  if SelectedColorList = VTBasicList then
  begin
    ApplyBasicColorToVT(VTBasicList);
    ApplyBasicColorToVT(VTMangaList);
    ApplyBasicColorToVT(VTFavoriteList);
    ApplyBasicColorToVT(VTChapterList);
  end
  else
    SelectedColorList.Repaint;
end;

initialization
  DoInit;

finalization
  DoFinal;

end.
