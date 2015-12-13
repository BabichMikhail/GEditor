unit UMyComCtrlsForTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, ComCtrls, StdCtrls, TypInfo, FPCanvas, DrawFigure, USaveAndOpen, LCLType;

type
  SuperVariant = record
    PropNumber: variant;
    isChange: boolean;
  end;

  Styles = record
    IntStyle: integer;
    isChange: boolean;
  end;

type
  TMyComponents = class(TComponent)
  private
    function SetParamNow(APropName: string): SuperVariant;
    function SetStyleNow(APropName: string): Styles;
    procedure ChangeParam(PropName: string; PropNumber: variant);
    procedure PressKey(Sender: TObject; var key: char);
  end;

  TWidthTrackBar = class(TMyComponents)
  private
    PenStyleComboBox: TComboBox;
    WidthTrack: TTrackBar;
    WidthEqualLabel, WidthNumLabel, NamePanelLabel: TLabel;
    procedure OnChangeWidth(Sender: TObject);
    procedure OnChangePenStyle(Sender: TObject);
    procedure OnMyItedDraw(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    constructor CreateComponent(APanel: TPanel; ATop: integer);
  end;

  TNumberOfCornerTrackBar = class(TMyComponents)
  private
    NumberOfCorner: TTrackBar;
    CornerEqualLabel, CornerNumLabel, NamePanelLabel: TLabel;
    procedure OnChangeCorner(Sender: TObject);
  public
    constructor CreateComponent(APanel: TPanel; ATop: integer);
  end;

  TRoundPanel = class(TMyComponents)
  private
    RoundXLabel, RoundYLabel: TLabel;
    RoundXEdit, RoundYEdit: TEdit;
    procedure OnChangeRoundX(Sender: TObject);
    procedure OnChangeRoundY(Sender: TObject);
  public
    constructor CreateComponent(APanel: TPanel; ATop: integer);
  end;

  TTextPanel = class(TMyComponents)
  private
    EditText: TEdit;
    IndentXLabel, IndentYLabel, SizeLabel: TLabel;
    EditIndentX, EditIndentY, EditSize: TEdit;
    procedure OnChangeText(Sender: TObject);
    procedure OnChangeIndentX(Sender: TObject);
    procedure OnChangeIndentY(Sender: TObject);
    procedure OnChangeSize(Sender: TObject);
  public
    constructor CreateComponent(APanel: TPanel; ATop: integer);
  end;

  TBrushStylePanel = class(TMyComponents)
  private
    BrushStyleComboBox: TComboBox;
    procedure OnChangeBrushStyle(Sender: TObject);
    procedure OnMyDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    constructor CreateComponent(APanel: TPanel; ATop: integer);
  end;

  TControlPanel = class(TMyComponents)
  private
    Deselect: TButton;
    procedure OnClickDeselect(Sender: TObject);
  public
    constructor CreateComponent(APanel: TPanel; ATop: integer);
  end;

  TSprayPanel = class(TMyComponents)
  private
    RadLabel: TLabel;
    RadPanel: TEdit;
    procedure OnChangeRadius(Sender: TObject);
  public
    constructor CreateComponent(APanel: TPanel; ATop: integer);
  end;

var
  WidthNow: integer = 1;
  NCornerNow: integer = 3;
  IndentXNow: integer = 0;
  IndentYNow: integer = 0;
  RoundXNow: integer = 30;
  RoundYNow: integer = 30;
  SizeNow: integer = 12;
  BrushStyleNow: TBrushStyle = bsClear;
  TextNow: string = 'Текст';
  IntervalNow: integer = 100;
  RadiusNow: integer = 50;
  PenStyleNow: TPenStyle;
  SizePointNow: integer = 2;

const
  //ArrayOfBrushStyleItems: array[0..7] of string = ('bsSolid', 'bsClear', 'bsHorizontal',
  //  'bsVertical', 'bsFDiagonal', 'bsFDiagonal', 'bsCross', 'bsDiagCross');
  BrushStyleList: array [0..7] of TBrushStyle = (bsSolid, bsClear, bsHorizontal,
    bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);
  //ArrayOfPenStyleItems: array[0..7] of string = ('psSolid', 'psDash', 'psDot',
  //  'psDashDot', 'psDashDotDot', 'psInsideFrame', 'psPattern', 'psClear');
  PenStyleList: array[0..4] of TPenStyle = (psSolid, psDash, psDot, psDashDot,
    psDashDotDot);


implementation

{TMyComCtrls}
procedure TMyComponents.PressKey(Sender: TObject; var key: char);
begin
  if not (key in ['0'..'9', ',', #8]) then
    key := #0;
end;

function TMyComponents.SetParamNow(APropName: string): SuperVariant;
var
  i: integer;
begin
  Result.isChange := False;
  if ArrayOfFigures <> nil then
    for i := 0 to High(ArrayOfFigures) do
      if ArrayOfFigures[i].FSelect then begin
        Result.PropNumber := GetPropValue(ArrayOfFigures[i], APropName);
        Result.isChange := True;
      end;
end;

procedure TMyComponents.ChangeParam(PropName: string; PropNumber: variant);
var
  i: integer;
begin
  if ArrayOfFigures <> nil then
    for i := 0 to High(ArrayOfFigures) do
      if ArrayOfFigures[i].FSelect then
        SetPropValue(ArrayOfFigures[i], PropName, PropNumber);
  SetStrHist;
end;

function TMyComponents.SetStyleNow(APropName: string): Styles;

var
  i: integer;
begin
  Result.isChange := False;
  if ArrayOfFigures <> nil then
    for i := 0 to High(ArrayOfFigures) do
      if ArrayOfFigures[i].FSelect then begin
        Result.IntStyle := Integer(GetOrdProp(ArrayOfFigures[i], APropName));
        Result.isChange := True;
      end;
end;

{TWidthPanel}
constructor TWidthTrackBar.CreateComponent(APanel: TPanel; ATop: integer);
var
  i: integer;
begin
  if SetParamNow('Width').isChange then
    WidthNow := SetParamNow('Width').PropNumber;
  WidthTrack := TTrackBar.Create(Self);
  with WidthTrack do begin
    Parent := APanel;
    Height := 25;
    Width := 124;
    Top := ATop - 40;
    Left := 19;
    Max := 50;
    Min := 1;
    Position := WidthNow;
    OnChange := @OnChangeWidth;
  end;
  WidthEqualLabel := TLabel.Create(Self);
  with WidthEqualLabel do begin
    Parent := APanel;
    Top := WidthTrack.Top - 20;
    Left := 22;
    Caption := 'Толщина линий =';
  end;
  WidthNumLabel := TLabel.Create(Self);
  with WidthNumLabel do begin
    Parent := APanel;
    Top := WidthEqualLabel.Top;
    Left := 127;
    Caption := IntToStr(WidthNow);
  end;
  NamePanelLabel := TLabel.Create(Self);
  with NamePanelLabel do begin
    Parent := APanel;
    Top := WidthTrack.Top + 22;
    Left := 27;
    Caption := '1                               50';
  end;
  if SetStyleNow('PenStyle').isChange then
    PenStyleNow := PenStyleList[SetStyleNow('PenStyle').IntStyle];
  PenStyleComboBox := TComboBox.Create(Self);
  with PenStyleComboBox do begin
    Parent := APanel;
    Style := csOwnerDrawFixed;
    Top := ATop;
    Left := 31;
    for i := 0 to High(PenStyleList) do
      Items.Add('');
    for i := 0 to High(PenStyleList) do
      if PenStyleNow = PenStyleList[i] then
        PenStyleComboBox.ItemIndex := i;
    OnChange := @OnChangePenStyle;
    OnDrawItem := @OnMyItedDraw;
    ReadOnly := True;
    Width := 100;
    ItemHeight := 26;
  end;
end;

procedure TWidthTrackBar.OnMyItedDraw(Control: TWinControl; Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
begin
  with PenStyleComboBox.Canvas do begin
    Brush.Style := bsClear;
    Pen.Style := psSolid;
    Rectangle(ARect);
    Pen.Style := PenStyleList[Index];
    Pen.Width := 2;
    Line(ARect);
  end;
end;

procedure TWidthTrackBar.OnChangePenStyle(Sender: TObject);
begin
  PenStyleNow := PenStyleList[PenStyleComboBox.ItemIndex];
  ChangeParam('PenStyle', integer(PenStyleNow));
end;

procedure TWidthTrackBar.OnChangeWidth(Sender: TObject);
begin
  WidthNow := WidthTrack.Position;
  WidthNumLabel.Caption := IntToStr(WidthNow);
  ChangeParam('Width', WidthNow);
end;

{TNumberOfCornerPanel}
constructor TNumberOfCornerTrackBar.CreateComponent(APanel: TPanel; ATop: integer);
begin
  if SetParamNow('NCorner').isChange then
    NCornerNow := SetParamNow('NCorner').PropNumber;
  NumberOfCorner := TTrackBar.Create(Self);
  with NumberOfCorner do begin
    Parent := APanel;
    Height := 25;
    Width := 124;
    Top := ATop;
    Left := 19;
    Max := 50;
    Min := 3;
    Position := NCornerNow;
    OnChange := @OnChangeCorner;
  end;
  CornerEqualLabel := TLabel.Create(Self);
  with CornerEqualLabel do begin
    Parent := APanel;
    Top := NumberOfCorner.Top - 20;
    Left := 19;
    Caption := 'Количество углов =';
  end;
  CornerNumLabel := TLabel.Create(Self);
  with CornerNumLabel do begin
    Parent := APanel;
    Top := CornerEqualLabel.Top;
    Left := 132;
    Caption := IntToStr(NCornerNow);
  end;
  NamePanelLabel := TLabel.Create(Self);
  with NamePanelLabel do begin
    Parent := APanel;
    Top := NumberOfCorner.Top + 22;
    Left := 25;
    Caption := '3                               50';
  end;
end;

procedure TNumberOfCornerTrackBar.OnChangeCorner(Sender: TObject);
begin
  NCornerNow := NumberOfCorner.Position;
  CornerNumLabel.Caption := IntToStr(NCornerNow);
  ChangeParam('NCorner', NCornerNow);
end;

{TRoundPanel}
constructor TRoundPanel.CreateComponent(APanel: TPanel; ATop: integer);
begin
  RoundYLabel := TLabel.Create(Self);
  with RoundYLabel do begin
    Parent := APanel;
    ParentColor := True ;
    ParentFont := True;
    Height := 16;
    Left := 19;
    Top := ATop;
    Caption := 'Закругл. Oy =';
  end;
  RoundXLabel := TLabel.Create(Self);
  with RoundXLabel do begin
    Parent := APanel;
    ParentColor := True;
    ParentFont := True;
    Height := 16;
    Left := 19;
    Top := RoundYLabel.Top - 30;
    Caption := 'Загругл. Ox =';
  end;
  if SetParamNow('RoundX').isChange then
    RoundXNow := SetParamNow('RoundX').PropNumber;
  RoundXEdit := TEdit.Create(Self);
  with RoundXEdit do begin
    Parent := APanel;
    Color := clWhite;
    ParentFont := True;
    Height := 23;
    Left := 98;
    Top := RoundXLabel.Top - 4;
    Text := IntToStr(RoundXNow);
    Width := 50;
    Alignment := taCenter;
    OnChange := @OnChangeRoundX;
    OnKeyPress := @PressKey;
  end;
  if SetParamNow('RoundY').isChange then
    RoundYNow := SetParamNow('RoundY').PropNumber;
  RoundYEdit := TEdit.Create(Self);
  with RoundYEdit do begin
    Parent := APanel;
    Color := clWhite;
    ParentFont := True;
    Height := 23;
    Left := 98;
    Top := RoundYLabel.Top - 4;
    Text := IntToStr(RoundYNow);
    Width := 50;
    Alignment := taCenter;
    OnChange := @OnChangeRoundY;
    OnKeyPress := @PressKey;
  end;
end;

procedure TRoundPanel.OnChangeRoundX(Sender: TObject);
begin
  if (RoundXEdit.Text = '') or (StrToInt(RoundXEdit.Text) = 0) then
    RoundXEdit.Text := '10';
  RoundXNow := StrToInt(RoundXEdit.Text);
  ChangeParam('RoundX', RoundXNow);
end;

procedure TRoundPanel.OnChangeRoundY(Sender: TObject);
begin
  if (RoundYEdit.Text = '') or (StrToInt(RoundYEdit.Text) = 0) then
    RoundYEdit.Text := '10';
  RoundYNow := StrToInt(RoundYEdit.Text);
  ChangeParam('RoundY', RoundYNow);
end;

{TTextPanel}
constructor TTextPanel.CreateComponent(APanel: TPanel; ATop: integer);
begin
  IndentXLabel := TLabel.Create(Self);
  with IndentXLabel do begin
    Parent := APanel;
    ParentColor := True;
    ParentFont := True;
    Left := 22;
    Top := ATop;
    Caption := 'Отступ Ox =';
  end;
  IndentYLabel := TLabel.Create(Self);
  with IndentYLabel do begin
    Parent := APanel;
    ParentColor := True;
    ParentFont := True;
    Left := 22;
    Top := IndentXLabel.Top + 30;
    Caption := 'Отступ Oy =';
  end;
  SizeLabel := TLabel.Create(Self);
  with SizeLabel do begin
    Parent := APanel;
    ParentColor := True;
    ParentFont := True;
    Left := 22;
    Top := IndentXLabel.Top + 60;
    Caption := 'Шрифт       =';
  end;
  if SetParamNow('IndentX').isChange then
    IndentXNow := SetParamNow('IndentX').PropNumber;
  EditIndentX := TEdit.Create(Self);
  with EditIndentX do begin
    Parent := APanel;
    Left := 93;
    Top := IndentXLabel.Top - 4;
    Text := IntToStr(IndentXNow);
    Width := 50;
    Height := 20;
    OnChange := @OnChangeIndentX;
    OnKeyPress := @PressKey;
    Alignment := taCenter;
  end;
  if SetParamNow('IndentY').isChange then
    IndentYNow := SetParamNow('IndentY').PropNumber;
  EditIndentY := TEdit.Create(Self);
  with EditIndentY do begin
    Parent := APanel;
    Left := 93;
    Top := IndentYLabel.Top - 4;
    Text := IntToStr(IndentYNow);
    Width := 50;
    Height := 20;
    OnChange := @OnChangeIndentY;
    OnKeyPress := @PressKey;
    Alignment := taCenter;
  end;
  if SetParamNow('Size').isChange then
    SizeNow := SetParamNow('Size').PropNumber;
  EditSize := TEdit.Create(Self);
  with EditSize do begin
    Parent := APanel;
    Color := clWhite;
    ParentFont := True;
    Left := 93;
    Top := IndentXLabel.Top + 56;
    Text := IntToStr(SizeNow);
    Width := 50;
    Height := 20;
    OnChange := @OnChangeSize;
    OnKeyPress := @PressKey;
    Alignment := taCenter;
  end;
  if SetParamNow('Text').isChange then
    TextNow := SetParamNow('Text').PropNumber;
  EditText := TEdit.Create(Self);
   with EditText do begin
    Parent := APanel;
    Color := clWhite;
    ParentFont := True;
    Left := 39;
    Top := IndentXLabel.Top - 34;
    Name := 'EditText';
    Text := TextNow;
    Width := 90;
    Height := 20;
    OnChange := @OnChangeText;
  end;
end;

procedure TTextPanel.OnChangeText(Sender: TObject);
begin
  TextNow := EditText.Text;
  ChangeParam('Text', TextNow);
end;

procedure TTextPanel.OnChangeIndentX(Sender: TObject);
begin
  if EditIndentX.Text = '' then
    EditIndentX.Text := '0';
  IndentXNow := StrToInt(EditIndentX.Text);
  EditIndentX.Text := FloatToStr(StrToFloat(EditIndentX.Text));
  ChangeParam('IndentX', IndentXNow);
end;

procedure TTextPanel.OnChangeIndentY(Sender: TObject);
begin
  if EditIndentY.Text = '' then
    EditIndentY.Text := '0';
  IndentYNow := StrToInt(EditIndentY.Text);
  EditIndentY.Text := FloatToStr(StrToFloat(EditIndentY.Text));
  ChangeParam('IndentY', IndentYNow);
end;

procedure TTextPanel.OnChangeSize(Sender: TObject);
begin
  if (EditSize.Text = '') or (StrToInt(EditSize.Text) = 0) then
    EditSize.Text := '1';
  SizeNow := StrToInt(EditSize.Text);
  EditSize.Text := FloatToStr(StrToFloat(EditSize.Text));
  ChangeParam('Size', SizeNow);
end;

{TBrushPanel}
constructor TBrushStylePanel.CreateComponent(APanel: TPanel; ATop: integer);
var
  i: integer;
begin
  BrushStyleNow := bsClear;
  BrushStyleComboBox := TComboBox.Create(Self);
  with BrushStyleComboBox do begin
    Parent := APanel;
    Top := ATop;
    Left := 31;
    Width := 100;
    for i := 0 to High(BrushStyleList) do
      Items.Add('');
    ItemIndex := 1;
    if SetStyleNow('BrushStyle').isChange then begin
      BrushStyleNow := BrushStyleList[SetStyleNow('BrushStyle').IntStyle];
      ItemIndex := SetStyleNow('BrushStyle').IntStyle;
    end;
    OnChange := @OnChangeBrushStyle;
    Style := csOwnerDrawFixed;
    OnDrawItem := @OnMyDrawItem;
    ReadOnly := True;
    ItemHeight := 26;
  end;
end;

procedure TBrushStylePanel.OnMyDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with BrushStyleComboBox.Canvas do begin
    Brush.Color := clWhite;
    FillRect(ARect);
    Brush.Style := BrushStyleList[Index];
    Brush.Color := $409F00;
    Rectangle(ARect);
  end;
end;

procedure TBrushStylePanel.OnChangeBrushStyle(Sender: TObject);
begin
  BrushStyleNow := BrushStyleList[BrushStyleComboBox.ItemIndex];
  ChangeParam('BrushStyle', integer(BrushStyleNow));
end;

constructor TControlPanel.CreateComponent(APanel: TPanel; ATop: integer);
begin
  Deselect := TButton.Create(Self);
  with Deselect do begin
    Parent := APanel;
    Top := ATop;
    Left := 17;
    Height := 40;
    Width := 130;
    Caption := 'Сбросить выделения';
    OnClick := @OnClickDeselect;
  end;
end;

procedure TControlPanel.OnClickDeselect(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(ArrayOfFigures) do
    ArrayOfFigures[i].FSelect := False;
  InvalidatePaintBox(nil);
end;

{SprayPanel}
constructor TSprayPanel.CreateComponent(APanel: TPanel; ATop: integer);
begin
  if SetParamNow('Radius').isChange then
    RadiusNow := SetParamNow('Radius').PropNumber;
  RadPanel := TEdit.Create(Self);
  with RadPanel do begin
    Parent := APanel;
    Color := clDefault;
    Top := ATop + 30;
    Left := 80;
    Width := 50;
    Text := IntToStr(RadiusNow);
    OnChange := @OnChangeRadius;
    OnKeyPress := @PressKey;
    Alignment := taCenter;
  end;
  RadLabel := TLabel.Create(Self);
  with RadLabel do begin
    Parent := APanel;
    ParentFont := True;
    Top := RadPanel.Top + 3;
    Left := 10;
    Width := 50;
    Caption := 'Радиус (px)';
  end;
end;

procedure TSprayPanel.OnChangeRadius(Sender: TObject);
begin
  if (RadPanel.Text = '') or (StrToInt(RadPanel.Text) = 0)then
    RadPanel.Text := '50';
  RadiusNow := StrToInt(RadPanel.Text);
  ChangeParam('Radius', RadiusNow);
end;

end.
