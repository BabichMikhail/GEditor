unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, TypInfo,
  ExtCtrls, Buttons, StdCtrls, ComCtrls, DrawFigure, uDoublePoint, UChangeParamFigures,
  UAllTools, FPCanvas, USaveAndOpen, UCopyCutInsert, strutils, Math, ClipBrd, ActnList;

type

  func = function(a: integer): integer;

type

  { TGraEditor }

  TGraEditor = class(TForm)
    AddColor: TButton;
    ComboBox1: TComboBox;
    CopyFigure: TImage;
    CutFigure: TImage;
    DeleteFigure: TImage;
    DeleteFig: TMenuItem;
    AllFigures: TMenuItem;
    Figure1: TMenuItem;
    UndoFig: TMenuItem;
    RedoFig: TMenuItem;
    UpFigure: TImage;
    DownFigure: TImage;
    InsertFigure: TImage;
    OpenPicture: TImage;
    SavePicture: TImage;
    Save_As: TMenuItem;
    DeleteAllFig: TMenuItem;
    OpenFile: TMenuItem;
    InsertFig: TMenuItem;
    CutFig: TMenuItem;
    CopyFig: TMenuItem;
    SaveFile: TMenuItem;
    Correction: TMenuItem;
    EditScale: TEdit;
    GEditor_: TMainMenu;
    File_: TMenuItem;
    Help_: TMenuItem;
    Exit_: TMenuItem;
    About_: TMenuItem;
    BoxForDraw: TPaintBox;
    BrushColor: TLabel;
    AllocationToolNow: TImage;
    OpenDialog: TOpenDialog;
    PenColor: TLabel;
    LPenColor: TImage;
    PanelUnderPaintBox: TPanel;
    BottomPanelUnderScroll: TPanel;
    RightPanelForScroll: TPanel;
    AddPanelForScroll: TPanel;
    SaveDialog: TSaveDialog;
    ScalePanel: TPanel;
    BottomScroll: TScrollBar;
    RightScroll: TScrollBar;
    TopStandardPanel: TPanel;
    ScaleTrackBar: TTrackBar;
    RBrushColor: TImage;
    LeftStandardPanel: TPanel;
    BottomStandardPanel: TPanel;
    ColorDialog: TColorDialog;
    procedure About_Program(Sender: TObject);
    procedure AddColorOnPanel(Sender: TObject);
    procedure BottomScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure BoxForDrawResize(Sender: TObject);
    procedure BrushColorClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure MyBoxForDrawInvalidate(Sender: TObject);
    procedure CopySelectFigureClick(Sender: TObject);
    procedure ChangeEditNumber(Sender: TObject; var Key: char);
    procedure ChangeCaption;
    procedure ChangeScalePosition(Sender: TObject);
    procedure ChangeColor(Sender: TObject);
    procedure CreateGrEditor(Sender: TObject);
    procedure CreateFigureListInMenu;
    procedure OnClickItems(Sender: TObject);
    procedure CutSelectFigureClick(Sender: TObject);
    procedure DeleteAllFigClick(Sender: TObject);
    procedure DeleteFigClick(Sender: TObject);
    procedure DownFiguresClick(Sender: TObject);
    procedure InsertSelectFigureClick(Sender: TObject);
    procedure LColorDialog(Sender: TObject);
    procedure CloseProgram(Sender: TObject);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnTimer(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure PenColorClick(Sender: TObject);
    procedure RColorDialog(Sender: TObject);
    procedure RedoFigClick(Sender: TObject);
    procedure ReFreshMaxMinPoints;
    procedure RightScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure SaveAsClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure ToolClicks(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawPaintBox(Sender: TObject);
    procedure UndoFigClick(Sender: TObject);
    procedure UpFiguresClick(Sender: TObject);
    private
      AddColorPanel: array[1..18] of TImage;
      ColorPanel: array[1..18] of TImage;
      ToolImage: array of TImage;
      NumAddClrPanel, PicturePosInHist: integer;
      PenOrBrushColorChange: (ChPenColor, ChBrushColor);
      PictureAlreadySaveToFile: boolean;
    const
      ToolHints: array[0..10] of string = ('Переместить холст', 'Карандаш', 'Отрезок',
        'Прямоугольник', 'Эллипс', 'Ломаная', 'Правильный многоугольник',
        'Прямоугольник с закруглёнными краями', 'Текст',
        'Выделение и редактирование фигур', 'Спрей');
      StandardColors: array[1..18] of TColor = (clBlack ,clBlue, clGreen,
        clMaroon, cLRed, clOlive, clNavy, clPurple, clTeal, clGray, clSilver,
        clLime, clFuchsia, clMedGray, clYellow, clWhite, clAqua, clSkyBlue);
  end;

var
  GraEditor: TGraEditor;

implementation

{$R *.lfm}
{ TGraEditor }

procedure TGraEditor.About_Program(Sender: TObject);
begin
  ShowMessage('Б8103а, Бабич Михаил');
end;
procedure TGraEditor.AddColorOnPanel(Sender: TObject);
begin
  if not ColorDialog.Execute then
    Exit;
  if NumAddClrPanel <= High(AddColorPanel) then begin
    AddColorPanel[NumAddClrPanel].Canvas.Brush.Color := ColorDialog.Color;
    AddColorPanel[NumAddClrPanel].Canvas.Rectangle
      (0, 0, AddColorPanel[NumAddClrPanel].Canvas.Width,
      AddColorPanel[NumAddClrPanel].Canvas.Height);
    NumAddClrPanel +=  1;
    end
    else begin
      AddColorPanel[NumAddClrPanel].Canvas.Brush.Color := ColorDialog.Color;
      AddColorPanel[NumAddClrPanel].Canvas.Rectangle
        (0, 0, AddColorPanel[NumAddClrPanel].Canvas.Width,
        AddColorPanel[NumAddClrPanel].Canvas.Height);
      NumAddClrPanel := 1;
    end;
end;

procedure TGraEditor.BottomScrollScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  Offset.x := ScrollPos/Scale;
  ReFreshMaxMinPoints;
  SetOffsetOnScroll(BottomScroll, Offset.x, Offset.x + BoxForDraw.Width/Scale,
    MinDoublePoint.x, MaxDoublePoint.x);
  BoxForDraw.Invalidate;
end;

procedure TGraEditor.BoxForDrawResize(Sender: TObject);
var
  Min1, Min2: integer;
begin
  Min1 := BottomScroll.Min;
  Min2 := RightScroll.Min;
  ToolNow.OnClick(LeftStandardPanel);
  if BottomScroll.Position + BoxForDraw.Width > BottomScroll.Max then
    BottomScroll.Max := BottomScroll.Position + BoxForDraw.Width;
  if RightScroll.Position + BoxForDraw.Height > RightScroll.Max then
    RightScroll.Max := RightScroll.Position + BoxForDraw.Height;
  BottomScroll.PageSize := BoxForDraw.Width;
  RightScroll.PageSize := BoxForDraw.Height;
  BottomScroll.Min := Min(Round(MinDoublePoint.x), Min1);
  RightScroll.Min := Min(Round(MinDoublePoint.y), Min2);
  ReFreshMaxMinPoints;
  ChangeMaxMinScrollBarOnMove(BottomScroll, RightScroll, BoxForDraw.Width, BoxForDraw.Height);
  BoxForDraw.Invalidate;
end;

procedure TGraEditor.MyBoxForDrawInvalidate(Sender: TObject);
begin
  BoxForDraw.Invalidate;
end;

procedure TGraEditor.BrushColorClick(Sender: TObject);
begin
  PenOrBrushColorChange := ChBrushColor;
  BrushColor.Font.Style := [fsBold];
  PenColor.Font.Style := [];
end;

procedure TGraEditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  StringList: TStringList = nil;
  Answer: integer;
begin
  CanClose := True;
  if not PictureChange(PicturePosInHist) then begin
    Answer := MessageDlg('Вы хотите сохранить изменения?' , mtWarning, [mbYes, mbNo, mbCancel], 0);
    if Answer = mrYes then begin
      if FullFileName = '' then
        if not SaveDialog.Execute then
          Exit;
      if FullFileName = '' then
        FullFileName := SaveDialog.FileName;
      SaveMyFile(StringList, ArrayOfFigures);
      StringList.SaveToFile(FullFileName);
      CanClose := True;
    end;
    if Answer = mrCancel then
      CanClose := False;
    if Answer = mrNo then
      CanClose := True;
  end;
end;

procedure TGraEditor.CopySelectFigureClick(Sender: TObject);
var
  i: integer;
  StringList: TStringList;
begin
  FigureInMemory.Clear;
  if ArrayOfFigures <> nil then begin
    StringList := TStringList.Create;
    for i := 0 to High(ArrayOfFigures) do
      if ArrayOfFigures[i].FSelect then
        CopyFigures(StringList);
    FigureInMemory.AsText := StringList.Text;
  end;
  StringList.Free;
  BoxForDraw.Invalidate;
end;

procedure TGraEditor.ChangeEditNumber(Sender: TObject; var Key: char);
begin
  if not (key in ['0'..'9', ',', #8]) then
    key := #0;
end;

procedure TGraEditor.ChangeCaption;
var
  FileName: AnsiString;
begin
  FileName := FullFileName;
  Caption := Name;
  Delete(FileName, 1, RPos('\', FileName));
  if not (FileName = '') then
    FileName :=  ' - ' + FileName;
  if not (FullFileName = '') then
    Caption := Name + FileName;
  if not PictureChange(PicturePosInHist) then
    Caption := Name + FileName + '*';
end;

procedure TGraEditor.ChangeScalePosition(Sender: TObject);
var
  TBPosition: integer;
  OldScale: double;
begin
  OldScale := Scale;
  TBPosition := ScaleTrackBar.Position;
  if TBPosition = 0 then
    TBPosition := 1;
  if TBPosition > 0 then
    Scale := TBPosition
  else
  if TBPosition < 0 then
    Scale := Round(1/Abs(TBPosition)*10000)/10000;
  if Scale = OldScale then
    Exit;
  ReFreshMaxMinPoints;
  SetNewOffset(BoxForDraw.Width, BoxForDraw.Height, OldScale);
  ChangeMaxMinScrollBarOnScale(BottomScroll, RightScroll, OldScale, Scale,
    BoxForDraw.Width, BoxForDraw.Height);
  EditScale.Text := FloatToStr(Round(Scale*10000)/10000);
  BoxForDraw.Invalidate;
end;

procedure TGraEditor.CloseProgram(Sender: TObject);
begin
  Close;
end;

procedure TGraEditor.CreateGrEditor(Sender: TObject);
var
  i: byte;
begin
  randomize;
  XPred := Random(1000);
  Scale := 1;
  Offset := DoublePoint(0, 0);
  ToolNow := TPolylineTool;
  PenOrBrushColorChange := ChPenColor;
  BrushColor.Font.Style := [];
  NumAddClrPanel:= 1;
  BoxForDraw.Canvas.Brush.Color := clWhite;
  BoxForDraw.Canvas.Brush.Style:= bsSolid;
  BrushColorNow := clWhite;
  PenColor.Font.Style := [fsBold];
  InvalidatePaintBox := @MyBoxForDrawInvalidate;
  FullFileName := '';
  PanelPointer := @LeftStandardPanel;
  ChangeCapt := @ChangeCaption;
  ReFreshMaxMinPoints;
  with BoxForDraw.Canvas do
    begin
      Brush.Color := clWhite;
      Pen.Color := clBlack;
      Pen.Width := 1;
    end;
  with LPenColor.Canvas do begin
    Brush.Color := clBlack;
    Pen.Color := clBlack;
  end;
  LPenColor.Canvas.Rectangle(0, 0, LPenColor.Width, LPenColor.Height);
  with RBrushColor.Canvas do begin
    Brush.Color := clWhite;
    Pen.Color := clBlack;
  end;
  RBrushColor.Canvas.Rectangle(0, 0, RBrushColor.Width, RBrushColor.Height);
  for i := 1 to 18 do begin
    ColorPanel[i] := TImage.Create(BottomStandardPanel);
    with ColorPanel[i] do begin
      Parent := BottomStandardPanel;
      Width := 30;
      Height := 30;
      Color := StandardColors[i];
      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Color := StandardColors[i];
      Left := PanelUnderPaintBox.Left +  (i - 1)*30 + 5;
      Top := 10;
      Canvas.Rectangle(0, 0, 30, 30);
      OnClick := @ChangeColor;
    end;
    AddColorPanel[i] := TImage.Create(BottomStandardPanel);
    with AddColorPanel[i] do begin
      Parent := BottomStandardPanel;
      Width := 30;
      Height := 30;
      Color := clWhite;
      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Color := clWhite;
      Left := PanelUnderPaintBox.Left + (i - 1)*30 + 5;
      Top := 50;
      Canvas.Rectangle(0, 0, AddColorPanel[i].Width, AddColorPanel[i].Height);
      OnClick := @ChangeColor;
    end;
  end;
  with AllocationToolNow do begin
    Width := 41;
    Height := Width;
  end;
  with AllocationToolNow.Canvas do begin
    Pen.Width := 5;
    Pen.Color := clBlack;
    Brush.Color := $00E1C7C1;
    Brush.Style := bsSolid;
    Rectangle(0, 0, 41, 41);
  end;
  ToolTag := 1;
  for i := 0 to High(ToolClasses) do begin
    SetLength(ToolImage, Length(ToolImage) + 1);
    ToolImage[High(ToolImage)] := TImage.Create(LeftStandardPanel);
    with ToolImage[High(ToolImage)] do begin
      Parent := LeftStandardPanel;
      Left := 5 + i div 3 * 40;
      Top := 50*(i mod 3)+ 20;
      if i = 0 then begin
        Left := Left + 5;
        Top :=  Top + 5;
      end;
      Width := 35;
      Height := 35;
      Name := ToolClasses[i].ClassName;
      OnClick := @ToolClicks;
      Caption := '';
      Tag := i;
      Picture.LoadFromFile('Icons_Tools\' + ToolClasses[i].ClassName + 'Icon.png');
      ShowHint := True;
      Hint := ToolHints[i];
    end;
  end;
  with AllocationToolNow do begin
    Left := ToolImage[1].Left - 3;
    Top := ToolImage[1].Top - 3;
    Canvas.Brush.Style := bsClear;
  end;
  HistPos := -1;
  SetStrHist;
  PicturePosInHist := HistPos;
  Timer := TTimer.Create(Self);
  Timer.Enabled := False;
  OnMyTimer := @OnTimer;
  Timer.OnTimer := OnMyTimer;
  ToolNow.OnClick(LeftStandardPanel);
  FigureInMemory := TClipBoard.Create;
  Scale := 1;
  Offset := DoublePoint(0, 0);
  with BottomScroll do begin
    Max := BoxForDraw.Width;
    Min := -3;
    PageSize := Max;
  end;
  with RightScroll do begin
    Max := BoxForDraw.Height;
    Min := -3;
    PageSize := Max;
  end;
  CreateFigureListInMenu;
end;

procedure TGraEditor.CreateFigureListInMenu;
var
  NewItem: array of TMenuItem;
  i, Index: integer;
begin
  AllFigures.Clear;
  if Length(ArrayOfFigures) = 0 then begin
    SetLength(NewItem, 1);
    NewItem[0] := TMenuItem.Create(Self);
    NewItem[0].Caption := 'Нет фигур';
    NewItem[0].Name := 'Figure0';
    AllFigures.Add(NewItem);
    Exit;
  end;
  if Length(ArrayOfFigures) <= 10 then
    SetLength(NewItem, Length(ArrayOfFigures) + 1)
  else
    SetLength(NewItem, 11);
  NewItem[0] := TMenuItem.Create(Self);
  NewItem[0].Caption := 'Всего фигур - ' + IntToStr(Length(ArrayOfFigures));
  for i := High(ArrayOfFigures) + 1 downto  Max(High(ArrayOfFigures) - 9, 1) do begin
    Index := Length(ArrayOfFigures) - i + 1;
    NewItem[Index] := TMenuItem.Create(Self);
    NewItem[Index].Caption := IntToStr(i) + '. ' + ToolHints[GetIndexFigure(ArrayOfFigures[i - 1].ClassName)];
    NewItem[Index].OnClick := @OnClickItems;
    NewItem[Index].Tag := i - 1;
  end;
  AllFigures.Add(NewItem);
end;

procedure TGraEditor.OnClickItems(Sender: TObject);
var Index: integer;
begin
  InvisMethod.DeselectFigures;
  Index := (Sender as TMenuItem).Tag;
  ArrayOfFigures[Index].FSelect := True;
  ToolNow := TControlTool;
  ToolTag := 9;
  GetToolClass(ArrayOfFigures[Index]).OnClick(LeftStandardPanel);
  isMouseDown := False;
  with AllocationToolNow do begin
    Left := ToolImage[ToolTag].Left - 3;
    Top := ToolImage[ToolTag].Top - 3;
  end;
  Timer.Enabled := True;
  Timer.Interval := 100;
  BoxForDraw.Invalidate;
  NewFigure := True;
end;

procedure TGraEditor.CutSelectFigureClick(Sender: TObject);
var
  i: integer;
  StringList: TStringList;
begin
  FigureInMemory.Clear;
  if ArrayOfFigures <> nil then begin
    StringList := TStringList.Create;
    for i := 0 to High(ArrayOfFigures) do
      if ArrayOfFigures[i].FSelect then
        CopyFigures(StringList);
    FigureInMemory.AsText := StringList.Text;
    StringList.Free;
  end;
  i := 0;
  if ArrayOfFigures <> nil then
    while i <= High(ArrayOfFigures) do begin
      if ArrayOfFigures[i].FSelect then begin
        ArrayOfFigures[i].FSelect := False;
        DeleteFigures(i);
        i -= 1;
      end;
      i += 1;
    end;
  SetStrHist;
  CreateFigureListInMenu;
  ReFreshMaxMinPoints;
  ChangeMaxMinScrollBarOnMove(BottomScroll, RightScroll, BoxForDraw.Width,
    BoxForDraw.Height);
end;

procedure TGraEditor.DeleteAllFigClick(Sender: TObject);
var
  i: integer;
begin
  i := 0;
  if ArrayOfFigures <> nil then
    while i <= High(ArrayOfFigures) do
      DeleteFigures(i);
  SetStrHist;
  CreateFigureListInMenu;
  ReFreshMaxMinPoints;
  ChangeMaxMinScrollBarOnMove(BottomScroll, RightScroll, BoxForDraw.Width,
    BoxForDraw.Height);
end;

procedure TGraEditor.DeleteFigClick(Sender: TObject);
var
  i: integer;
begin
  i := 0;
  if ArrayOfFigures <> nil then
    while i <= High(ArrayOfFigures) do begin
      if ArrayOFFigures[i].FSelect then begin
        DeleteFigures(i);
        i -= 1;
      end;
      i += 1;
    end;
  SetStrHist;
  CreateFigureListInMenu;
  ReFreshMaxMinPoints;
  ChangeMaxMinScrollBarOnMove(BottomScroll, RightScroll, BoxForDraw.Width,
    BoxForDraw.Height);
end;

procedure TGraEditor.DownFiguresClick(Sender: TObject);
var
  i: integer;
begin
  if ArrayOfFigures <> nil then
    for i := 1 to High(ArrayOfFigures) do
      if ArrayOfFigures[i].FSelect and not (ArrayOfFigures[i - 1].FSelect)
      then
        SwapFigures(ArrayOfFigures[i], ArrayOfFigures[i - 1]);
  SetStrHist;
  ChangeCaption;
  CreateFigureListInMenu;
end;

procedure TGraEditor.InsertSelectFigureClick(Sender: TObject);
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  StringList.Text := FigureInMemory.AsText;
  DeselectFigures;
  OpenMyFile(StringList);
  StringList.Free;
  ReFreshMaxMinPoints;
  ChangeMaxMinScrollBarOnMove(BottomScroll, RightScroll, BoxForDraw.Width,
    BoxForDraw.Height);
end;

procedure TGraEditor.LColorDialog(Sender: TObject);
begin
  if not ColorDialog.execute then
    Exit;
  LPenColor.Color := ColorDialog.Color;
  BoxForDraw.Canvas.Pen.Color := ColorDialog.Color;
  LPenColor.Canvas.Brush.Color := ColorDialog.Color;
  PenColorNow := ColorDialog.Color;
  LPenColor.Canvas.Rectangle(0, 0, LPenColor.Width, LPenColor.Height);
  ChangePenColorSelectFigures(PenColorNow);
  SetStrHist;
end;

procedure TGraEditor.ChangeColor(Sender: TObject);
begin
  if PenOrBrushColorChange = ChPenColor then begin
    BoxForDraw.Canvas.Pen.Color := (Sender as TImage).Canvas.Brush.Color;
    LPenColor.Canvas.Brush.Color := (Sender as TImage).Canvas.Brush.Color;
    LPenColor.Canvas.Rectangle(0, 0, LPenColor.Width, LPenColor.Height);
    PenColorNow := (Sender as TImage).Canvas.Brush.Color;
    ChangePenColorSelectFigures(PenColorNow);
  end
  else begin
    BoxForDraw.Canvas.Brush.Color := (Sender as TImage).Canvas.Brush.Color;
    RBrushColor.Canvas.Brush.Color := (Sender as TImage).Canvas.Brush.Color;
    RBrushColor.Canvas.Rectangle(0, 0, RBrushColor.Width, RBrushColor.Height);
    BrushColorNow := (Sender as TImage).Canvas.Brush.Color;
    ChangeBrushColorSelectFigures(BrushColorNow);
  end;
  SetStrHist;
end;

procedure TGraEditor.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LastPoint := Point(x, y);
  ToolNow.MouseDown(ScreenToWorld(LastPoint));
  BoxForDraw.Invalidate;
end;

procedure TGraEditor.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  LastPoint := Point(x, y);
  ToolNow.MouseMove(ScreenToWorld(LastPoint));
  if isMouseDown then begin
    ReFreshMaxMinPoints;
    ChangeMaxMinScrollBarOnMove(BottomScroll, RightScroll, BoxForDraw.Width,
      BoxForDraw.Height);
  end;
  BoxForDraw.Invalidate;
end;

procedure TGraEditor.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LastPoint := Point(x, y);
  ToolNow.MouseUp;
  SetStrHist;
  CreateFigureListInMenu;
end;

procedure TGraEditor.OnTimer(Sender: TObject);
begin
  Inc(Time);
  isTimer := True;
  ToolNow.MouseMove(ScreenToWorld(LastPoint));
  BoxForDraw.Invalidate;
end;

procedure TGraEditor.OpenClick(Sender: TObject);
var
  StringList: TStringList;
begin
  if not OpenDialog.Execute then
    Exit;
  SetLength(ArrayOfFigures, 0);
  FullFilename := OpenDialog.FileName;
  StringList := TStringList.Create;
  StringList.LoadFromFile(FullFileName);
  OpenMyFile(StringList);
  StringList.Free;
  BoxForDraw.Invalidate;
  SetLength(History, 0);
  HistPos := -1;
  PictureAlreadySaveToFile := True;
  PicturePosInHist := 0;
  DeselectFigures;
  SetStrHist;
  CreateFigureListInMenu;
  ReFreshMaxMinPoints;
  ChangeMaxMinScrollBarOnMove(BottomScroll, RightScroll, BoxForDraw.Width,
    BoxForDraw.Height);
end;

procedure TGraEditor.PenColorClick(Sender: TObject);
begin
  PenOrBrushColorChange := ChPenColor;
  PenColor.Font.Style := [fsBold];
  BrushColor.Font.Style := [];
end;

procedure TGraEditor.RColorDialog(Sender: TObject);
begin
  if not ColorDialog.execute then
    Exit;
  RBrushColor.Color := ColorDialog.Color;
  RBrushColor.Canvas.Brush.Color := ColorDialog.Color;
  BoxForDraw.Canvas.Brush.Color := ColorDialog.Color;
  BrushColorNow := ColorDialog.Color;
  RBrushColor.Canvas.Rectangle(0, 0, RBrushColor.Width, RBrushColor.Height);
  ChangeBrushColorSelectFigures(BrushColorNow);
  SetStrHist;
end;

procedure TGraEditor.RedoFigClick(Sender: TObject);
begin
  HistPos += 1;
  GetStrHist;
  ChangeCaption;
  DeselectFigures;
  CreateFigureListInMenu;
  ReFreshMaxMinPoints;
  ChangeMaxMinScrollBarOnMove(BottomScroll, RightScroll, BoxForDraw.Width,
    BoxForDraw.Height);
end;

procedure TGraEditor.ReFreshMaxMinPoints;
var
  i: integer;
begin
  MaxDoublePoint := DoublePoint(-9E18, -9E18);
  MinDoublePoint := DoublePoint(9E18, 9E18);
  for i := 0 to High(ArrayOfFigures) do
    ArrayOfFigures[i].GetMaxAndMinPoints;
end;

procedure TGraEditor.RightScrollScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  Offset.y := RightScroll.Position/Scale;
  ReFreshMaxMinPoints;
  SetOffsetOnScroll(RightScroll, Offset.y, Offset.y + BoxForDraw.Height/Scale,
    MinDoublePoint.y, MaxDoublePoint.y);
  BoxForDraw.Invalidate;
end;

procedure TGraEditor.SaveAsClick(Sender: TObject);
var
  StringList: TStringList = nil;
begin
  PictureSaveInThisProc := False;
  if not SaveDialog.Execute then
    Exit;
  ReFreshMaxMinPoints;
  FullFileName := SaveDialog.FileName;
  SaveMyFile(StringList, ArrayOfFigures);
  if not PictureSaveInThisProc then
    StringList.SaveToFile(UTF8ToSys(FullFileName));
  PictureAlreadySaveToFile := True;
  StringList.Free;
  PicturePosInHist := HistPos;
  ChangeCaption;
end;

procedure TGraEditor.SaveClick(Sender: TObject);
var
  StringList: TStringList = nil;
begin
  PictureSaveInThisProc := False;
  if PictureAlreadySaveToFile then begin
    ReFreshMaxMinPoints;
    SaveMyFile(StringList, ArrayOfFigures);
    if not PictureSaveInThisProc then
      StringList.SaveToFile(UTF8ToSys(FullFileName));
    StringList.Free;
    PicturePosInHist := HistPos;
    ChangeCaption;
  end
  else begin
    if not SaveDialog.Execute then
      Exit;
    ReFreshMaxMinPoints;
    FullFileName := SaveDialog.FileName;
    SaveMyFile(StringList, ArrayOfFigures);
    if not PictureSaveInThisProc then
      StringList.SaveToFile(UTF8ToSys(FullFileName));
    StringList.Free;
    PictureAlreadySaveToFile := True;
    PicturePosInHist := HistPos;
    ChangeCaption;
  end;
end;

procedure TGraEditor.ToolClicks(Sender: TObject);
begin
  InvisMethod.DeselectFigures;
  Timer.Enabled := False;
  ToolTag := (Sender as TImage).Tag;
  ToolNow := ToolClasses[ToolTag];
  ToolNow.OnClick(LeftStandardPanel);
  isMouseDown := False;
  with AllocationToolNow do begin
    Left := ToolImage[ToolTag].Left - 3;
    Top := ToolImage[ToolTag].Top - 3;
    if ToolTag = 0 then begin
      Left := Left - 5;
      Top := Top - 5;
    end;
  end;
  BoxForDraw.Invalidate;
  NewFigure := True;
end;

procedure TGraEditor.DrawPaintBox(Sender: TObject);
  procedure GetNormPoint(var P1, P2: TPoint);
  var
    P0: TPoint;
  begin
    P0 := Point(P1.x, P1.y);
    P1 := Point(min(P0.x, P2.x), min(P0.y, P2.y));
    P2 := Point(max(P0.x, P2.x), max(P0.y, P2.y));
  end;
var
  Point1, Point2: TPoint;
  i, k: integer;
begin
  with BoxForDraw.Canvas do begin
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    Pen.Color := clWhite;
    Pen.Style := psSolid;
    Rectangle(0, 0, BoxForDraw.Width, BoxForDraw.Height);
  end;
  for i := 0 to High(ArrayOfFigures) do
    ArrayOfFigures[i].Draw(BoxForDraw.Canvas);
  for i := 0 to High(ArrayOfFigures) do
    if ArrayOfFigures[i].FSelect then
      ArrayOfFigures[i].DrawAnchors(BoxForDraw.Canvas);
  if (ToolNow = TControlTool) and isMouseDown then begin
    Point1 := WorldToScreen(InvisMethod.FPoint1);
    Point2 := WorldToScreen(InvisMethod.FPoint2);
    GetNormPoint(Point1, Point2);
    with BoxForDraw.Canvas do begin
      Pen.Width := 1;
      Pen.Color := clRed;
      Brush.Style := bsClear;
      Pen.Style := psDash;
      k := Time mod 24;
      if (abs(Point1.x - Point2.x) < 24) or (abs(Point1.y - Point2.y) < 24) then
        k := Time mod 12;
      if (abs(Point1.x - Point2.x) < 12) or (abs(Point1.y - Point2.y) < 12) then
        k := Time mod 6;
       if (Point1.x = Point2.x) and (Point1.y = Point2.y) then
        k := 0;
      Line(Point1.x + k, Point1.y, Point2.x, Point1.y);
      Line(Point2.x, Point1.y + k, Point2.x, Point2.y);
      Line(Point2.x - k, Point2.y, Point1.x, Point2.y);
      Line(Point1.x, Point2.y - k, Point1.x, Point1.y);
      if k > 6 then begin
        k -= 6;
        Line(Point1.x, Point1.y, Point1.x + k, Point1.y);
        Line(Point2.x, Point1.y, Point2.x, Point1.y + k);
        Line(Point2.x, Point2.y, Point2.x - k, Point2.y);
        Line(Point1.x, Point2.y, Point1.x, Point2.y - k);
      end;
    end;
  end;
end;

procedure TGraEditor.UndoFigClick(Sender: TObject);
begin
  HistPos -= 1;
  GetStrHist;
  ChangeCaption;
  DeselectFigures;
  CreateFigureListInMenu;
  ReFreshMaxMinPoints;
  ChangeMaxMinScrollBarOnMove(BottomScroll, RightScroll, BoxForDraw.Width,
    BoxForDraw.Height);
end;

procedure TGraEditor.UpFiguresClick(Sender: TObject);
var
  i: integer = 0;
begin
  if ArrayOfFigures <> nil then
    for i := High(ArrayOfFigures) - 1 downto 0 do
      if ArrayOfFigures[i].FSelect and not (ArrayOfFigures[i + 1].FSelect) then
        SwapFigures(ArrayOfFigures[i + 1], ArrayOfFigures[i]);
  SetStrHist;
  CreateFigureListInMenu;
end;

end.

