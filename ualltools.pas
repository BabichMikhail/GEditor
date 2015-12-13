unit UAllTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDoublePoint, DrawFigure, Forms, Graphics, ExtCtrls,
  TypInfo, FPCanvas, UMyComCtrlsForTools, UInvisbleMethods;

type
  TAllTools = class(TObject){TAllTools}
  public
    class procedure OnClick(APanel: TPanel); virtual;
    class procedure MouseDown(Point: TDoublePoint); virtual; abstract;
    class procedure MouseMove(Point: TDoublePoint); virtual; abstract;
    class procedure MouseUp; virtual; abstract;
  end;
  TToolClass = class of TAllTools;

  TInvisibleTool = class(TAllTools){TInvisibleTool}
  end;

  THandTool = class(TInvisibleTool){TStandTool}
  public
    class procedure MouseDown(Point: TDoublePoint); override;
    class procedure MouseMove(Point: TDoublePoint); override;
    class procedure MouseUp; override;
  end;

  TControlTool = class(TInvisibleTool){TControlTool}
  public
    class procedure MouseDown(Point: TDoublePoint); override;
    class procedure MouseMove(Point: TDoublePoint); override;
    class procedure MouseUp; override;
  end;

  TOtherVisibleTool = class(TAllTools)
  end;

  TTextTool = class(TOtherVisibleTool){TTextTool}
  public
    class procedure OnClick(APanel: TPanel); override;
    class procedure MouseDown(Point: TDoublePoint); override;
    class procedure MouseMove(Point: TDoublePoint); override;
    class procedure MouseUp; override;
  end;

  TVisibleTool = class(TAlltools){TVisibleTool}
  public
    class procedure OnClick(APanel: TPanel); override;
    class procedure MouseDown(Point: TDoublePoint); override;
  end;

  {TToolWithotBrush}
  TToolWithoutBrush = class(TVisibleTool)
  end;

  TSprayTool = class(TOtherVisibleTool){TSprayTool}
  public
    class procedure OnClick(APanel: TPanel); override;
    class procedure MouseDown(Point: TDoublePoint); override;
    class procedure MouseMove(Point: TDoublePoint); override;
    class procedure MouseUp; override;
  end;

  TPolylineTool = class(TToolWithoutBrush){TPolylineTool}
  public
    class procedure MouseDown(Point: TDoublePoint); override;
    class procedure MouseMove(Point: TDoublePoint); override;
    class procedure MouseUp; override;
  end;

  TSegmentTool = class(TToolWithoutBrush){TSegmentTool}
  public
    class procedure MouseDown(Point: TDoublePoint); override;
    class procedure MouseMove(Point: TDoublePoint); override;
    class procedure MouseUp; override;
  end;

  TSegmentsTool = class(TToolWithoutBrush){TSegmentsTool}
  public
    class procedure MouseDown(Point: TDoublePoint); override;
    class procedure MouseMove(Point: TDoublePoint); override;
    class procedure MouseUp; override;
  end;

  {TToolWithBrush}
  TToolWithBrush = class(TVisibleTool)
  public
    class procedure OnClick(APanel: TPanel); override;
    class procedure MouseDown(Point: TDoublePoint); override;
  end;

  TRectangleTool = class(TToolWithBrush){TRectangleTool}
  public
    class procedure MouseDown(Point: TDoublePoint); override;
    class procedure MouseMove(Point: TDoublePoint); override;
    class procedure MouseUp; override;
  end;

  TEllipseTool = class(TToolWithBrush){EllipseTool}
  public
    class procedure MouseDown(Point: TDoublePoint); override;
    class procedure MouseMove(Point: TDoublePoint); override;
    class procedure MouseUp; override;
  end;

  TRoundRectTool = class(TToolWithBrush){TRoundRectTool}
  public
    class procedure OnClick(APanel: TPanel); override;
    class procedure MouseDown(Point: TDoublePoint); override;
    class procedure MouseMove(Point: TDoublePoint); override;
    class procedure MouseUp; override;
  end;

  TRegPolygonTool = class(TToolWithBrush){TRegPolygonTool}
  public
    class procedure OnClick(APanel: TPanel); override;
    class procedure MouseDown(Point: TDoublePoint); override;
    class procedure MouseMove(Point: TDoublePoint); override;
    class procedure MouseUp; override;
  end;

  function GetToolClass(AFigure: TFigure): TToolClass;
  function GetGeneralClass: TClass;

var
  BrushColorNow, PenColorNow: TColor;
  ToolClasses: array of TToolClass;
  IsMouseDown: boolean;
  ToolNow: TToolClass;
  ToolTag: integer;
  InvisMethod: TControlDo;
  Timer: TTimer;
  OnMyTimer: TNotifyEvent;
  NewFigure: boolean = True;
  isTimer: boolean;
  PanelPointer: ^TPanel;

const
  ArrayStrProp: array[0..10] of string = ('Size', 'Color', 'IndentX', 'IndentY',
    'Text', 'BrushColor', 'BrushStyle', 'Width', 'PenColor', 'RoundX', 'RoundY');

implementation

var
  WidthPanel, NCornerPanel, DeselectPanel, RoundPanel, TextPanel, SprayPanel,
    BrushStylePanel: TComponent;
  DeselectNumber: byte = 0;
  StartMousePosition: TDoublePoint;
  SelectPoint, SelectFigure: boolean;

{TAllTools}
class procedure TAllTools.OnClick(APanel: TPanel);
begin
  DeselectNumber := 0;
  NCornerPanel.Free;
  NCornerPanel := nil;
  RoundPanel.Free;
  RoundPanel := nil;
  WidthPanel.Free;
  WidthPanel := nil;
  BrushStylePanel.Free;
  BrushStylePanel := nil;
  TextPanel.Free;
  TextPanel := nil;
  DeselectPanel.Free;
  DeselectPanel := nil;
  SprayPanel.Free;
  SprayPanel := nil;
  if ToolNow = TControlTool then begin
    DeselectNumber := 1;
    DeselectPanel := TControlPanel.CreateComponent(APanel, APanel.Height - 45);
  end;
end;

{TInvibleTool}
{THandTool}
class procedure THandTool.MouseDown(Point: TDoublePoint);
begin
  IsMouseDown := True;
  StartMousePosition := DoublePoint(Point.x, Point.y);
end;

class procedure THandTool.MouseMove(Point: TDoublePoint);
begin
  if IsMouseDown then begin
    Offset.x += StartMousePosition.x - Point.x;
    Offset.y += StartMousePosition.y - Point.y;
  end;
end;

class procedure THandTool.MouseUp;
begin
  IsMouseDown := False;
end;

{TControlTool}
class procedure TControlTool.MouseDown(Point: TDoublePoint);
begin
  IsMouseDown := True;
  SelectPoint := InvisMethod.FindSelectPoint(Point);
  SelectFigure := InvisMethod.FindSelectFigure(Point);
  if SelectPoint then begin
    StartMousePosition := Point;
    with InvisMethod do begin
      FPoint1 := Point;
      FPoint2 := Point;
    end;
  end
  else
  if SelectFigure then begin
    StartMousePosition := Point;
    with InvisMethod do begin
      FPoint1 := Point;
      FPoint2 := Point;
    end;
  end
  else begin
    with InvisMethod do begin
      FPoint1 := Point;
      FPoint2 := Point;
      FindFigure(ArrayOfFigures);
    end;
  end;
  Timer.Enabled := True;
  Timer.Interval := 100;
  Time := 0;
end;

class procedure TControlTool.MouseMove(Point: TDoublePoint);
begin
  if isMouseDown then begin
    if SelectPoint then begin
      InvisMethod.ChangeFoundSelectPoint(StartMousePosition, Point);
      StartMousePosition := Point;
      with InvisMethod do begin
        FPoint1 := Point;
        FPoint2 := Point;
      end
    end
    else
    if SelectFigure then begin
      InvisMethod.FindAndMoveFigure(StartMousePosition, Point);
      StartMousePosition := Point;
      with InvisMethod do begin
        FPoint1 := Point;
        FPoint2 := Point;
      end
    end
    else
      with InvisMethod do begin
        FPoint2 := Point;
        FindFigure(ArrayOfFigures);
      end;
  end;
end;

class procedure TControlTool.MouseUp;
var
  i: integer;
  GeneralToolClass: TClass;
begin
  isMouseDown := False;
  Timer.Enabled := False;
  for i := 0 to High(ArrayOfFigures) do
    if ArrayOfFigures[i].FSelect then begin
      Timer.Enabled := True;
      Break;
    end;

  for i := 0 to High(ArrayOfFigures) do
    if ArrayOfFigures[i].FSelect then begin
      GeneralToolClass := GetGeneralClass;
      TToolClass(GeneralToolClass).OnClick(PanelPointer^);
      Break;
    end;
end;
{TOTherVisibleTool}
{TTextTool}
class procedure TTextTool.OnClick(APanel: TPanel);
begin
  inherited;
  TextPanel := TTextPanel.CreateComponent(APanel, APanel.Height - 90 - 45*DeselectNumber );
end;

class procedure TTextTool.MouseDown(Point: TDoublePoint);
begin
  InvisMethod.DeselectFigures;
  isMouseDown := True;
  SetLength(ArrayOfFigures, Length(ArrayOfFigures) + 1);
  ArrayOfFigures[High(ArrayOfFigures)] := ArrayClassFigure[ToolTag].Create;
  ArrayOfFigures[High(ArrayOfFigures)].AddPoint(Point);
  SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'Size', SizeNow);
  SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'PenColor', PenColorNow);
  SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'IndentX', IndentXNow);
  SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'IndentY', IndentYNow);
  SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'Text', TextNow);
end;

class procedure TTextTool.MouseMove(Point: TDoublePoint);
begin
  if IsMouseDown then
    ArrayofFigures[High(ArrayOfFigures)].AddSecondPoint(Point);
end;

class procedure TTextTool.MouseUp;
begin
  isMouseDown := False;
end;

{TSprayTool}
class procedure TSprayTool.OnClick(APanel: TPanel);
begin
  inherited;
  SprayPanel := TSprayPanel.CreateComponent(APanel, APanel.Height - 50*DeselectNumber - 60);
end;

class procedure TSprayTool.MouseDown(Point: TDoublePoint);
begin
  isMouseDown := True;
  InvisMethod.DeselectFigures;
  SetLength(ArrayOfFigures, Length(ArrayOfFigures) + 1);
  ArrayOfFigures[High(ArrayOfFigures)] := ArrayClassFigure[ToolTag].Create;
  SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'Radius', RadiusNow);
  SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'PenColor', PenColorNow);
  Timer.Interval := 50;
  Timer.Enabled := True;
end;

class procedure TSprayTool.MouseMove(Point: TDoublePoint);
var
  DoublePoint: TDoublePoint;
  Radius, i: integer;
begin
  if not isMouseDown then
    Exit;
  if isTimer then begin
    for i := 0 to 19 do begin
      Radius := GetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'Radius');
      DoublePoint := RandomDoublePoint(Radius, Point);
      if Sqr(DoublePoint.x - Point.x) + Sqr(DoublePoint.y - Point.y) <= Sqr(Radius)
      then
        ArrayOfFigures[High(ArrayOfFigures)].AddPoint(DoublePoint);
    end;
    isTimer := False;
  end;
end;

class procedure TSprayTool.MouseUp;
begin
  isMouseDown := False;
  Timer.Enabled := False;
  if (Length(ArrayOfFigures[High(ArrayOfFigures)].FPoints) = 0) then begin
    ArrayOfFigures[High(ArrayOfFigures)] := nil;
    ArrayOfFigures[High(ArrayOfFigures)].Free;
    SetLength(ArrayOfFigures, Length(ArrayOfFigures) - 1);
  end;
end;

{TVisibleTools}
class procedure TVisibleTool.OnClick(APanel: TPanel);
begin
  inherited;
  WidthPanel := TWidthTrackBar.CreateComponent(APanel, APanel.Height - 45*DeselectNumber - 40);
end;

class procedure TVisibleTool.MouseDown(Point: TDoublePoint);
begin
  if NewFigure then begin
    InvisMethod.DeselectFigures;
    SetLength(ArrayOfFigures, Length(ArrayOfFigures) + 1);
    ArrayOfFigures[High(ArrayOfFigures)]:= ArrayClassFigure[ToolTag].Create;
    SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'PenColor', PenColorNow);
    SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'Width', WidthNow);
    SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'PenStyle', PenStyleNow);
  end;
end;

{TToolWithoutBrush}
{TPolylineTool}
class procedure TPolylineTool.MouseDown(Point: TDoublePoint);
begin
  inherited;
  isMouseDown := True;
  ArrayOfFigures[High(ArrayofFigures)].AddPoint(Point);
end;

class procedure TPolylineTool.MouseMove(Point: TDoublePoint);
begin
  if isMouseDown then
    ArrayOfFigures[High(ArrayOfFigures)].AddPoint(Point);
end;

class procedure TPolylineTool.MouseUp;
begin
  isMouseDown := False;
end;

{TSegmentTool}
class procedure TSegmentTool.MouseDown(Point: TDoublePoint);
begin
  inherited;
  isMouseDown := True;
  ArrayOfFigures[High(ArrayOfFigures)].AddPoint(Point);
end;

class procedure TSegmentTool.MouseMove(Point: TDoublePoint);
begin
  if isMouseDown then
    ArrayOfFigures[High(ArrayOfFigures)].AddSecondPoint(Point);
end;

class procedure TSegmentTool.MouseUp;
begin
  isMouseDown := False;
end;

{TSegmentsTool}
class procedure TSegmentsTool.MouseDown(Point: TDoublePoint);
begin
  inherited;
  NewFigure := False;
  ArrayOfFigures[High(ArrayOfFigures)].AddPoint(Point);
end;

class procedure TSegmentsTool.MouseMove(Point: TDoublePoint);
begin
end;

class procedure TSegmentsTool.MouseUp;
begin
end;

{TToolWithBrush}
class procedure TToolWithBrush.MouseDown(Point: TDoublePoint);
begin
  inherited;
  SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'BrushColor', BrushColorNow);
  SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'BrushStyle', integer(BrushStyleNow));
end;

class procedure TToolWithBrush.OnClick(APanel: TPanel);
begin
  inherited;
  BrushStylePanel := TBrushStylePanel.CreateComponent(APanel, APanel.Height - 45*DeselectNumber - 140);
end;

{TRectangleTool}
class procedure TRectangleTool.MouseDown(Point: TDoublePoint);
begin
  inherited;
  IsMouseDown := True;
  ArrayOfFigures[High(ArrayOfFigures)].AddPoint(Point);
end;

class procedure TRectangleTool.MouseMove(Point: TDoublePoint);
begin
  if IsMouseDown then
    ArrayofFigures[High(ArrayOfFigures)].AddSecondPoint(Point);
end;

class procedure TRectangleTool.MouseUp;
begin
  IsMouseDown := False;
end;

{TEllipseTool}
class procedure TEllipseTool.MouseDown(Point: TDoublePoint);
begin
  inherited;
  IsMouseDown := True;
  ArrayOfFigures[High(ArrayOfFigures)].AddPoint(Point);
end;

class procedure TEllipseTool.MouseMove(Point: TDoublePoint);
begin
  if IsMouseDown then
    ArrayofFigures[High(ArrayOfFigures)].AddSecondPoint(Point);
end;

class procedure TEllipseTool.MouseUp;
begin
  IsMouseDown := False;
end;

{TRoundRectTool}
class procedure TRoundRectTool.OnClick(APanel: TPanel);
begin
  inherited;
  RoundPanel := TRoundPanel.CreateComponent(APanel, APanel.Height - 40*DeselectNumber - 170);
end;

class procedure TRoundRectTool.MouseDown(Point: TDoublePoint);
begin
  inherited;
  SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'RoundX', RoundXNow);
  SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'RoundY', RoundYNow);
  IsMouseDown := True;
  ArrayOfFigures[High(ArrayOfFigures)].AddPoint(Point);
end;

class procedure TRoundRectTool.MouseMove(Point: TDoublePoint);
begin
  if IsMouseDown then
    ArrayofFigures[High(ArrayOfFigures)].AddSecondPoint(Point);
end;

class procedure TRoundRectTool.MouseUp;
begin
  isMouseDown := False;
end;

{TRegPolygonTool}
class procedure TRegPolygonTool.OnClick(APanel: TPanel);
begin
  inherited;
  NCornerPanel := TNumberOfCornerTrackBar.CreateComponent(APanel, APanel.Height - 45*DeselectNumber - 180);
end;

class procedure TRegPolygonTool.MouseDown(Point: TDoublePoint);
begin
  inherited;
  SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], 'NCorner', NCornerNow);
  isMouseDown := True;
  ArrayOfFigures[High(ArrayOfFigures)].AddPoint(Point);
end;

class procedure TRegPolygonTool.MouseMove(Point: TDoublePoint);
begin
  if IsMouseDown then
    ArrayOfFigures[High(ArrayOfFigures)].AddSecondPoint(Point);
end;

class procedure TRegPolygonTool.MouseUp;
begin
  IsMouseDown := False;
end;

////////////////////////////////////////////////////////////////////////////////
function GetToolClass(AFigure: TFigure): TToolClass;
var
  i, j: integer;
begin
  for i := 0 to High(ArrayClassFigure) do
    if AFigure.ClassName = ArrayClassFigure[i].ClassName then begin
      j := i;
      Break;
    end;
  Result := ToolClasses[j];
end;

procedure AddClassTool(ClassTool: TToolClass);
begin
  SetLength(ToolClasses, Length(ToolClasses) + 1);
  ToolClasses[High(ToolClasses)] := ClassTool;
end;

function GetGeneralClass: TClass;
type
  ArrayOfTClass = array of TClass;
var
  SelectToolClasses: array of TToolClass = nil;
  ArrayClass1: ArrayOfTClass = nil;
  ArrayClass2: ArrayOfTClass = nil;
  Go: boolean;
  i, k, h: integer;
  GeneralToolClass: TClass;
procedure GetArrayClasses(var ArrayOfClass: ArrayOfTClass; AClass: TClass);
begin
  SetLength(ArrayOfClass, 1);
  ArrayOfClass[0] := AClass;
  while AClass <> TAllTools do begin
    AClass := AClass.ClassParent;
    SetLength(ArrayOfClass, Length(ArrayOfClass) + 1);
    ArrayOfClass[High(ArrayOfClass)] := AClass;
  end;
end;
begin
  Go := False;
  for i := 0 to High(ArrayOfFigures) do
    if ArrayOfFigures[i].FSelect then begin
      SetLength(SelectToolClasses, Length(SelectToolClasses) + 1);
      SelectToolClasses[High(SelectToolClasses)] := GetToolClass(ArrayOfFigures[i]);
      if not Go then begin
        Go := True;
        GeneralToolClass := SelectToolClasses[High(SelectToolClasses)];
      end;
    end;
  if Go then
    for i := 1 to High(SelectToolClasses) do begin
      GetArrayClasses(ArrayClass1, GeneralToolClass);
      GetArrayClasses(ArrayClass2, SelectToolClasses[i]);
      Go := False;
      h := 0;
      k := 0;
      while not Go do begin
        while h <= High(ArrayClass2) do begin
          if ArrayClass1[k] = ArrayClass2[h] then begin
            GeneralToolClass := ArrayClass2[h];
            Go := True;
            k := 0;
            h := High(ArrayClass2) + 1;
          end
          else
            h += 1;
        end;
        k += 1;
        h := 0;
      end;
    end;
  Result := GeneralToolClass;
end;

initialization

  AddClassTool(THandTool);
  AddClassTool(TPolylineTool);
  AddClassTool(TSegmentTool);
  AddClassTool(TRectangleTool);
  AddClassTool(TEllipseTool);
  AddClassTool(TSegmentsTool);
  AddClassTool(TRegPolygonTool);
  AddClassTool(TRoundRectTool);
  AddClassTool(TTextTool);
  AddClassTool(TControlTool);
  AddClassTool(TSprayTool);

  InvisMethod := TControlDo.Create;

end.
