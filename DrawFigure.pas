unit DrawFigure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Graph, uDoublePoint, FPCanvas,
  typInfo, math;

type
  TFigure = class(TObject)
  public
    FPoints: array of TDoublePoint;
    FSelect: boolean;
  public
    procedure AddPoint(APoint: TDoublePoint); virtual; abstract;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure AddSecondPoint(APoint: TDoublePoint); virtual; abstract;
    procedure GetMaxAndMinPoints; virtual;
    procedure DrawAnchors(ACanvas: TCanvas); virtual; abstract;
    procedure ChangePointPosition(APoint1, APoint2: TDoublePoint); virtual; abstract;
    procedure MoveFigure(APoint1, APoint2: TDoublePoint); virtual; abstract;
    function FoundAnchorsPoint(APoint: TDoublePoint): boolean; virtual; abstract;
    function MouseOnFigure(APoint1: TDoublePoint): boolean; virtual; abstract;
    function MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean; virtual; abstract;
    procedure GetSVGString(var Str: string); virtual; abstract;
  end;

  TInvisibleFigure = class(TFigure){Invisible}
  end;

  THand = class(TInvisibleFigure){Hand}
  end;

  TControlFig = class(TInvisibleFigure)
  end;

  TOtherVisibleFigure = class(TFigure)
  end;

  TText = class(TOtherVisibleFigure){Text}
  protected
    FText: string;
    FSize: integer;
    FPenColor: TColor;
    FIndentX, FIndentY: integer;
  public
    procedure AddPoint(APoint: TDoublePoint); override;
    procedure AddSecondPoint(APoint: TDoublePoint); override;
    procedure Draw(Canvas: TCanvas); override;
    function MouseOnFigure(APoint1: TDoublePoint): boolean; override;
    function MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean; override;
    procedure DrawAnchors(ACanvas: TCanvas); override;
    procedure ChangePointPosition(APoint1, APoint2: TDoublePoint); override;
    procedure MoveFigure(APoint1, APoint2: TDoublePoint); override;
    function FoundAnchorsPoint(APoint: TDoublePoint): boolean; override;
    procedure GetSVGString(var Str: string); override;
  published
    property IndentX: integer read FIndentX write FIndentX;
    property IndentY: integer read FIndentY write FIndentY;
    property PenColor: TColor read FPenColor write FPenColor;
    property Size: integer read FSize write FSize;
    property Text: string read FText write FText;
  end;

  TSpray = class(TOtherVisibleFigure)
  protected
    FRadius: integer;
    FPenColor: TColor;
  published
    property Radius: integer read FRadius write FRadius;
    property PenColor: TColor read FPenColor write FPenColor;
  public
    procedure Draw(Canvas: TCanvas); override;
    procedure AddPoint(APoint: TDoublePoint); override;
    function MouseOnFigure(APoint1: TDoublePoint): boolean; override;
    function MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean; override;
    procedure DrawAnchors(ACanvas: TCanvas); override;
    procedure ChangePointPosition(APoint1, APoint2: TDoublePoint); override;
    procedure MoveFigure(APoint1, APoint2: TDoublePoint); override;
    function FoundAnchorsPoint(APoint: TDoublePoint): boolean; override;
    procedure GetSVGString(var Str: string); override;
  end;

  TVisibleFigure = class(TFigure){Visible}
  protected
    FPenStyle: TPenStyle;
    FPenColor: TColor;
    FWidth: integer;
  published
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property PenColor: TColor read FPenColor write FPenColor;
    property Width: integer read FWidth write FWidth;
  end;

  TWithoutBrushFigure = class(TVisibleFigure){WithoutBrush}
  public
    procedure GetSVGString(var Str: string); override;
  end;

  TPolyline = class(TWithoutBrushFigure){Polyline}
  public
    procedure AddPoint(APoint: TDoublePoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure ChangePointPosition(APoint1, APoint2: TDoublePoint); override;
    procedure MoveFigure(APoint1, APoint2: TDoublePoint); override;
    procedure DrawAnchors(ACanvas: TCanvas); override;
    function MouseOnFigure(APoint1: TDoublePoint): boolean; override;
    function MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean; override;
    function FoundAnchorsPoint(APoint: TDoublePoint): boolean; override;
  end;

  TSegment = class(TWithoutBrushFigure){Segment}
  public
    procedure AddPoint(APoint: TDoublePoint); override;
    procedure AddSecondPoint(APoint: TDoublePoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure ChangePointPosition(APoint1, APoint2: TDoublePoint); override;
    procedure MoveFigure(APoint1, APoint2: TDoublePoint); override;
    procedure DrawAnchors(ACanvas: TCanvas); override;
    function MouseOnFigure(APoint1: TDoublePoint): boolean; override;
    function MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean; override;
    function FoundAnchorsPoint(APoint: TDoublePoint): boolean; override;
  end;

  TSegments = class(TWithoutBrushFigure){Segments}
  public
    procedure AddPoint(APoint: TDoublePoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawAnchors(ACanvas: TCanvas); override;
    procedure ChangePointPosition(APoint1, APoint2: TDoublePoint); override;
    procedure MoveFigure(APoint1, APoint2: TDoublePoint); override;
    function FoundAnchorsPoint(APoint: TDoublePoint): boolean; override;
    function MouseOnFigure(APoint1: TDoublePoint): boolean; override;
    function MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean; override;
  end;

  TBrushFigure = class(TVisibleFigure){BrushColor}
  protected
    FBrushStyle: TBrushStyle;
    FBrushColor: TColor;
  published
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property BrushColor: TColor read FBrushColor write FBrushColor;
  end;

  TRectangle = class(TBrushFigure){Rectangle}
  public
    procedure AddPoint(APoint: TDoublePoint); override;
    procedure AddSecondPoint(APoint: TDoublePoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure ChangePointPosition(APoint1, APoint2: TDoublePoint); override;
    procedure MoveFigure(APoint1, APoint2: TDoublePoint); override;
    procedure DrawAnchors(ACanvas: TCanvas); override;
    function MouseOnFigure(APoint1: TDoublePoint): boolean; override;
    function MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean; override;
    function FoundAnchorsPoint(APoint: TDoublePoint): boolean; override;
    procedure GetSVGString(var Str: string); override;
  end;

  TEllipse = class(TBrushFigure){Ellipse}
  public
    procedure AddPoint(APoint: TDoublePoint); override;
    procedure AddSecondPoint(APoint: TDoublePoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure ChangePointPosition(APoint1, APoint2: TDoublePoint); override;
    procedure MoveFigure(APoint1, APoint2: TDoublePoint); override;
    procedure DrawAnchors(ACanvas: TCanvas); override;
    function MouseOnFigure(APoint1: TDoublePoint): boolean; override;
    function MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean; override;
    function FoundAnchorsPoint(APoint: TDoublePoint): boolean; override;
    procedure GetSVGString(var Str: string); override;
  end;

  TRegPolygon = class(TBrushFigure){RegPolygon}
  protected
    FNCorner: integer;
  published
    property NCorner: integer read FNCorner write FNCorner;
  public
    procedure AddPoint(APoint: TDoublePoint); override;
    procedure AddSecondPoint(APoint: TDoublePoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure ChangePointPosition(APoint1, APoint2: TDoublePoint); override;
    procedure MoveFigure(APoint1, APoint2: TDoublePoint); override;
    procedure DrawAnchors(ACanvas: TCanvas); override;
    function MouseOnFigure(APoint1: TDoublePoint): boolean; override;
    function MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean; override;
    function FoundAnchorsPoint(APoint: TDoublePoint): boolean; override;
    procedure GetSVGString(var Str: string); override;
  end;

  TRoundRect = class (TBrushFigure){RoundRect}
  protected
    FRoundX, FRoundY: integer;
  public
    procedure AddPoint(APoint: TDoublePoint); override;
    procedure AddSecondPoint(APoint: TDoublePoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure ChangePointPosition(APoint1, APoint2: TDoublePoint); override;
    procedure MoveFigure(APoint1, APoint2: TDoublePoint); override;
    procedure DrawAnchors(ACanvas: TCanvas); override;
    function MouseOnFigure(APoint1: TDoublePoint): boolean; override;
    function MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean; override;
    function FoundAnchorsPoint(APoint: TDoublePoint): boolean; override;
    procedure GetSVGString(var Str: string); override;
  published
    property RoundX: integer read FRoundX write FRoundX;
    property RoundY: integer read FRoundY write FRoundY;
  end;

  TFiguresClass = class of TFigure;
  TEasyEvent = procedure of Object;

  function GetFigureClass(ANameFigure: string): TFiguresClass;
  function GetColor(ATime: integer): TColor;
  function GetIndexFigure(ANameFigure: string): integer;
  function SVGColor(AColor: integer): string;
  function SVGPenStyle(APenStyle: TPenStyle; AWidth: integer): string;
  function SVGBrushStyle(ABrushStyle: TBrushStyle; ABrushColor: TColor): string;
  function SVGBrushStylePattern(ABrushStyle: TBrushStyle; ABrushColor: TColor): string;

  var
    ArrayClassFigure: array of TFiguresClass;
    ArrayOfFigures: array of TFigure;
    Time: integer = 15;
    NewFigure: boolean = True;
    NumberPattern: integer = 0;

  implementation

  {Figure}
  procedure TFigure.GetMaxAndMinPoints;
  var
    i: integer;
  begin
    for i := 0 to High(FPoints) do begin
      if FPoints[i].x > MaxDoublePoint.x then
        MaxDoublePoint.x := FPoints[i].x;
      if FPoints[i].y > MaxDoublePoint.y then
        MaxDoublePoint.y := FPoints[i].y;
      if FPoints[i].x < MinDoublePoint.x then
        MinDoublePoint.x := FPoints[i].x;
      if FPoints[i].y < MinDoublePoint.y then
        MinDoublePoint.y := FPoints[i].y;
    end;
  end;

  {OtherVisibleFigure}
  {Text}
  procedure TText.AddPoint(APoint: TDoublePoint);
  begin
    if Length(FPoints) <> 2 then
      SetLength(FPoints, 2);
    FPoints[0] := APoint;
    FPoints[1] := APoint;
  end;

  procedure TText.AddSecondPoint(APoint: TDoublePoint);
  begin
    FPoints[1] := APoint;
  end;

  procedure TText.Draw(Canvas: TCanvas);
  var
    PointInt1, PointInt2: TPoint;
  begin
    PointInt1 := WorldToScreen(FPoints[0]);
    PointInt2 := WorldToScreen(FPoints[1]);
    Canvas.Font.Color := FPenColor;
    Canvas.Font.Size := FSize;
    Canvas.TextRect(
      Rect(
        min(PointInt1.x, PointInt2.x),
        min(PointInt1.y, PointInt2.y),
        max(PointInt1.x, PointInt2.x),
        max(PointInt1.y, PointInt2.y)),
      min(PointInt1.x, PointInt2.x) + IndentX,
      min(PointInt1.y, PointInt2.y) + IndentY,
      FText);
  end;

  function TText.MouseOnFigure(APoint1: TDoublePoint): boolean;
  begin
    if (min(FPoints[0].x, FPoints[1].x) < APoint1.x) and
      (min(FPoints[0].y, FPoints[1].y) < APoint1.y) and
      (max(FPoints[0].x, FPoints[1].x) > APoint1.x) and
      (max(FPoints[0].y, FPoints[1].y) > APoint1.y)
    then
      Result := True
    else
      Result := False;
  end;

  function TText.MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean;
  begin
    if (max(APoint1.x, APoint2.x) < min(FPoints[0].x, FPoints[1].x)) or
      (min(APoint1.x, APoint2.x) > max(FPoints[0].x, FPoints[1].x)) or
      (max(APoint1.y, APoint2.y) < min(FPoints[0].y, FPoints[1].y)) or
      (min(APoint1.y, APoint2.y) > max(FPoints[0].y, FPoints[1].y))
    then begin
      Result := False;
      Exit;
    end;
    Result := True;
  end;

  procedure TText.ChangePointPosition(APoint1, APoint2: TDoublePoint);
  begin
    if (abs(FPoints[0].x - APoint1.x) <= 4/Scale) and (abs(FPoints[0].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[0] := APoint2;
      Exit;
    end;
    if (abs(FPoints[1].x - APoint1.x) <= 4/Scale) and (abs(FPoints[1].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[1] := APoint2;
      Exit;
    end;
    if (abs(FPoints[0].x - APoint1.x) <= 4/Scale) and (abs(FPoints[1].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[0].x := APoint2.x;
      FPoints[1].y := APoint2.y;
      Exit;
    end;
    if (abs(FPoints[1].x - APoint1.x) <= 4/Scale) and (abs(FPoints[0].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[1].x := APoint2.x;
      FPoints[0].y := APoint2.y;
    end;
  end;

  procedure TText.DrawAnchors(ACanvas: TCanvas);
  var
    Point1, Point2: TPoint;
  begin
    Point1 := WorldToScreen(FPoints[0]);
    Point2 := WorldToScreen(FPoints[1]);
    with ACanvas do begin
      Pen.Color := GetColor(Time);
      Brush.Color := GetColor(Time);
      Brush.Style := bsSolid;
      Pen.Width := 1;
      Pen.Style := psSolid;
      Rectangle(Point1.x - 4, Point1.y - 4, Point1.x + 4, Point1.y + 4);
      Rectangle(Point1.x - 4, Point2.y - 4, Point1.x + 4, Point2.y + 4);
      Rectangle(Point2.x - 4, Point1.y - 4, Point2.x + 4, Point1.y + 4);
      Rectangle(Point2.x - 4, Point2.y - 4, Point2.x + 4, Point2.y + 4);
    end;
  end;

  procedure TText.MoveFigure(APoint1, APoint2: TDoublePoint);
  begin
    FPoints[0].x += Apoint2.x - APoint1.x;
    FPoints[0].y += APoint2.y - APoint1.y;
    FPoints[1].y += APoint2.y - APoint1.y;
    FPoints[1].x += APoint2.x - APoint1.x;
  end;

  function TText.FoundAnchorsPoint(APoint: TDoublePoint): boolean;
  begin
    if ((abs(FPoints[0].x - APoint.x) <= 4/Scale) and (abs(FPoints[0].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[0].x - APoint.x) <= 4/Scale) and (abs(FPoints[1].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[1].x - APoint.x) <= 4/Scale) and (abs(FPoints[0].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[1].x - APoint.x) <= 4/Scale) and (abs(FPoints[1].y - APoint.y) <= 4/Scale))
    then
      Result := True
    else
      Result := False;
  end;

  procedure TText.GetSVGString(var Str: string);
  var
    Point: TDoublePoint;
  begin
    Str += '<text opacity="1" font-size="' + IntToStr(FSize) + '" fill="#' + SVGColor(FPenColor) + '"';
    Point := DoublePoint(Min(FPoints[0].x, FPoints[1].x), Min(FPoints[0].y, FPoints[1].y));
    Str += ' x="' + FloatToStr(Point.x) + '" y="' + FloatToStr(Point.y) + '">' + FText + '</text>';
  end;

  {Spray}
  procedure TSpray.AddPoint(APoint: TDoublePoint);
  begin
    SetLength(FPoints, Length(FPoints) + 1);
    FPoints[High(FPoints)] := APoint;
  end;

  procedure TSpray.Draw(Canvas: TCanvas);
  var
    i: integer;
    Points: array of TPoint;
  begin
    if Length(FPoints) = 0 then
      Exit;
    with Canvas do begin
      Pen.Color := PenColor;
      SetLength(Points, Length(FPoints));
      for i := 0 to High(FPoints) do begin
        Points[i] := WorldToScreen(FPoints[i]);
        Pixels[Points[i].x, Points[i].y] := PenColor;
      end;
    end;
  end;

  function TSpray.MouseOnFigure(APoint1: TDoublePoint): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to High(FPoints) do
      if (abs(APoint1.x - FPoints[i].x) <= 4/Scale) and
        (abs(APoint1.y - FPoints[i].y) <= 4/Scale)
      then begin
        Result := True;
        Exit;
      end;
  end;

  function TSpray.MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to High(FPoints) do
      if (FPoints[i].x <= max(APoint1.x, APoint2.x)) and
        (FPoints[i].x >= min(APoint1.x, APoint2.x)) and
        (FPoints[i].y <= max(APoint1.y, APoint2.y)) and
        (FPoints[i].y >= min(APoint1.y, APoint2.y))
      then begin
        Result := True;
        Exit;
      end;
  end;

  procedure TSpray.DrawAnchors(ACanvas: TCanvas);
  var
    APoint1, APoint2: TDoublePoint;
    P1, P2: TPoint;
    i: integer;
  begin
    APoint1 := FPoints[0];
    APoint2 := FPoints[0];
    for i := 1 to High(FPoints) do begin
      if APoint1.x < FPoints[i].x then
        APoint1.x := FPoints[i].x;
      if APoint1.y < FPoints[i].y then
        APoint1.y := FPoints[i].y;
      if APoint2.x > FPoints[i].x then
        APoint2.x := FPoints[i].x;
      if APoint2.y > FPoints[i].y then
        APoint2.y := FPoints[i].y;
    end;
    with ACanvas do begin
      Pen.Style := psSolid;
      Pen.Color := GetColor(Time);
      Pen.Width := 1;
      Brush.Color := GetColor(Time);
      Brush.Style := bsSolid;
      P1 := WorldToScreen(APoint1);
      P2 := WorldToScreen(APoint2);
      Rectangle(P1.x - 4, P1.y - 4, P1.x + 4, P1.y + 4);
      Rectangle(P2.x - 4, P1.y - 4, P2.x + 4, P1.y + 4);
      Rectangle(P2.x - 4, P2.y - 4, P2.x + 4, P2.y + 4);
      Rectangle(P1.x - 4, P2.y - 4, P1.x + 4, P2.y + 4);
    end;
  end;

  procedure TSpray.ChangePointPosition(APoint1, APoint2: TDoublePoint);
  var
    i: integer;
  begin
    for i := 0 to High(FPoints) do begin
      FPoints[i].x += APoint2.x - APoint1.x;
      FPoints[i].y += APoint2.y - APoint1.y;
    end;
  end;

  procedure TSpray.MoveFigure(APoint1, APoint2: TDoublePoint);
  var
    i: integer;
  begin
    for i := 0 to High(FPoints) do begin
      FPoints[i].x += APoint2.x - APoint1.x;
      FPoints[i].y += APoint2.y - APoint1.y;
    end;
  end;

  function TSpray.FoundAnchorsPoint(APoint: TDoublePoint): boolean;
  var
    APoint1, APoint2: TDoublePoint;
    i: integer;
  begin
    for i := 0 to High(FPoints) do
      if (abs(FPoints[i].x - APoint.x) <= 4/Scale) and (abs(FPoints[i].y - APoint.y) <= 4/Scale) then
        Result := True
      else
        Result := False;
    if Result then
      Exit;
    APoint1 := FPoints[0];
    APoint2 := FPoints[0];
    for i := 1 to High(FPoints) do begin
      if APoint1.x < FPoints[i].x then
        APoint1.x := FPoints[i].x;
      if APoint1.y < FPoints[i].y then
        APoint1.y := FPoints[i].y;
      if APoint2.x > FPoints[i].x then
        APoint2.x := FPoints[i].x;
      if APoint2.y > FPoints[i].y then
        APoint2.y := FPoints[i].y;
    end;
    if ((abs(APoint1.x - APoint.x) <= 4/Scale) and (abs(APoint1.y - APoint.y) <= 4/Scale)) or
      ((abs(APoint1.x - APoint.x) <= 4/Scale) and (abs(APoint2.y - APoint.y) <= 4/Scale)) or
      ((abs(APoint2.x - APoint.x) <= 4/Scale) and (abs(APoint1.y - APoint.y) <= 4/Scale)) or
      ((abs(APoint2.x - APoint.x) <= 4/Scale) and (abs(APoint2.y - APoint.y) <= 4/Scale))
    then
      Result := True
    else
      Result := False;
  end;

  procedure TSpray.GetSVGString(var Str: string);
  var
    i: integer;
    PointsStr: string = '';
  begin
    for i := 0 to High(FPoints) do
      PointsStr += 'M ' + IntToStr(Round(FPoints[i].x)) + ' ' + IntToStr(Round(FPoints[i].y))
        + ' ' + 'L ' + IntToStr(Round(FPoints[i].x + 1)) + ' ' + IntToStr(Round(FPoints[i].y + 1)) + ' ';
    PointsStr += 'z';
    Str := '<path fill="none" stroke="#' + SVGColor(FPenColor) + '" stroke-width="0.5"'
      + ' d="' + PointsStr + '"></path>';
  end;

  {Visible}
  {WithoutBrushFigure}
  procedure TWithoutBrushFigure.GetSVGString(var Str: string);
  var
    i: integer;
    PointsStr: string = '';
  begin
    for i := 0 to High(FPoints) do
      PointsStr += IntToStr(Round(FPoints[i].x)) + ',' + IntToStr(Round(FPoints[i].y)) + ' ';
    Str := '<polyline fill="none" stroke="#' + SVGColor(FPenColor) +
    '" stroke-width="' + IntToStr(FWidth) + '" points="' + PointsStr + '" ' +
    SVGPenStyle(FPenStyle, FWidth) + ' />';
  end;

  {Polyline}
  procedure TPolyline.AddPoint(APoint: TDoublePoint);
  begin
    SetLength(FPoints, Length(FPoints) + 1);
    FPoints[High(FPoints)] := APoint;
  end;

  procedure TPolyline.Draw(Canvas: TCanvas);
  var
    ArrayPoint: array of TPoint;
    i: integer;
  begin
    Canvas.Pen.Color := FPenColor;
    Canvas.Pen.Width := FWidth;
    Canvas.Pen.Style := FPenStyle;
    SetLength(ArrayPoint, length(FPoints));
    for i:= 0 to High(FPoints) do
      ArrayPoint[i] := WorldToScreen(FPoints[i]);
    Canvas.Polyline(ArrayPoint);
  end;

  function TPolyline.MouseOnFigure(APoint1: TDoublePoint): boolean;
  var
    i: integer;
  begin
    for i := 0 to High(FPoints) do
      if (abs(APoint1.x - FPoints[i].x) <= 4/scale) and
        (abs(APoint1.y - FPoints[i].y) <= 4/scale)
      then begin
        Result := True;
        Exit;
      end
      else
        Result := False;
  end;

  function TPolyline.MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean;
  var
    i: integer;
  begin
    for i := 0 to High(FPoints) do
      if (FPoints[i].x <= max(APoint1.x, APoint2.x)) and
        (FPoints[i].x >= min(APoint1.x, APoint2.x)) and
        (FPoints[i].y <= max(APoint1.y, APoint2.y)) and
        (FPoints[i].y >= min(APoint1.y, APoint2.y))
      then begin
        Result := True;
        Exit;
      end
      else
        Result := False;
  end;

  procedure TPolyline.DrawAnchors(ACanvas: TCanvas);
  var
    Point: TPoint;
  begin
    with ACanvas do begin
      Pen.Color := GetColor(Time);
      Brush.Color := GetColor(Time);
      Brush.Style := bsSolid;
      Pen.Width := 1;
      Pen.Style := psSolid;
    end;
    Point := WorldToScreen(FPoints[1]);
    ACanvas.Rectangle(Point.x - 4, Point.y - 4, Point.x + 4, Point.y + 4);
    Point := WorldToScreen(FPoints[High(FPoints)]);
    ACanvas.Rectangle(Point.x - 4, Point.y - 4, Point.x + 4, Point.y + 4);
  end;

  procedure TPolyline.ChangePointPosition(APoint1, APoint2: TDoublePoint);
  var
    i: integer;
  begin
    for i := 0 to High(FPoints) do begin
      FPoints[i].x += APoint2.x - APoint1.x;
      FPoints[i].y += APoint2.y - APoint1.y;
    end;
  end;

  procedure TPolyline.MoveFigure(APoint1, APoint2: TDoublePoint);
  var
    i: integer;
  begin
    for i := 0 to High(FPoints) do begin
      FPoints[i].x += APoint2.x - APoint1.x;
      FPoints[i].y += APoint2.y - APoint1.y;
    end;
  end;

  function TPolyline.FoundAnchorsPoint(APoint: TDoublePoint): boolean;
  begin
    if (abs(FPoints[0].x - APoint.x) <= 4/Scale) and (abs(FPoints[0].y - APoint.y) <= 4/Scale) or
      (abs(FPoints[High(FPoints)].x - APoint.x) <= 4/Scale) and (abs(FPoints[High(FPoints)].y - APoint.y) <= 4/Scale)
    then
      Result := True
    else
      Result := False;
  end;

  {Segment}
  procedure TSegment.AddPoint(APoint: TDoublePoint);
  begin
    SetLength(FPoints, 2);
    FPoints[0] := APoint;
    FPoints[1] := APoint;
  end;

  procedure TSegment.AddSecondPoint(APoint: TDoublePoint);
  begin
    FPoints[1] := APoint;
  end;

  procedure TSegment.Draw(Canvas: TCanvas);
  begin
    with Canvas do begin
      Pen.Color := FPenColor;
      Pen.Width := FWidth;
      Pen.Style := FPenStyle;
      Line(WorldToScreen(FPoints[0]), WorldToScreen(FPoints[1]));
    end;
  end;

  function TSegment.MouseOnFigure(APoint1: TDoublePoint): boolean;
  var
    DP1, DP2: TDoublePoint;
    k, b: double;
  begin
    if (FPoints[0].x < FPoints[1].x) then begin
      DP1 := FPoints[0];
      DP2 := FPoints[1];
    end
    else begin
      DP1 := FPoints[1];
      DP2 := FPoints[0];
    end;
    if DP1.x = DP2.x then begin
      k := 0;
      if (APoint1.x + 4/scale >= DP1.x) and (APoint1.x - 4/scale <= DP1.x) then
        Result := True
      else
        Result := False;
      Exit;
    end
    else begin
      k := (DP2.y - DP1.y)/(DP2.x - DP1.x);
      b := DP1.y - k*DP1.x;
      if (APoint1.y + 4/scale >= k*APoint1.x + b) and (APoint1.y - 4/scale <= k*APoint1.x + b)
        then begin
          Result := True;
          Exit;
        end
      else
        Result := False;
    end;
  end;

  function TSegment.MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean;
  var
    DP1, DP2: TDoublePoint;
    k, b: double;
  begin
    if (FPoints[0].x < FPoints[1].x) then begin
      DP1 := FPoints[0];
      DP2 := FPoints[1];
    end
    else begin
      DP1 := FPoints[1];
      DP2 := FPoints[0];
    end;
    if DP1.x = DP2.x then begin
      k := 0;
      if (max(APoint1.x, APoint2.x) < min(DP1.x, DP2.x)) or
        (min(APoint1.x, APoint2.x) > max(DP1.x, DP2.x)) or
        (max(APoint1.y, APoint2.y) < min(DP1.y, DP2.y)) or
        (min(APoint1.y, APoint2.y) > max(DP1.y, DP2.y))
      then begin
        Result := False;
        Exit;
      end
      else
        Result := True;
    end
    else begin
      k := (DP2.y - DP1.y)/(DP2.x - DP1.x);
      b := DP1.y - k*DP1.x;
      if (APoint1.y < k*APoint1.x + b) and
        (APoint1.y < k*APoint2.x + b) and
        (APoint2.y < k*APoint1.x + b) and
        (APoint2.y < k*APoint2.x + b)
      then begin
        Result := False;
        Exit;
      end;
      if (APoint1.y > k*APoint1.x + b) and
        (APoint1.y > k*APoint2.x + b) and
        (APoint2.y > k*APoint1.x + b) and
        (APoint2.y > k*APoint2.x + b)
      then begin
        Result := False;
        Exit;
      end;
      Result := True;
      if (max(APoint1.x, APoint2.x) < min(DP1.x, DP2.x)) or
        (min(APoint1.x, APoint2.x) > max(DP1.x, DP2.x)) or
        (max(APoint1.y, APoint2.y) < min(DP1.y, DP2.y)) or
        (min(APoint1.y, APoint2.y) > max(DP1.y, DP2.y))
      then
        Result := False;
    end;
  end;

  procedure TSegment.DrawAnchors(ACanvas: TCanvas);
  var
    Point: TPoint;
  begin
    with ACanvas do begin
      Pen.Color := GetColor(Time);
      Brush.Color := GetColor(Time);
      Brush.Style := bsSolid;
      Pen.Width := 1;
      Pen.Style := psSolid;
    end;
    Point := WorldToScreen(FPoints[0]);
    ACanvas.Rectangle(Point.x - 4, Point.y - 4, Point.x + 4, Point.y + 4);
    Point := WorldToScreen(FPoints[1]);
    ACanvas.Rectangle(Point.x - 4, Point.y - 4, Point.x + 4, Point.y + 4);
  end;

  procedure TSegment.ChangePointPosition(APoint1, APoint2: TDoublePoint);
  begin
    if (abs(APoint1.x - FPoints[0].x) <= 4/Scale) and (abs(APoint1.y - FPoints[0].y) <= 4/Scale)
    then begin
      FPoints[0] := APoint2;
      Exit;
    end;
    if (abs(APoint1.x - FPoints[1].x) <= 4/Scale) and (abs(APoint1.y - FPoints[1].y) <= 4/Scale)
    then begin
      FPoints[1] := APoint2;
      Exit;
    end
  end;

  procedure TSegment.MoveFigure(APoint1, APoint2: TDoublePoint);
  begin
    FPoints[0].x +=  APoint2.x - APoint1.x;
    FPoints[0].y +=  APoint2.y - APoint1.y;
    FPoints[1].x +=  APoint2.x - APoint1.x;
    FPoints[1].y +=  APoint2.y - APoint1.y;
  end;

  function TSegment.FoundAnchorsPoint(APoint: TDoublePoint): boolean;
  begin
    if ((abs(FPoints[0].x - APoint.x) <= 4/Scale) and (abs(FPoints[0].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[1].x - APoint.x) <= 4/Scale) and (abs(FPoints[1].y - APoint.y) <= 4/Scale))
    then begin
      Result := True;
      Exit;
    end;
    Result := False;
  end;

  {Segments}
  procedure TSegments.AddPoint(APoint: TDoublePoint);
  begin
    if (Length(FPoints) > 0) and (Abs(APoint.x - FPoints[High(FPoints)].x) <= 4/Scale) and
      (Abs(APoint.y - FPoints[High(FPoints)].y) <= 4/Scale)
    then begin
      NewFigure := True;
      Exit;
    end;
    SetLength(FPoints, Length(FPoints) + 1);
    FPoints[High(FPoints)] := APoint;
  end;

  procedure TSegments.Draw(Canvas: TCanvas);
  var
    i: integer;
  begin
    Canvas.Pen.Color := FPenColor;
    Canvas.Pen.Width := FWidth;
    Canvas.Pen.Style := FPenStyle;
    for i := 0 to High(FPoints) - 1 do
      Canvas.Line(WorldToScreen(FPoints[i]), WorldToScreen(FPoints[i + 1]));
  end;

  function TSegments.MouseOnFigure(APoint1: TDoublePoint): boolean;
  var
    DP1, DP2: TDoublePoint;
    k, b: double;
    i: integer;
  begin
    for i := 0 to High(FPoints) - 1 do begin
      if (FPoints[i].x < FPoints[i + 1].x) then begin
        DP1 := FPoints[i];
        DP2 := FPoints[i + 1];
      end
      else begin
        DP1 := FPoints[i + 1];
        DP2 := FPoints[i];
      end;
      if DP1.x = DP2.x then begin
        k := 0;
        if (APoint1.x + 4/scale >= DP1.x) and (APoint1.x - 4/scale <= DP1.x) then begin
          Result := True;
          Exit;
        end
        else
          Result := False;
      end
      else begin
        k := (DP2.y - DP1.y)/(DP2.x - DP1.x);
        b := DP1.y - k*DP1.x;
        if (APoint1.y + 4/scale >= k*APoint1.x + b) and (APoint1.y - 4/scale <= k*APoint1.x + b)
          then begin
            Result := True;
            Exit;
          end
      else
        Result := False;
      end;
    end;
  end;

  function TSegments.MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean;
  var
    DP1, DP2: TDoublePoint;
    k, b: double;
    i: integer;
  begin
    for i := 0 to High(FPoints) - 1 do begin
      if (FPoints[i].x < FPoints[i + 1].x) then begin
        DP1 := FPoints[i];
        DP2 := FPoints[i + 1];
      end
      else begin
        DP1 := FPoints[i + 1];
        DP2 := FPoints[i];
      end;
      if DP1.x = DP2.x then begin
        k := 0;
        if (max(APoint1.x, APoint2.x) > DP2.x) or
          (min(APoint1.x, APoint2.x) < DP1.x)
        then
          Result := False
        else begin
          Result := True;
          if (max(APoint1.x, APoint2.x) < min(FPoints[i].x, FPoints[i + 1].x)) or
            (min(APoint1.x, APoint2.x) > max(FPoints[i].x, FPoints[i + 1].x)) or
            (max(APoint1.y, APoint2.y) < min(FPoints[i].y, FPoints[i + 1].y)) or
            (min(APoint1.y, APoint2.y) > max(FPoints[i].y, FPoints[i + 1].y))
          then
            Result := False;
          if Result = True then
            Exit;
        end;
      end
      else begin
        k := (DP2.y - DP1.y)/(DP2.x - DP1.x);
        b := DP1.y - k*DP1.x;
        if ((APoint1.y < k*APoint1.x + b) and
          (APoint1.y < k*APoint2.x + b) and
          (APoint2.y < k*APoint1.x + b) and
          (APoint2.y < k*APoint2.x + b)) or
          ((APoint1.y > k*APoint1.x + b) and
          (APoint1.y > k*APoint2.x + b) and
          (APoint2.y > k*APoint1.x + b) and
          (APoint2.y > k*APoint2.x + b))
        then begin
          Result := False;
        end
        else begin
          Result := True;
          if (max(APoint1.x, APoint2.x) < min(FPoints[i].x, FPoints[i + 1].x)) or
            (min(APoint1.x, APoint2.x) > max(FPoints[i].x, FPoints[i + 1].x)) or
            (max(APoint1.y, APoint2.y) < min(FPoints[i].y, FPoints[i + 1].y)) or
            (min(APoint1.y, APoint2.y) > max(FPoints[i].y, FPoints[i + 1].y))
          then
            Result := False;
          if Result = True then
            Exit;
        end;
      end
    end;
  end;

  procedure TSegments.DrawAnchors(ACanvas: TCanvas);
  var
    Point: TPoint;
    i: integer;
  begin
    with ACanvas do begin
      Pen.Color := GetColor(Time);
      Brush.Color := GetColor(Time);
      Brush.Style := bsSolid;
      Pen.Width := 1;
      Pen.Style := psSolid;
    end;
    for i := 0 to high(FPoints) do begin
      Point := WorldToScreen(FPoints[i]);
      ACanvas.Rectangle(Point.x - 4, Point.y - 4, Point.x + 4, Point.y + 4);
    end;
  end;

  procedure TSegments.ChangePointPosition(APoint1, APoint2: TDoublePoint);
  var
    i: integer;
  begin
    for i := 0 to high(FPoints) do
      if ((abs(APoint1.x - FPoints[i].x) <= 4/Scale) and (abs(APoint1.y - FPoints[i].y) <= 4/Scale))
      then begin
        FPoints[i].x := APoint2.x;
        FPoints[i].y := APoint2.y;
        Exit;
      end;
  end;

  procedure TSegments.MoveFigure(APoint1, APoint2: TDoublePoint);
  var
    i: integer;
  begin
    for i := 0 to High(FPoints) do begin
      FPoints[i].x += APoint2.x - APoint1.x;
      FPoints[i].y += APoint2.y - APoint1.y;
    end;
  end;

  function TSegments.FoundAnchorsPoint(APoint: TDoublePoint): boolean;
  var
    i: integer;
  begin
    for i := 0 to High(FPoints) do begin
      if (abs(FPoints[i].x - APoint.x) <= 4/Scale) and (abs(FPoints[i].y - APoint.y) <= 4/Scale)
      then begin
        Result :=  True;
        Exit;
      end;
    end;
    Result := False;
  end;

  {BrushFigure}
  {Rectangle}
  procedure TRectangle.AddPoint(APoint: TDoublePoint);
  begin
    if Length(FPoints) <> 2 then
      SetLength(FPoints, 2);
    FPoints[0] := APoint;
    FPoints[1] := APoint;
  end;

  procedure TRectangle.AddSecondPoint(APoint: TDoublePoint);
  begin
    FPoints[1] := APoint;
  end;

  procedure TRectangle.Draw(Canvas: TCanvas);
  var
    FirstPointInt, SecondPointInt: TPoint;
  begin
    Canvas.Brush.Color := FBrushColor;
    Canvas.Brush.Style := FBrushStyle;
    Canvas.Pen.Color := FPenColor;
    Canvas.Pen.Width := FWidth;
    Canvas.Pen.Style := FPenStyle;
    FirstPointInt := WorldToScreen(FPoints[0]);
    SecondPointInt := WorldToScreen(FPoints[1]);
    Canvas.Rectangle(FirstPointInt.x, FirstPointInt.y, SecondPointInt.x, SecondPointInt.y);
  end;

  function TRectangle.MouseOnFigure(APoint1: TDoublePoint): boolean;
  begin
    if (min(FPoints[0].x, FPoints[1].x) < APoint1.x) and
      (min(FPoints[0].y, FPoints[1].y) < APoint1.y) and
      (max(FPoints[0].x, FPoints[1].x) > APoint1.x) and
      (max(FPoints[0].y, FPoints[1].y) > APoint1.y)
    then
      Result := True
    else
      Result := False;
  end;

  function TRectangle.MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean;
  begin
    if (max(APoint1.x, APoint2.x) < min(FPoints[0].x, FPoints[1].x)) or
      (min(APoint1.x, APoint2.x) > max(FPoints[0].x, FPoints[1].x)) or
      (max(APoint1.y, APoint2.y) < min(FPoints[0].y, FPoints[1].y)) or
      (min(APoint1.y, APoint2.y) > max(FPoints[0].y, FPoints[1].y))
    then begin
      Result := False;
      Exit;
    end;
    Result := True;
  end;

  procedure TRectangle.DrawAnchors(ACanvas: TCanvas);
  var
    Point1, Point2 :TPoint;
  begin
    Point1 := WorldToScreen(FPoints[0]);
    Point2 := WorldToScreen(FPoints[1]);

    with ACanvas do begin
      Pen.Color := GetColor(Time);
      Brush.Color := GetColor(Time);
      Brush.Style := bsSolid;
      Pen.Width := 1;
      Pen.Style := psSolid;
      Rectangle(Point1.x - 4, Point1.y - 4, Point1.x + 4, Point1.y + 4);
      Rectangle(Point1.x - 4, Point2.y - 4, Point1.x + 4, Point2.y + 4);
      Rectangle(Point2.x - 4, Point1.y - 4, Point2.x + 4, Point1.y + 4);
      Rectangle(Point2.x - 4, Point2.y - 4, Point2.x + 4, Point2.y + 4);
    end;
  end;

  procedure TRectangle.ChangePointPosition(APoint1, APoint2: TDoublePoint);
  begin
    if (abs(FPoints[0].x - APoint1.x) <= 4/Scale) and (abs(FPoints[0].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[0] := APoint2;
      Exit;
    end;
    if (abs(FPoints[1].x - APoint1.x) <= 4/Scale) and (abs(FPoints[1].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[1] := APoint2;
      Exit;
    end;
    if (abs(FPoints[0].x - APoint1.x) <= 4/Scale) and (abs(FPoints[1].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[0].x := APoint2.x;
      FPoints[1].y := APoint2.y;
      Exit;
    end;
    if (abs(FPoints[1].x - APoint1.x) <= 4/Scale) and (abs(FPoints[0].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[1].x := APoint2.x;
      FPoints[0].y := APoint2.y;
    end;
  end;

  procedure TRectangle.MoveFigure(APoint1, APoint2: TDoublePoint);
  begin
    FPoints[0].x += Apoint2.x - APoint1.x;
    FPoints[0].y += APoint2.y - APoint1.y;
    FPoints[1].y += APoint2.y - APoint1.y;
    FPoints[1].x += APoint2.x - APoint1.x;
  end;

  function TRectangle.FoundAnchorsPoint(APoint: TDoublePoint): boolean;
  begin
    if ((abs(FPoints[0].x - APoint.x) <= 4/Scale) and (abs(FPoints[0].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[0].x - APoint.x) <= 4/Scale) and (abs(FPoints[1].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[1].x - APoint.x) <= 4/Scale) and (abs(FPoints[0].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[1].x - APoint.x) <= 4/Scale) and (abs(FPoints[1].y - APoint.y) <= 4/Scale))
    then
      Result := True
    else
      Result := False;
  end;

  procedure TRectangle.GetSVGString(var Str: string);
  begin
    Str += SVGBrushStylePattern(FBrushStyle, FBrushColor) +
      '<rect x="' +  FloatToStr(Min(FPoints[0].x, FPoints[1].x)) + '" y="' +
      FloatToStr(Min(FPoints[0].y, FPoints[1].y)) + '" width="' +
      FloatToStr(Abs(FPoints[0].x - FPoints[1].x)) + '" height="' +
      FloatToStr(Abs(FPoints[0].y - FPoints[1].y)) + '" stroke-width="' +
      IntToStr(FWidth) + '" stroke="#' + SVGColor(FPenColor) + '"' +
      SVGBrushStyle(FBrushStyle, FBrushColor) + SVGPenStyle(FPenStyle, FWidth) + ' />';
  end;

  {Ellipse}
  procedure TEllipse.AddPoint(APoint: TDoublePoint);
  begin
    if Length(FPoints) <> 2 then
      SetLength(FPoints, 2);
    FPoints[0] := APoint;
    FPoints[1] := APoint;
  end;

  procedure TEllipse.AddSecondPoint(APoint: TDoublePoint);
  begin
    FPoints[1] := APoint;
  end;

  procedure TEllipse.Draw(Canvas: TCanvas);
  var
    FirstPointInt, SecondPointInt: TPoint;
  begin
    Canvas.Brush.Color := FBrushColor;
    Canvas.Brush.Style := FBrushStyle;
    Canvas.Pen.Color := FPenColor;
    Canvas.Pen.Width := FWidth;
    Canvas.Pen.Style := FPenStyle;
    FirstPointInt := WorldToScreen(FPoints[0]);
    SecondPointInt := WorldToScreen(FPoints[1]);
    Canvas.Ellipse(FirstPointInt.x, FirstPointInt.y, SecondPointInt.x, SecondPointInt.y);
  end;

  function TEllipse.MouseOnFigure(APoint1: TDoublePoint): boolean;
  var
    a, b, x, y: double;
  begin
    a := abs(FPoints[0].x - FPoints[1].x)/2;
    b := abs(FPoints[0].y - FPoints[1].y)/2;
    x := APoint1.x - min(FPoints[0].x, FPoints[1].x) - a;
    y := APoint1.y - min(FPoints[0].y, FPoints[1].y) - b;
    if  (a <> 0) and (b <> 0) then begin
      if sqr(x)/sqr(a) + sqr(y)/sqr(b) <= 1 then
        Result := True
      else
        Result := False;
    end
    else begin
      if (a = 0) and (max(FPoints[0].y, FPoints[1].y) > APoint1.y) and
        (min(FPoints[0].y, FPoints[1].y) < APoint1.y)
      then
        Result := True
      else
        if (b = 0) and (max(FPoints[0].x, FPoints[1].x) > APoint1.x) and
          (min(FPoints[0].x, FPoints[1].x) < APoint1.x)
        then
          Result := True
          else
            Result := False;
    end;
  end;

  function TEllipse.MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean;
  var
    a, b, x1, y1, x2, y2: double;
  begin
    if (max(APoint1.x, APoint2.x) < min(FPoints[0].x, FPoints[1].x)) or
      (min(APoint1.x, APoint2.x) > max(FPoints[0].x, FPoints[1].x)) or
      (max(APoint1.y, APoint2.y) < min(FPoints[0].y, FPoints[1].y)) or
      (min(APoint1.y, APoint2.y) > max(FPoints[0].y, FPoints[1].y))
    then begin
      Result := False;
      Exit;
    end;
    a := abs(FPoints[0].x - FPoints[1].x)/2;
    b := abs(FPoints[0].y - FPoints[1].y)/2;
    x1 := APoint1.x - min(FPoints[0].x, FPoints[1].x) - a;
    y1 := APoint1.y - min(FPoints[0].y, FPoints[1].y) - b;
    x2 := APoint2.x - min(FPoints[0].x, FPoints[1].x) - a;
    y2 := APoint2.y - min(FPoints[0].y, FPoints[1].y) - b;

    if (x1*x2 > 0) and (y1*y2 > 0) then
      if (sqr(x1)/sqr(a) + sqr(y1)/sqr(b) > 1) and
        (sqr(x2)/sqr(a) + sqr(y2)/sqr(b) > 1) and
        (sqr(x1)/sqr(a) + sqr(y2)/sqr(b) > 1) and
        (sqr(x2)/sqr(a) + sqr(y1)/sqr(b) > 1)
      then begin
        Result := False;
        Exit;
      end;

    Result := True;
  end;

  procedure TEllipse.DrawAnchors(ACanvas: TCanvas);
  var
    Point1, Point2 :TPoint;
  begin
    Point1 := WorldToScreen(FPoints[0]);
    Point2 := WorldToScreen(FPoints[1]);
    with ACanvas do begin
      Pen.Color := GetColor(Time);
      Brush.Color := GetColor(Time);
      Brush.Style := bsSolid;
      Pen.Width := 1;
      Pen.Style := psSolid;
      Rectangle(Point1.x - 4, Point1.y - 4, Point1.x + 4, Point1.y + 4);
      Rectangle(Point1.x - 4, Point2.y - 4, Point1.x + 4, Point2.y + 4);
      Rectangle(Point2.x - 4, Point1.y - 4, Point2.x + 4, Point1.y + 4);
      Rectangle(Point2.x - 4, Point2.y - 4, Point2.x + 4, Point2.y + 4);
    end;
  end;

  procedure TEllipse.ChangePointPosition(APoint1, APoint2: TDoublePoint);
  begin
    if (abs(FPoints[0].x - APoint1.x) <= 4/Scale) and (abs(FPoints[0].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[0].x := APoint2.x;
      FPoints[0].y := APoint2.y;
      Exit;
    end;
    if (abs(FPoints[1].x - APoint1.x) <= 4/Scale) and (abs(FPoints[1].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[1] := APoint2;
      Exit;
    end;
    if (abs(FPoints[0].x - APoint1.x) <= 4/Scale) and (abs(FPoints[1].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[0].x := APoint2.x;
      FPoints[1].y := APoint2.y;
      Exit;
    end;
    if (abs(FPoints[1].x - APoint1.x) <= 4/Scale) and (abs(FPoints[0].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[1].x := APoint2.x;
      FPoints[0].y := APoint2.y;
    end;
  end;

  procedure TEllipse.MoveFigure(APoint1, APoint2: TDoublePoint);
  begin
    FPoints[0].x += Apoint2.x - APoint1.x;
    FPoints[0].y += APoint2.y - APoint1.y;
    FPoints[1].y += APoint2.y - APoint1.y;
    FPoints[1].x += APoint2.x - APoint1.x;
  end;

  function TEllipse.FoundAnchorsPoint(APoint: TDoublePoint): boolean;
  begin
    if ((abs(FPoints[0].x - APoint.x) <= 4/Scale) and (abs(FPoints[0].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[0].x - APoint.x) <= 4/Scale) and (abs(FPoints[1].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[1].x - APoint.x) <= 4/Scale) and (abs(FPoints[0].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[1].x - APoint.x) <= 4/Scale) and (abs(FPoints[1].y - APoint.y) <= 4/Scale))
    then
      Result := True
    else
      Result := False;
  end;

  procedure TEllipse.GetSVGString(var Str: string);
  var
    rx, ry, cx, cy: string;
  begin
    cx := IntToStr(Round((FPoints[0].x + FPoints[1].x)/2));
    cy := IntToStr(Round((FPoints[0].y + FPoints[1].y)/2));
    rx := IntToStr(Round(Abs((FPoints[0].x - FPoints[1].x)/2)));
    ry := IntToStr(Round(Abs((FPoints[0].y - FPoints[1].y)/2)));
  Str += SVGBrushStylePattern(FBrushStyle, FBrushColor) +
    '<ellipse cx="' + cx + '" cy="' + cy + '" stroke-width="' +
    IntToStr(FWidth) + '" stroke="#' + SVGColor(FPenColor) + '"' +
    SVGBrushStyle(FBrushStyle, FBrushColor) +  ' rx="' + rx + '" ry="' + ry  +
    '"' + SVGPenStyle(FPenStyle, FWidth) + ' />';
  end;

  {RegPolygon}
  procedure TRegPolygon.AddPoint(APoint: TDoublePoint);
  var
    i: integer;
  begin
    if Length(FPoints) = 0 then begin
      SetLength(FPoints, 1 + FNCorner);
      for i := 1 to High(FPoints) do
        FPoints[i] := APoint;
    end;
    FPoints[0] := APoint;
  end;

  procedure TRegPolygon.AddSecondPoint(APoint: TDoublePoint);
  var
    i: integer;
    N: integer;
    dl: double;
    Sgn: double;
  begin
    dl := sqrt( sqr(APoint.x - FPoints[0].x) + sqr(APoint.y - FPoints[0].y) );
    N := FNCorner;
    SetLength(FPoints, N + 1);
    FPoints[1] := APoint;
    for i:= 2 to High(FPoints) do
      if FPoints[1].x <> FPoints[0].x then begin
        Sgn := abs(FPoints[1].x - FPoints[0].x) / (FPoints[1].x - FPoints[0].x);
        FPoints[i].x := FPoints[0].x + Sgn * dl * cos(2*pi*(i - 1)/N +
          arctan((FPoints[1].y - FPoints[0].y)/(FPoints[1].x - FPoints[0].x)));

        FPoints[i].y := FPoints[0].y + Sgn * dl * sin(2*pi*(i - 1)/N +
          arctan((FPoints[1].y - FPoints[0].y)/(FPoints[1].x - FPoints[0].x)));
      end
      else
      if FPoints[1].y <> FPoints[0].y then begin
        Sgn := abs(FPoints[1].y - FPoints[0].y) / (FPoints[1].y - FPoints[0].y);
        FPoints[i].x := FPoints[0].x +  Sgn * dl * cos(2*pi*(High(FPoints) - (i - 1) + 1)/N + pi/2);
        FPoints[i].y := FPoints[0].y +  Sgn * dl * sin(2*pi*(High(FPoints) - (i - 1) + 1)/N + pi/2);
      end
      else begin
        FPoints[i].x := FPoints[0].x;
        FPoints[i].y := FPoints[0].y;
      end;
  end;

  procedure TRegPolygon.Draw(Canvas: TCanvas);
  var
    PointsInt: array of TPoint;
    i: integer;
  begin
    if (FPoints <> nil) and (Length(FPoints) > 1) then begin
      AddSecondPoint(FPoints[1]);
      Canvas.Brush.Color := FBrushColor;
      Canvas.Brush.Style := FBrushStyle;
      Canvas.Pen.Color := FPenColor;
      Canvas.Pen.Width := FWidth;
      Canvas.Pen.Style := FPenStyle;
      SetLength(PointsInt, Length(FPoints) - 1);
      for i:= 0 to High(PointsInt) do
        PointsInt[i] := WorldToScreen(FPoints[i + 1]);
      Canvas.Polygon(PointsInt);
    end;
  end;

  function TRegPolygon.MouseOnFigure(APoint1: TDoublePoint): boolean;
  var
    k, b: double;
    i: integer;
    a, c: byte;
  begin
    if Length(FPoints) = 1 then begin
      Result := False;
      Exit;
    end;
    for i := 1 to High(FPoints) do begin
      a := i;
      c := i + 1;
      if i = High(FPoints) then begin
        a := i;
        c := 1;
      end;
      if FPoints[a].x = FPoints[c].x then begin
        if (FPoints[c].x = APoint1.x) and
        ((Max(FPoints[a].y, FPoints[c].y) > APoint1.y) or
        (Min(FPoints[a].y, FPoints[c].y) < APoint1.y)) then
          Result := True
        else
          Result := False;
        Exit;
      end
      else
        k := (FPoints[a].y - FPoints[c].y)/(FPoints[a].x - FPoints[c].x);
      b := FPoints[a].y - FPoints[a].x*k;
      if (FPoints[a].x < FPoints[c].x) and (APoint1.y < APoint1.x*k + b) then begin
        Result := False;
        Exit;
      end;
      if (FPoints[a].x > FPoints[c].x) and (APoint1.y > APoint1.x*k + b) then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;

  function TRegPolygon.MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean;
  var
    k, b: double;
    i: integer;
    Index1, Index2: byte;
    MinX, MinY, MaxX, MaxY: double;
  begin
    if (Length(FPoints) = 1) or ((FPoints[1].x = FPoints[2].x) and (FPoints[1].y = FPoints[2].y))
    then begin
      Result := False;
      Exit;
    end;
    MinX := FPoints[1].x;
    MinY := FPoints[1].y;
    MaxX := MinX;
    MaxY := MinY;
    for i := 1 to High(FPoints) do begin
      Index1 := i;
      Index2 := i + 1;
      if i = High(FPoints) then begin
        Index1 := i;
        Index2 := 0;
      end;
      k := (FPoints[Index1].y - FPoints[Index2].y)/(FPoints[Index1].x - FPoints[Index2].x);
      b := FPoints[Index1].y - FPoints[Index1].x*k;
      if (FPoints[Index1].x < FPoints[Index2].x) and (APoint1.y < APoint1.x*k + b) and
        (APoint2.y < APoint1.x*k + b) and (APoint2.y < APoint2.x*k + b) and
        (APoint1.y < APoint2.x*k + b)
      then begin
        Result := False;
        Exit;
      end;
      if (FPoints[Index1].x > FPoints[Index2].x) and (APoint1.y > APoint1.x*k + b) and
      (APoint2.y > APoint1.x*k + b) and (APoint1.y > APoint2.x*k + b) and
      (APoint2.y > APoint2.x*k + b)
      then begin
        Result := False;
        Exit;
      end;
    end;
    for i := 0 to High(FPoints) do begin
      if FPoints[i].x > MaxX then
        MaxX := FPoints[i].x;
      if FPoints[i].y > MaxY then
        MaxY := FPoints[i].y;
      if FPoints[i].x < MinX then
        MinX := FPoints[i].x;
      if FPoints[i].y < MinY then
        MinY := FPoints[i].y;
    end;
    if (APoint1.x > MaxX) and (APoint2.x > MaxX) or
      (APoint1.x < MinX) and (APoint2.x < MinY) or
      (APoint1.y > MaxY) and (APoint2.y > MaxY) or
      (APoint1.y < MinY) and (APoint2.y < MinY)
    then begin
      Result := False;
      Exit;
    end;
    Result := True;
  end;

  procedure TRegPolygon.DrawAnchors(ACanvas: TCanvas);
  var
    Point: TPoint;
    i: integer;
  begin
    with ACanvas do begin
      Pen.Color := GetColor(Time);
      Brush.Color := GetColor(Time);
      Brush.Style := bsSolid;
      Pen.Width := 1;
      Pen.Style := psSolid;
      for i := 1 to High(FPoints) do begin
        Point := WorldToScreen(FPoints[i]);
        Rectangle(Point.x - 4, Point.y - 4, Point.x + 4, Point.y + 4);
      end;
    end;
  end;

  procedure TRegPolygon.ChangePointPosition(APoint1, APoint2: TDoublePoint);
  var
    i: integer;
  begin
    for i := 1 to High(FPoints) do
      if (abs(APoint1.x - FPoints[i].x) <= 4/Scale) and (abs(APoint1.y - FPoints[i].y) <= 4/Scale)
      then begin
        FPoints[1] := DoublePoint(FPoints[i].x, FPoints[i].y);
        FPoints[1].x += APoint2.x - APoint1.x;
        FPoints[1].y += APoint2.y - APoint1.y;
      end;
  end;

  procedure TRegPolygon.MoveFigure(APoint1, APoint2: TDoublePoint);
  var
    i: integer;
  begin
    for i := 0 to High(FPoints) do begin
      FPoints[i].x += APoint2.x - APoint1.x;
      FPoints[i].y += APoint2.y - APoint1.y;
    end;
  end;

  function TRegPolygon.FoundAnchorsPoint(APoint: TDoublePoint): boolean;
  var
    i: integer;
  begin
    for i := 1 to High(FPoints) do
      if (abs(FPoints[i].x - APoint.x) <= 4/Scale) and (abs(FPoints[i].y - APoint.y) <= 4/Scale)
      then begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;

  procedure TRegPolygon.GetSVGString(var Str: string);
  var
    i: integer;
    PointsStr: string = '';
  begin
    for i := 1 to High(FPoints) do
      PointsStr += IntToStr(Round(FPoints[i].x)) + ',' + IntToStr(Round(FPoints[i].y)) + ' ';
    Str := SVGBrushStylePattern(FBrushStyle, FBrushColor) +
      '<polygon ' + SVGBrushStyle(FBrushStyle, FBrushColor) + ' stroke="#' +
      SVGColor(FPenColor) + '" stroke-width="' + IntToStr(FWidth) + '" points="' +
      PointsStr + '" ' + SVGPenStyle(FPenStyle, FWidth) + ' />';
  end;

  {RoundRect}
  procedure TRoundRect.AddPoint(APoint: TDoublePoint);
  begin
    if Length(FPoints) <> 2 then
      SetLength(FPoints, 2);
    FPoints[0] := APoint;
    FPoints[1] := APoint;
  end;

  procedure TRoundRect.AddSecondPoint(APoint: TDoublePoint);
  begin
    FPoints[1] := APoint;
  end;

  procedure TRoundRect.Draw(Canvas: TCanvas);
  var
    FirstPointInt, SecondPointInt: TPoint;
  begin
    Canvas.Brush.Color := FBrushColor;
    Canvas.Brush.Style := FBrushStyle;
    Canvas.Pen.Color := FPenColor;
    Canvas.Pen.Width := FWidth;
    Canvas.Pen.Style := FPenStyle;
    FirstPointInt := WorldToScreen(FPoints[0]);
    SecondPointInt := WorldToScreen(FPoints[1]);
    Canvas.RoundRect(FirstPointInt.x, FirstPointInt.y, SecondPointInt.x, SecondPointInt.y,
      RoundX, RoundY);
  end;

  function TRoundRect.MouseOnFigure(APoint1: TDoublePoint): boolean;
  begin
    if (min(FPoints[0].x, FPoints[1].x) < APoint1.x) and
      (min(FPoints[0].y, FPoints[1].y) < APoint1.y) and
      (max(FPoints[0].x, FPoints[1].x) > APoint1.x) and
      (max(FPoints[0].y, FPoints[1].y) > APoint1.y)
    then
      Result := True
    else
      Result := False;
  end;

  function TRoundRect.MouseRectOnFigure(APoint1, APoint2: TDoublePoint): boolean;
  begin
    if (max(APoint1.x, APoint2.x) < min(FPoints[0].x, FPoints[1].x)) or
      (min(APoint1.x, APoint2.x) > max(FPoints[0].x, FPoints[1].x)) or
      (max(APoint1.y, APoint2.y) < min(FPoints[0].y, FPoints[1].y)) or
      (min(APoint1.y, APoint2.y) > max(FPoints[0].y, FPoints[1].y))
    then begin
      Result := False;
      Exit;
    end;
    Result := True;
  end;

  procedure TRoundRect.DrawAnchors(ACanvas: TCanvas);
  var
    Point1, Point2 :TPoint;
  begin
    Point1 := WorldToScreen(FPoints[0]);
    Point2 := WorldToScreen(FPoints[1]);
    with ACanvas do begin
      Pen.Color := GetColor(Time);
      Brush.Color := GetColor(Time);
      Brush.Style := bsSolid;
      Pen.Width := 1;
      Pen.Style := psSolid;
      Rectangle(Point1.x - 4, Point1.y - 4, Point1.x + 4, Point1.y + 4);
      Rectangle(Point1.x - 4, Point2.y - 4, Point1.x + 4, Point2.y + 4);
      Rectangle(Point2.x - 4, Point1.y - 4, Point2.x + 4, Point1.y + 4);
      Rectangle(Point2.x - 4, Point2.y - 4, Point2.x + 4, Point2.y + 4);
    end;
  end;

  procedure TRoundRect.ChangePointPosition(APoint1, APoint2: TDoublePoint);
  begin
    if (abs(FPoints[0].x - APoint1.x) <= 4/Scale) and (abs(FPoints[0].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[0] := APoint2;
      Exit;
    end;
    if (abs(FPoints[1].x - APoint1.x) <= 4/Scale) and (abs(FPoints[1].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[1] := APoint2;
      Exit;
    end;
    if (abs(FPoints[0].x - APoint1.x) <= 4/Scale) and (abs(FPoints[1].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[0].x := APoint2.x;
      FPoints[1].y := APoint2.y;
      Exit;
    end;
    if (abs(FPoints[1].x - APoint1.x) <= 4/Scale) and (abs(FPoints[0].y - APoint1.y) <= 4/Scale)
    then begin
      FPoints[1].x := APoint2.x;
      FPoints[0].y := APoint2.y;
    end;
  end;

  procedure TRoundRect.MoveFigure(APoint1, APoint2: TDoublePoint);
  begin
    FPoints[0].x += Apoint2.x - APoint1.x;
    FPoints[0].y += APoint2.y - APoint1.y;
    FPoints[1].y += APoint2.y - APoint1.y;
    FPoints[1].x += APoint2.x - APoint1.x;
  end;

  function TRoundRect.FoundAnchorsPoint(APoint: TDoublePoint): boolean;
  begin
    if ((abs(FPoints[0].x - APoint.x) <= 4/Scale) and (abs(FPoints[0].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[0].x - APoint.x) <= 4/Scale) and (abs(FPoints[1].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[1].x - APoint.x) <= 4/Scale) and (abs(FPoints[0].y - APoint.y) <= 4/Scale)) or
      ((abs(FPoints[1].x - APoint.x) <= 4/Scale) and (abs(FPoints[1].y - APoint.y) <= 4/Scale))
    then
      Result := True
    else
      Result := False;
  end;

  procedure TRoundRect.GetSVGString(var Str: string);
  begin
   Str += SVGBrushStylePattern(FBrushStyle, FBrushColor) +
     '<rect x="' +  FloatToStr(Min(FPoints[0].x, FPoints[1].x)) + '" y="' +
     FloatToStr(Min(FPoints[0].y, FPoints[1].y)) + '" width="' +
     FloatToStr(Abs(FPoints[0].x - FPoints[1].x)) + '" height="' +
     FloatToStr(Abs(FPoints[0].y - FPoints[1].y)) + '" stroke-width="' +
     IntToStr(FWidth) + '" stroke="#' + SVGColor(FPenColor) + '"' +
     SVGBrushStyle(FBrushStyle, FBrushColor) +  ' rx="' + IntToStr(RoundX div 2) + '" ry="' +
     IntToStr(RoundY div 2) + '"' + SVGPenStyle(FPenStyle, FWidth) + ' />';
  end;

////////////////////////////////////////////////////////////////////////////////

  function SVGColor(AColor: integer): string;
  var
    Red, Green, Blue, Hex: string;
  begin
    Hex := IntToHex(AColor, 6);
    Red := Copy(Hex, 1, 2);
    Green := Copy(Hex, 3, 2);
    Blue := Copy(Hex, 5, 2);
    Result := Blue + Green + Red;  // RGB --> BGR
  end;

  function SVGPenStyle(APenStyle: TPenStyle; AWidth: integer): string;
  var
    W: integer;
  begin
    W := AWidth;
    Result := ' stroke-dasharray="';
    case APenStyle of
      psSolid:
        Result := '';
      psDash:
        Result += IntToStr(18) + ',' + IntToStr(6 + 2*W) + '"';
      psDot:
        Result += IntToStr(3) + ',' + IntToStr(3 + 2*W) + '"';
      psDashDot:
        Result += IntToStr(12) + ',' + IntToStr(3 + 2*W) + ',' + IntToStr(3) +
          ',' + IntToStr(9 + 2*W) + '"';
      psDashDotDot:
        Result += IntToStr(12) + ',' + IntToStr(3 + 2*W) + ',' + IntToStr(3) + ','
          + IntToStr(3 + 2*W) + ',' + IntToStr(3) + ',' + IntToStr(3 + 2*W) + '"';
    end;
  end;

  function SVGBrushStyle(ABrushStyle: TBrushStyle; ABrushColor: TColor): string;
  begin
    Result := ' fill="';
    case ABrushStyle of
      bsSolid:
        Result += '#' + SVGColor(ABrushColor) + '"';
      bsClear:
        Result += 'none"';
    end;
    if not (ABrushStyle = bsSolid) and not (ABrushStyle = bsClear) then
      Result += 'url(#Pattern' + IntToStr(NumberPattern) + ')"';
    Inc(NumberPattern);
  end;

  function SVGBrushStylePattern(ABrushStyle: TBrushStyle; ABrushColor: TColor): string;
  begin
    Result := '<g stroke="#' + SVGColor(ABrushColor)  + '" ><pattern id="Pattern' + IntToStr(NumberPattern)
    + '" patternUnits="userSpaceOnUse" x="0" y="0" width="8" height="8" '
    + 'viewBox="1 1 5 5" ><path stroke-width="0.5"';
    case ABrushStyle of
      bsSolid:
        Result := '';
      bsClear:
        Result := '';
      bsHorizontal:
        Result += ' d="M 0 3 L 6 3 z" ';
      bsVertical:
        Result += ' d="M 3 0 L 3 6 z" ';
      bsFDiagonal:
        Result += ' d="M 1 1 L 6 6 z" ';
      bsBDiagonal:
        Result += ' d="M 6 1 L 1 6 z" ';
      bsCross:
        Result += ' d="M 3 0 L 3 6 M 0 3 L 6 3 z" ';
      bsDiagCross:
        Result += ' d="M 0 0 L 6 6 M 6 1 L 1 6 z" ';
    end;
    if not (ABrushStyle = bsSolid) and not (ABrushStyle = bsClear) then
      Result += '/></pattern></g>';
  end;

  function GetFigureClass(ANameFigure: string): TFiguresClass;
  var
    i: integer;
  begin
    for i := 0 to High(ArrayClassFigure) do
      if ANameFigure = ArrayClassFigure[i].ClassName then begin
        Result := ArrayClassFigure[i];
        Exit;
      end;
  end;

  function GetIndexFigure(ANameFigure: string): integer;
  var
    i: integer;
  begin
    for i := 0 to High(ArrayClassFigure) do
      if ANameFigure = ArrayClassFigure[i].ClassName then begin
        Result := i;
        Exit;
      end;
  end;

  procedure AddClassFigure(ClassFigure: TFiguresClass);
  begin
    SetLength(ArrayClassFigure, Length(ArrayClassFigure) + 1);
    ArrayClassFigure[High(ArrayClassFigure)] := ClassFigure;
  end;

  function GetColor(ATime: integer): TColor;
  var
    Flag: integer;
  begin
    Flag := ATime div 16;
    ATime := ATime mod 16;
    if Flag mod 2 = 0 then
      ATime := 16 - ATime;
    Result := $000000 + ATime*$F;
  end;

initialization
  AddClassFigure(THand);
  AddClassFigure(TPolyline);
  AddClassFigure(TSegment);
  AddClassFigure(TRectangle);
  AddClassFigure(TEllipse);
  AddClassFigure(TSegments);
  AddClassFigure(TRegPolygon);
  AddClassFigure(TRoundRect);
  AddClassFigure(TText);
  AddClassFigure(TControlFig);
  AddClassFigure(TSpray);
end.
