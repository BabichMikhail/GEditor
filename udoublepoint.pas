unit UDoublePoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, StdCtrls, Math;

type

  TDoublePoint = record
    x, y: Double;
  end;

  TVisibleRectangle = record
    Left, Top, Right, Bottom: double;
  end;

function ScreenToWorld(AScreenPoint: TPoint): TDoublePoint;
function WorldToScreen(AWorldPoint: TDoublePoint): TPoint;
function DoublePoint(Ax, Ay: Double): TDoublePoint;
function MyRandom(M: integer): integer;
function RandomDoublePoint(Modul: integer; Point: TDoublePoint): TDoublePoint;
procedure ChangeMaxMinScrollBarOnScale(var ABottomScroll, ARightScroll: TScrollBar;
  OldScale, NewScale: double; AWidth, AHeight: integer);
procedure ChangeMaxMinScrollBarOnMove(var ABottomScroll, ARightScroll: TScrollBar;
  AWidth, AHeight: integer);
procedure SetNewOffSet(AWidth, AHeight: integer; OldScale: double);
procedure SetOffsetOnScroll(var AScroll: TScrollBar; AMinVisRect, AMaxVisRect,
  MinPoint, MaxPoint: double);

var
  Offset: TDoublePoint;
  Scale: Double;
  XPred: integer;
  LastPoint: TPoint;
  MaxDoublePoint, MinDoublePoint: TDoublePoint;
  VisRect: TVisibleRectangle;

implementation

function ScreenToWorld(AScreenPoint: TPoint): TDoublePoint;
begin
  Result.x := AScreenPoint.x/Scale + Offset.x;
  Result.y := AScreenPoint.y/Scale + Offset.y;
end;

function WorldToScreen(AWorldPoint: TDoublePoint): TPoint;
begin
  Result.x := Round((AWorldPoint.x - Offset.x)*Scale);
  Result.y := Round((AWorldPoint.y - Offset.y)*Scale);
end;

function DoublePoint(Ax, Ay: Double): TDoublePoint;
begin
  Result.x := Ax;
  Result.y := Ay;
end;

function MyRandom(m: integer): integer;
begin
  Result := (4231*XPred + 123457) mod m;
  XPred := Result;
  {1664525/1013904223}{4231/123457}{4096/150889/714025}
end;

function RandomDoublePoint(Modul: integer; Point: TDoublePoint): TDoublePoint;
begin
  Result.x := Point.x + Random(2*Modul) - Modul;
  Result.y := Point.y + Random(2*Modul) - Modul;
end;

procedure ChangeMaxMinScrollBarOnScale(var ABottomScroll, ARightScroll: TScrollBar;
  OldScale, NewScale: double; AWidth, AHeight: integer);
begin
  with VisRect do begin
    Left := Offset.x*Scale;
    Top :=  Offset.y*Scale;
    Right := Offset.x*Scale + AWidth;
    Bottom := Offset.y*Scale + AHeight;
  end;
  if NewScale/OldScale > 1 then begin
    ABottomScroll.Max := Max(Round(MaxDoublePoint.x*Scale), Round(VisRect.Right)) + 3 ;
    ABottomScroll.Min := Min(Round(MinDoublePoint.x*Scale), Round(VisRect.Left)) - 3;
    ARightScroll.Max := Max(Round(MaxDoublePoint.y*Scale), Round(VisRect.Bottom)) + 3;
    ARightScroll.Min := Min(Round(MinDoublePoint.y*Scale), Round(VisRect.Top)) - 3;
  end
  else begin
    ABottomScroll.Min := Min(Round(MinDoublePoint.x*Scale), Round(VisRect.Left)) - 3;
    ABottomScroll.Max := Max(Round(MaxDoublePoint.x*Scale), Round(VisRect.Right)) + 3;
    ARightScroll.Min := Min(Round(MinDoublePoint.y*Scale), Round(VisRect.Top)) - 3;
    ARightScroll.Max := Max(Round(MaxDoublePoint.y*Scale), Round(VisRect.Bottom)) + 3;
  end;
  ABottomScroll.Position := Round(VisRect.Left);
  ARightScroll.Position := Round(VisRect.Top);
end;

procedure ChangeMaxMinScrollBarOnMove(var ABottomScroll, ARightScroll: TScrollBar;
  AWidth, AHeight: integer);
begin
  with VisRect do begin
    Left := Offset.x*Scale;
    Top :=  Offset.y*Scale;
    Right := Offset.x*Scale + AWidth;
    Bottom := Offset.y*Scale + AHeight;
  end;
  ABottomScroll.Max := Max(Round(MaxDoublePoint.x*Scale), Round(VisRect.Right)) + 3;
  ABottomScroll.Min := Min(Round(MinDoublePoint.x*Scale), Round(VisRect.Left)) - 3;
  ARightScroll.Max := Max(Round(MaxDoublePoint.y*Scale), Round(VisRect.Bottom)) + 3;
  ARightScroll.Min := Min(Round(MinDoublePoint.y*Scale), Round(VisRect.Top)) - 3;
  ABottomScroll.Position := Round(VisRect.Left);
  ARightScroll.Position := Round(VisRect.Top);
end;

procedure SetNewOffSet(AWidth, AHeight: integer; OldScale: double);
begin
  Offset.x += AWidth*(1/OldScale - 1/Scale)/2;
  Offset.y += AHeight*(1/OldScale - 1/Scale)/2;
end;

procedure SetOffsetOnScroll(var AScroll: TScrollBar; AMinVisRect, AMaxVisRect,
  MinPoint, MaxPoint: double);
begin
  AScroll.Max := Max(Round(MaxPoint*Scale), Round(AMaxVisRect*Scale)) + 3;
  AScroll.Min := Min(Round(MinPoint*Scale), Round(AMinVisRect*Scale)) - 3;
end;

end.
