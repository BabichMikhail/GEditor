unit UInvisbleMethods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDoublePoint, Graphics, DrawFigure;

type

  TControlDo = class
  public
    FPoint1, FPoint2: TDoublePoint;
    procedure FindFigure(AArrayOfFigures: array of TFigure);
    procedure DeselectFigures;
    procedure ChangeFoundSelectPoint(APoint1, APoint2: TDoublePoint);
    function FindSelectPoint(APoint: TDoublePoint): boolean;
    function FindSelectFigure(APoint: TDoublePoint): boolean;
    procedure FindAndMoveFigure(APoint1, APoint2: TDoublePoint);
  end;

var
  IndexFig: integer;

implementation

  procedure TControlDo.FindFigure(AArrayOfFigures: array of TFigure);
  var
    i: integer;
    L: double;//Расстояние между 2мя точками
  begin
    DeselectFigures;
    L := sqrt(sqr(FPoint1.x - FPoint2.x) + sqr(FPoint1.y - FPoint2.y));
    if L <= 4 then
      for i := 0 to High(AArrayOfFigures) do
        ArrayOfFigures[i].FSelect := AArrayOfFigures[i].MouseOnFigure(FPoint1)
    else
      if L > 4 then
        for i := 0 to High(AArrayOfFigures) do
          ArrayOfFigures[i].FSelect := AArrayOfFigures[i].MouseRectOnFigure(FPoint1, FPoint2);
  end;

  procedure TControlDo.DeselectFigures;
  var
    i: integer;
  begin
    for i := 0 to High(ArrayOfFigures) do
      ArrayOfFigures[i].FSelect := False;
  end;

  function TControlDo.FindSelectPoint(APoint: TDoublePoint): boolean;
  var
    i: integer;
  begin
    for i := High(ArrayOfFigures) downto 0 do
      if ArrayOfFigures[i].FSelect then begin
        Result := ArrayOfFigures[i].FoundAnchorsPoint(APoint);
        if Result then begin
          IndexFig := i;
          Exit;
        end;
      end;
    Result := False;
  end;

  function TControlDo.FindSelectFigure(APoint: TDoublePoint): boolean;
  var
    i: integer;
  begin
    for i := High(ArrayOfFigures) downto 0 do begin
      if ArrayOfFigures[i].FSelect then begin
        Result := ArrayOfFigures[i].MouseOnFigure(APoint);
        if Result then
          Exit;
      end;
    end;
    Result := False;
  end;

  procedure TControlDo.ChangeFoundSelectPoint(APoint1, APoint2: TDoublePoint);
  begin
    if ArrayOfFigures[IndexFig].FSelect and ArrayOfFigures[IndexFig].FoundAnchorsPoint(APoint1) then
      ArrayofFigures[IndexFig].ChangePointPosition(APoint1, APoint2);
  end;

  procedure TControlDo.FindAndMoveFigure(APoint1, APoint2: TDoublePoint);
  var
    i: integer;
  begin
    for i := 0 to High(ArrayOFFigures) do
      if ArrayOfFigures[i].FSelect then
        ArrayOfFigures[i].MoveFigure(APoint1, APoint2);
  end;

end.
