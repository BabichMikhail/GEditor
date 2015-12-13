unit UChangeParamFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DrawFigure, UAllTools, TypInfo, Graphics, USaveAndOpen;

procedure ChangePenColorSelectFigures(APenColor: TColor);
procedure ChangeBrushColorSelectFigures(ABrushColor: TColor);
procedure DeselectFigures;

implementation

procedure ChangePenColorSelectFigures(APenColor: TColor);
var
  i: integer;
begin
  if ArrayOfFigures <> nil then
    for i := 0 to High(ArrayOfFigures) do
      if ArrayOfFigures[i].FSelect then
        SetPropValue(ArrayOfFigures[i], 'PenColor', APenColor);
  SetStrHist;
end;

procedure ChangeBrushColorSelectFigures(ABrushColor: TColor);
var
  i: integer;
begin
  if ArrayOfFigures <> nil then
    for i := 0 to High(ArrayOfFigures) do
      if ArrayOfFigures[i].FSelect and (ArrayOfFigures[i].ClassParent = TBrushFigure) then
        SetPropValue(ArrayOfFigures[i], 'BrushColor', ABrushColor);
  SetStrHist;
end;

procedure DeselectFigures;
var
  i: integer;
begin
  for i := 0 to High(ArrayOfFigures) do
    ArrayOfFigures[i].FSelect := False;
end;

end.
