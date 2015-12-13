unit UCopyCutInsert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DrawFigure, UAllTools, TypInfo, Clipbrd, USaveAndOpen;

var
  FigureInMemory: TClipBoard;

procedure CopyFigures(var AStringList: TStringList);
procedure DeleteFigures(index: integer);
procedure SwapFigures(var Figure1, Figure2: TFigure);

implementation

procedure CopyFigures(var AStringList: TStringList);
var
  i: integer;
  AddArrayOfFigures: array of TFigure = nil;
begin
  for i := 0 to High(ArrayOfFigures) do begin
    if ArrayOfFigures[i].FSelect then begin
      SetLength(AddArrayOfFigures, Length(AddArrayOfFigures) + 1);
      AddArrayOfFigures[High(AddArrayOfFigures)] := ArrayOfFigures[i];
    end;
  end;
  SaveMyFile(AStringList, AddArrayOfFigures);
end;

procedure DeleteFigures(index: integer);
var
  i: integer;
begin
  for i := index to High(ArrayOfFigures) - 1 do begin
    ArrayOfFigures[i] := nil;
    ArrayOfFigures[i] := ArrayOfFigures[i + 1];
  end;
  ArrayOfFigures[High(ArrayOfFigures)] := nil;
  SetLength(ArrayOfFigures, Length(ArrayOfFigures) - 1);
end;

procedure SwapFigures(var Figure1, Figure2: TFigure);
var
  Figure: TFigure;
begin
  Figure := Figure2;
  Figure2 := Figure1;
  Figure1 := Figure;
end;

end.
