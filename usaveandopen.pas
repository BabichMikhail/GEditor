unit USaveAndOpen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DrawFigure, Dialogs, TypInfo, StrUtils, Graphics, UDoublePoint, SaveJPEGForm, forms;

procedure OpenMyFile(var AStringList: TStringList);
procedure SaveMyFile(var AStringList: TStringList; AArrayOfFigures: array of TFigure);
procedure SaveMGEFile(var AStringList: TStringList; AArrayOfFigures: array of TFigure);
procedure SaveSVGFile(var AStringList: TStringList);
procedure SaveJPEGFile;
procedure SaveFigure(AFigure: TFigure; var AStringList: TStringList);
procedure GetStrHist;
procedure SetStrHist;
function PictureChange(APicturePos: integer): boolean;

var
  History: array of TStringList;
  HistPos: integer;
  InvalidatePaintBox: TNotifyEvent;
  ChangeCapt: TEasyEvent;
  FullFileName: AnsiString;
  PictureAlreadySaveToFile, PictureSaveInThisProc: boolean;

implementation

{Open}
procedure OpenMyFile(var AStringList: TStringList);
var
  i, j, StrNowIndex, NSim: integer;
  StrInProc, PropNumber, PropName: AnsiString;
  FigureClass: TFiguresClass;
begin
  StrNowIndex := 1;
  i := AStringList.Count;
  try
    if (AStringList.Count > 0) and (AStringList.Strings[i - 1] <> 'End StringList.') then
      raise Exception.Create('Данные в буфере обмена были повреждены и их чтение невозможно');
    while AStringList.Strings[StrNowIndex] <> 'End StringList.' do begin
      StrInProc := AStringList.Strings[StrNowIndex];
      NSim := 0;
      if StrInProc <> '  End Figure.' then begin
        i := Pos('=', AStringList.Strings[StrNowIndex]);
        PropName := Copy(StrInProc, 0, i - 2);
        PropNumber := Copy(StrInProc, i + 2, Length(StrInProc) - (i + 1));
        if PropName = '  Figure' then begin
          Delete(PropName, 1, 2);
          SetLength(ArrayOfFigures, Length(ArrayOfFigures) + 1);
          FigureClass := GetFigureClass(PropNumber);
          ArrayOfFigures[High(ArrayOfFigures)] := FigureClass.Create;
          ArrayOfFigures[High(ArrayOfFigures)].FSelect := True;
        end
        else begin
          NSim := -1;
          if PropName = '    FPoints' then begin
            NSim := 0;
            for j := 1 to Length(PropNumber) do
              if PropNumber[j] = '&' then
                NSim += 1;
            SetLength(ArrayOfFigures[High(ArrayOfFigures)].FPoints, NSim);
            for j := 0 to High(ArrayOfFigures[High(ArrayOfFigures)].FPoints) do
              with ArrayOfFigures[High(ArrayOfFigures)].FPoints[j] do begin
                  i := Pos('&', PropNumber);
                  x := StrToFloat(Copy(PropNumber, 2, i - 2));
                  y := StrToFloat(Copy(PropNumber, i + 2, Pos(')', PropNumber) - i - 2));
                  Delete(PropNumber, 1, Pos(')', PropNumber) + 2);
              end;
          end
          else begin
            Delete(PropName, 1, 4);
            SetPropValue(ArrayOfFigures[High(ArrayOfFigures)], PropName, PropNumber);
          end;
        end;
      end;
      StrNowIndex += 1;
    end;
  except
    AStringList.Free;
    AStringList := nil;
  end;
end;

{Save}
procedure SaveMyFile(var AStringList: TStringList; AArrayOfFigures: array of TFigure);
var
  i: integer;
  FileType: string;
begin
  i := RPos('.', FullFileName);
  FileType := Copy(FullFileName, i + 1, Length(FullFileName) - i);
  case FileType of
    'mge': SaveMGEFile(AStringList, AArrayOfFigures);
    'svg': SaveSVGFile(AStringList);
    'jpeg': SaveJPEGFile;
  end;
end;

procedure SaveMGEFile(var AStringList: TStringList; AArrayOfFigures: array of TFigure);
var
  i: integer;
begin
  AStringList := TStringList.Create;
  AStringList.Add('Графический редактор. Автор: Бабич Михаил. Группа Б8103а. 2013год. ДВФУ');
  with AStringList do begin
    for i := 0 to High(AArrayOfFigures) do
      SaveFigure(AArrayOfFigures[i], AStringList);
    Add('End StringList.');
  end;
end;

procedure SaveFigure(AFigure: TFigure; var AStringList: TStringList);
var
  i: integer;
  Count: integer;
  Prop: PPropInfo;
  PropList: PPropList;
  Str: AnsiString;
begin
  Count := GetTypeData(AFigure.ClassInfo)^.PropCount;
  GetMem(PropList, SizeOf(Prop)*Count);
  GetPropInfos(AFigure.ClassInfo, PropList);
  Str := '  Figure = ' + AFigure.ClassName;
  AStringList.Add(Str);
  Str := '    FPoints =';
  for i := 0 to High(AFigure.FPoints) do
    //Str += format('(%e& %e);', [AFigure.FPoints[i].x, AFigure.FPoints[i].y]);
    Str += ' ' + '(' + FloatToStr(AFigure.FPoints[i].x) + '& ' + FloatToStr(AFigure.FPoints[i].y) + ');';
  AStringList.Add(Str);
  for i := 0 to Count - 1  do begin
    Prop := PropList^[i];
    Str := '    ' + Prop^.Name + ' = ';
    Str += String(GetPropValue(AFigure, Prop^.Name));
    AStringList.Add(Str);
  end;
  AStringList.Add('  End Figure.');
  FreeMem(PropList, SizeOf(Prop)*Count);
end;

procedure SaveSVGFile(var AStringList: TStringList);
var
  i: integer;
  Str: string;
begin
  AStringList := TStringList.Create;
  try
    if Length(ArrayOfFigures) = 0 then
      raise Exception.Create('Невозможно сохранить, поскольку отсутствуют фигуры');
    with AStringList do begin
      Str := 'transform="translate(' + IntToStr(-Round(MinDoublePoint.x) + 50) + ',' +
        IntToStr(-Round(MinDoublePoint.y) + 50) + ')"';
      Add('<svg' + ' version="1.1" xmlns="http://www.w3.org/2000/svg" ' + Str + '>');
      Add('<desc>Графический редактор. Автор: Бабич Михаил. Группа Б8103а. 2013год. ДВФУ</desc>');
      Add('<g stroke-linecap="round" stroke-linejoin="round">');
      Str := '';
      for i := 0 to High(ArrayOfFigures) do begin
        ArrayOfFigures[i].GetSVGString(Str);
        Add(Str);
        Str := '';
      end;
      Add('</g></svg>');
    end;
  except
    AStringList.Free
  end;
end;

procedure SaveJPEGFile;
var
  Buffer: TBitmap;
  Height, Width, Indent: integer;
  OldScale: double;
  OldOffset: TDoublePoint;
  i: integer;
begin
  Buffer := TBitmap.Create;
  try
    if AskForm.ShowModal = 1 then begin
      Width := WidthImage;
      Indent := IndentImage;
    end
    else begin
      Width := 512;
      Indent := 30;
    end;
    Width += 2*Indent;
    Height := Round(Width/(MaxDoublePoint.x - MinDoublePoint.x + 2*Indent)*
      (MaxDoublePoint.y - MinDoublePoint.y + 2*Indent));
    OldScale := Scale;
    Scale := Width/(MaxDoublePoint.x - MinDoublePoint.x + 2*Indent);
    OldOffset := Offset;
    Offset.x := MinDoublePoint.x - Indent/Width*Height;
    Offset.y := MinDoublePoint.y - Indent/Width*Height;
    Buffer.SetSize(Width, Height);
    Buffer.Canvas.Pen.Color := clWhite;
    Buffer.Canvas.Brush.Color := clWhite;
    Buffer.Canvas.Rectangle(0, 0, Width, Height);
    for i := 0 to High(ArrayOfFigures) do
      ArrayOfFigures[i].Draw(Buffer.Canvas);
    Buffer.Assign(Buffer);
    Buffer.SaveToFile(FullFileName);
    PictureAlreadySaveToFile := True;
  finally
    Scale := OldScale;
    Offset := OldOffset;
    Buffer.Free;
  end;
  PictureSaveInThisProc := True;
end;

{History}
procedure GetStrHist;
begin
  if HistPos < 0 then begin
    HistPos := 0;
    Exit;
  end;
  if HistPos > High(History) then begin
    HistPos := High(History);
    Exit;
  end;
  SetLength(ArrayOfFigures, 0);
  OpenMyFile(History[HistPos]);
  InvalidatePaintBox(nil);
end;

procedure SetStrHist;
var
  NewHistory: TStringList;
  function ChangeHistory(History1, History2: TStringList): boolean;
  var
    i: integer;
  begin
    Result := False;
    if History1.Count = History2.Count then begin
      for i := 0 to History1.Count - 1 do
        if History1.Strings[i] <> History2.Strings[i] then begin
          Result := True;
          Exit;
        end
        else
          Result := True;
    end
    else
      Result := True;
  end;
begin
  NewHistory := TStringList.Create;
  SaveMGEFile(NewHistory, ArrayOfFigures);
  if (Length(History) = 0) or ChangeHistory(History[HistPos], NewHistory) then begin
    HistPos += 1;
    SetLength(History, Length(History) + 1);
    History[HistPos] := NewHistory;
  end;
  ChangeCapt;
  NewHistory := nil;
  NewHistory.Free;
  InvalidatePaintBox(nil);
end;

function PictureChange(APicturePos: integer): boolean;
begin
  Result := APicturePos = HistPos;
end;

end.
