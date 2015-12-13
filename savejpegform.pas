unit SaveJPEGForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TAskForm }

  TAskForm = class(TForm)
    OKbutton: TButton;
    WidthEdit: TEdit;
    IndentEdit: TEdit;
    AskLabel1: TLabel;
    Label2: TLabel;
    procedure IndentEditChange(Sender: TObject);
    procedure OKbuttonClick(Sender: TObject);
    procedure WidthEditChange(Sender: TObject);
    procedure WidthEditKeyPress(Sender: TObject; var Key: char);
  end;

var
  AskForm: TAskForm;
  WidthImage: integer = 1024;
  IndentImage: integer = 30;

implementation

{$R *.lfm}

{ TAskForm }

procedure TAskForm.IndentEditChange(Sender: TObject);
begin
  if IndentEdit.Text = '' then
    IndentEdit.Text := '30';
  IndentImage := StrToInt(IndentEdit.Caption);
end;

procedure TAskForm.OKbuttonClick(Sender: TObject);
begin
  Close;
end;

procedure TAskForm.WidthEditChange(Sender: TObject);
begin
  if WidthEdit.Text = '' then
    WidthEdit.Text := '1024';
  WidthImage := StrToInt(WidthEdit.Text);
end;

procedure TAskForm.WidthEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (key in ['0'..'9', #8]) then
    key := #0;
end;

end.

