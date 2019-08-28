unit frmMemoEdit;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMemoEditForm = class(TForm)
    Memo1: TMemo;
    OkButton: TButton;
    Button2: TButton;
  end;

var
  MemoEditForm: TMemoEditForm;

implementation

{$R *.dfm}

end.
