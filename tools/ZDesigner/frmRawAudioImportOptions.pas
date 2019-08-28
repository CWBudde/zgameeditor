unit frmRawAudioImportOptions;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TImportRawAudioForm = class(TForm)
    sampleFormatListBox: TListBox;
    sampleRateComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
  end;

var
  ImportRawAudioForm: TImportRawAudioForm;

implementation

{$R *.dfm}

procedure TImportRawAudioForm.FormCreate(Sender: TObject);
begin
  sampleFormatListBox.ItemIndex := 0;
end;

end.
