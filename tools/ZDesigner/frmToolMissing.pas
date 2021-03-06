unit frmToolMissing;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TToolMissingForm = class(TForm)
    Button1: TButton;
    DownloadURLLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ExeNameLabel: TLabel;
    ToolPathLabel: TLabel;
    ToolNameLabel: TLabel;
    procedure DownloadURLLabelClick(Sender: TObject);
    procedure ToolPathLabelMouseEnter(Sender: TObject);
    procedure ToolPathLabelMouseLeave(Sender: TObject);
  end;

var
  ToolMissingForm: TToolMissingForm;

implementation

{$R *.dfm}

uses ShellApi,uHelp;

procedure TToolMissingForm.DownloadURLLabelClick(Sender: TObject);
begin
  GoUrl( (Sender as TLabel).Caption );
end;

procedure TToolMissingForm.ToolPathLabelMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Color := clBlue;
end;

procedure TToolMissingForm.ToolPathLabelMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Color := clBlack;
end;

end.
