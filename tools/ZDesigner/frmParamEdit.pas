unit frmParamEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Contnrs, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, ZClasses, ZExpressions;

type
  TValueChanged = procedure(Sender: TObject; Index: Integer; Value: Single) of object;

  TParameter = class
  private
    FIndex: Integer;
    FTrackbar: TTrackBar;
    FLabel: TLabel;
    FOnValueChanged: TValueChanged;
    procedure TrackBarChange(Sender: TObject);
  public
    constructor Create(Parent: TWinControl; Index: Integer); virtual;
    destructor Destroy; override;

    property Trackbar: TTrackBar read FTrackbar;
    property ParamLabel: TLabel read FLabel;
    property OnValueChanged: TValueChanged read FOnValueChanged write FOnValueChanged;
  end;

  TParamEditForm = class(TForm)
    LabelAudio: TLabel;
    ComboBoxAudio: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FParams: TObjectList;
    FZParameters: TDefineArray;
    procedure AddParameter(Text: String; Index: Integer; Value: Single);
    procedure ValueChangedHandler(Sender: TObject; Index: Integer; Value: Single);
    function LocateZParameterz(ComponentList: TZComponentList): TDefineArray;
  public
    procedure UpdateParameters(ComponentList: TZComponentList);
  end;

implementation

{$R *.dfm}

uses frmEditor;

{ TParameter }

constructor TParameter.Create(Parent: TWinControl; Index: Integer);
begin
  inherited Create;

  FIndex := Index;

  FTrackBar := TTrackBar.Create(Parent);
  FTrackBar.Left := 97;
  FTrackBar.Width := 145;
  FTrackBar.Height := 22;
  FTrackBar.Max := 1000;
  FTrackBar.PageSize := 100;
  FTrackBar.Frequency := 100;
  FTrackBar.TabOrder := 0;
  FTrackBar.ThumbLength := 12;
  FTrackBar.Parent := Parent;
  FTrackbar.OnChange := TrackBarChange;
  FTrackbar.Anchors := [akLeft, akTop, akRight];
  FLabel := TLabel.Create(Parent);
  FLabel.Parent := Parent;
  FLabel.Left := 8;
  FLabel.Width := 63;
  FLabel.Height := 22;
end;

destructor TParameter.Destroy;
begin
  FTrackbar.Free;
  FLabel.Free;

  inherited;
end;

procedure TParameter.TrackBarChange(Sender: TObject);
var
  NormalizedValue: Single;
begin
  NormalizedValue := (FTrackBar.Position - FTrackBar.Min) / (FTrackBar.Max - FTrackBar.Min);
  if Assigned(FOnValueChanged) then
    FOnValueChanged(Self, FIndex, NormalizedValue);
end;


{ TParamEditForm }

procedure TParamEditForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
  EditorForm.ParameterEditMenuItem.Checked := not Visible;
end;

procedure TParamEditForm.FormCreate(Sender: TObject);
begin
  FParams := TObjectList.Create;
  FParams.OwnsObjects := True;
end;

procedure TParamEditForm.FormDestroy(Sender: TObject);
begin
  FParams.Free;
end;

procedure TParamEditForm.AddParameter(Text: String; Index: Integer; Value: Single);
var
  Param: TParameter;
begin
  Param := TParameter.Create(Self, Index);
  Param.ParamLabel.Caption := Text;
  Param.ParamLabel.Top := 40 + FParams.Count * (Param.Trackbar.Height + 8);
  Param.Trackbar.Top := Param.ParamLabel.Top;
  Param.Trackbar.Position := Round(1000 * Value);
  Param.OnValueChanged := ValueChangedHandler;
  FParams.Add(Param);
end;

function ScanComponentList(const Name: String; List: TZComponentList): TZComponent;
var
  Index: Integer;
begin
  for Index := 0 to List.Count - 1 do
  begin
    if List.GetComponent(Index).Name = Name then
      Result := List.GetComponent(Index)
    else
    if List.GetComponent(Index) is TLogicalGroup then
    begin
      Result := ScanComponentList(Name, TLogicalGroup(List.GetComponent(Index)).Children);
      if Result <> nil then
        exit;
    end;
  end;
end;

procedure TParamEditForm.ValueChangedHandler(Sender: TObject; Index: Integer; Value: Single);
var
  ParamValue: PFloat;
  CurrentIndex: Integer;
begin
  ParamValue := FZParameters.GetData;

  for CurrentIndex := 0 to Index - 1 do
    Inc(ParamValue);

  ParamValue^ := Value;
end;

function TParamEditForm.LocateZParameterz(ComponentList: TZComponentList): TDefineArray;
var
  ParametersComponent: TZComponent;
begin
  ParametersComponent := ScanComponentList('Parameters', ComponentList);
  Result := ParametersComponent as TDefineArray;
  Assert(Result.SizeDim2 = 0);
  Assert(Result.SizeDim3 = 0);
end;

procedure TParamEditForm.UpdateParameters(ComponentList: TZComponentList);
var
  ParametersComponent, ParamHelpConstComponent: TZComponent;
  Parameters: TDefineArray;
  ParamHelpConst: TDefineConstant;
  ParamHelpList: TStringList;
  ParamValue: PFloat;
  LastParam: TParameter;
  Index: Integer;
begin
  while FParams.Count > 0 do
  begin
    FParams[0].Free;
    FParams.Delete(0);
  end;

  FZParameters := LocateZParameterz(ComponentList);
  ParamValue := FZParameters.GetData;

  ParamHelpConstComponent := ScanComponentList('ParamHelpConst', ComponentList);
  ParamHelpConst := ParamHelpConstComponent as TDefineConstant;
  Assert(ParamHelpConst._Type = zctString);

  ParamHelpList := TStringList.Create;
  try
    ParamHelpList.Text := ParamHelpConst.StringValue;
    for Index := 0 to ParamHelpList.Count - 1 do
    begin
      AddParameter(ParamHelpList.Strings[Index], Index, ParamValue^);
      Inc(ParamValue);
    end;
  finally
    ParamHelpList.Free;
  end;

  LastParam := TParameter(FParams[FParams.Count - 1]);
  ClientHeight := LastParam.Trackbar.Top + LastParam.Trackbar.Height + 8;
end;

end.

