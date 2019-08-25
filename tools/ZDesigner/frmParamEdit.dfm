object ParamEditForm: TParamEditForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Parameter Edit'
  ClientHeight = 39
  ClientWidth = 252
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    252
    39)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelAudio: TLabel
    Left = 8
    Top = 11
    Width = 60
    Height = 13
    Caption = 'Audio Input:'
  end
  object ComboBoxAudio: TComboBox
    Left = 97
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 0
    Text = 'none'
    Items.Strings = (
      'none'
      'white noise')
  end
end
