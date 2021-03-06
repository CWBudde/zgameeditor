object SettingsForm: TSettingsForm
  Left = 206
  Top = 110
  BorderIcons = [biSystemMenu]
  Caption = 'Settings'
  ClientHeight = 476
  ClientWidth = 443
  Color = clBtnFace
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    443
    476)
  PixelsPerInch = 96
  TextHeight = 13
  object OkButton: TButton
    Left = 280
    Top = 444
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = OkButtonClick
    ExplicitTop = 394
  end
  object Button2: TButton
    Left = 362
    Top = 444
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitTop = 394
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 429
    Height = 426
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    ExplicitHeight = 376
    object TabSheet1: TTabSheet
      Caption = 'General'
      ExplicitHeight = 348
      DesignSize = (
        421
        398)
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 404
        Height = 154
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Packer settings'
        TabOrder = 0
        object Label1: TLabel
          Left = 14
          Top = 32
          Width = 35
          Height = 13
          Caption = 'Presets'
        end
        object Label2: TLabel
          Left = 14
          Top = 59
          Width = 39
          Height = 13
          Caption = 'Program'
        end
        object Label3: TLabel
          Left = 14
          Top = 101
          Width = 50
          Height = 13
          Caption = 'Arguments'
        end
        object Label4: TLabel
          Left = 70
          Top = 80
          Width = 194
          Height = 13
          Caption = '{$toolpath} = Path to ZGameEditor\Tools'
        end
        object Label5: TLabel
          Left = 70
          Top = 122
          Width = 164
          Height = 13
          Caption = '{$exename} = Name of executable'
        end
        object PackerEdit: TEdit
          Left = 70
          Top = 56
          Width = 235
          Height = 21
          TabOrder = 0
          Text = 'PackerEdit'
        end
        object PackerParamsEdit: TEdit
          Left = 70
          Top = 99
          Width = 235
          Height = 21
          TabOrder = 1
          Text = 'PackerParamsEdit'
        end
        object PackerPresetCombo: TComboBox
          Left = 70
          Top = 29
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 2
          OnChange = PackerPresetComboChange
          Items.Strings = (
            'Upx default'
            'Upx lzma'
            'Upx best'
            'Upx brute'
            'kkrunchy default'
            'kkrunchy best')
        end
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 160
        Width = 404
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'File association'
        TabOrder = 1
        object ShellCheck: TCheckBox
          Left = 10
          Top = 16
          Width = 255
          Height = 17
          Caption = 'Associate .zgeproj file extension with explorer'
          TabOrder = 0
          OnClick = ShellCheckClick
        end
      end
      object GroupBox3: TGroupBox
        Left = 0
        Top = 215
        Width = 404
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Designer layout'
        TabOrder = 2
        object Label6: TLabel
          Left = 14
          Top = 19
          Width = 185
          Height = 13
          Caption = 'Property editor position (requires restart)'
        end
        object GuiLayoutCombo: TComboBox
          Left = 211
          Top = 16
          Width = 89
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          Items.Strings = (
            'Left'
            'Middle')
        end
      end
      object GroupBox4: TGroupBox
        Left = 0
        Top = 270
        Width = 404
        Height = 59
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Code editor'
        TabOrder = 3
        DesignSize = (
          404
          59)
        object Label7: TLabel
          Left = 14
          Top = 24
          Width = 188
          Height = 13
          Caption = 'Delay for code completion (milliseconds)'
        end
        object UpDown1: TUpDown
          Left = 265
          Top = 22
          Width = 16
          Height = 21
          Anchors = [akLeft, akBottom]
          Associate = CompDelayEdit
          Min = 100
          Max = 10000
          Increment = 100
          Orientation = udHorizontal
          Position = 100
          TabOrder = 0
        end
        object CompDelayEdit: TEdit
          Left = 224
          Top = 22
          Width = 41
          Height = 21
          Anchors = [akLeft, akBottom]
          ReadOnly = True
          TabOrder = 1
          Text = '100'
        end
      end
      object GroupBox5: TGroupBox
        Left = 0
        Top = 335
        Width = 404
        Height = 59
        Anchors = [akLeft, akTop, akRight]
        Caption = 'UI scaling'
        TabOrder = 4
        object Label14: TLabel
          Left = 14
          Top = 24
          Width = 124
          Height = 13
          Caption = 'Scaling % (requires restart)'
        end
        object ScalingCombo: TComboBox
          Left = 211
          Top = 22
          Width = 89
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          Items.Strings = (
            '100'
            '125'
            '150'
            '175'
            '200')
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Android'
      ImageIndex = 1
      DesignSize = (
        421
        398)
      object Label8: TLabel
        Left = 6
        Top = 19
        Width = 89
        Height = 13
        Caption = 'Android SDK Path:'
      end
      object Label9: TLabel
        Left = 6
        Top = 46
        Width = 101
        Height = 13
        Caption = 'Android Sdcard Path:'
      end
      object Label10: TLabel
        Left = 6
        Top = 73
        Width = 84
        Height = 13
        Caption = 'Apache Ant Path:'
      end
      object Label11: TLabel
        Left = 3
        Top = 161
        Width = 63
        Height = 13
        Caption = 'Keystore File:'
      end
      object Label12: TLabel
        Left = 3
        Top = 188
        Width = 69
        Height = 13
        Caption = 'Keystore Alias:'
      end
      object Label13: TLabel
        Left = 114
        Top = 139
        Width = 72
        Height = 13
        Caption = 'Signing options'
      end
      object AndroidSdkPathEdit: TEdit
        Left = 114
        Top = 16
        Width = 260
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object AndroidSdCardPathEdit: TEdit
        Left = 114
        Top = 43
        Width = 260
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object AndroidAntPathEdit: TEdit
        Left = 114
        Top = 70
        Width = 260
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object AndroidKeystorePathEdit: TEdit
        Left = 114
        Top = 158
        Width = 260
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
      end
      object AndroidKeystoreAliasEdit: TEdit
        Left = 114
        Top = 185
        Width = 79
        Height = 21
        TabOrder = 4
      end
    end
  end
end
