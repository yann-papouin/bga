inherited FSViewForm: TFSViewForm
  Caption = 'File system View'
  ClientHeight = 442
  ClientWidth = 852
  Position = poScreenCenter
  ExplicitWidth = 868
  ExplicitHeight = 480
  PixelsPerInch = 96
  TextHeight = 13
  inherited Container: TSpTBXPanel
    Top = 42
    Width = 852
    Height = 400
    ExplicitTop = 42
    ExplicitWidth = 890
    ExplicitHeight = 400
    inherited RFAList: TVirtualStringTree
      Width = 852
      Height = 329
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
      OnDblClick = RFAListDblClick
      OnGetHint = RFAListGetHint
      ExplicitWidth = 890
      ExplicitHeight = 329
      Columns = <
        item
          MinWidth = 300
          Position = 0
          Width = 350
          WideText = 'Filename'
        end
        item
          Alignment = taRightJustify
          Position = 1
          Width = 90
          WideText = 'Size'
        end
        item
          Alignment = taRightJustify
          Position = 2
          Width = 90
          WideText = 'Compressed'
        end
        item
          Alignment = taRightJustify
          Position = 3
          Width = 73
          WideText = 'Ratio'
        end
        item
          Alignment = taRightJustify
          Position = 5
          Width = 80
          WideText = 'Offset'
        end
        item
          MinWidth = 150
          Position = 4
          Width = 165
          WideText = 'Archive'
        end>
    end
    inherited SearchBar: TSpTBXPanel
      Top = 329
      Width = 852
      ExplicitTop = 329
      ExplicitWidth = 890
      inherited SearchEdit: TSpTBXEdit
        Width = 417
        ExplicitWidth = 455
      end
      inherited SearchProgressBar: TSpTBXProgressBar
        Left = 503
        ExplicitLeft = 541
      end
    end
    object Footer: TSpTBXPanel
      AlignWithMargins = True
      Left = 0
      Top = 364
      Width = 852
      Height = 36
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      Padding.Left = 3
      Padding.Right = 3
      Padding.Bottom = 3
      TabOrder = 2
      Borders = False
      TBXStyleBackground = True
      ExplicitWidth = 890
      object ButtonOk: TSpTBXButton
        AlignWithMargins = True
        Left = 612
        Top = 3
        Width = 114
        Height = 27
        Action = Ok
        Align = alRight
        TabOrder = 0
        Images = ResourcesForm.Images16x16
        ImageIndex = 1118
        ExplicitLeft = 650
      end
      object ButtonCancel: TSpTBXButton
        AlignWithMargins = True
        Left = 732
        Top = 3
        Width = 114
        Height = 27
        Action = Cancel
        Align = alRight
        TabOrder = 1
        Images = ResourcesForm.Images16x16
        ImageIndex = 143
        ExplicitLeft = 770
      end
      object SpTBXButton3: TSpTBXButton
        AlignWithMargins = True
        Left = 6
        Top = 3
        Width = 114
        Height = 27
        Action = Settings
        Align = alLeft
        TabOrder = 2
        Images = ResourcesForm.Images16x16
        ImageIndex = 498
      end
    end
  end
  object Panel2: TSpTBXPanel [1]
    Left = 0
    Top = 0
    Width = 852
    Height = 42
    Align = alTop
    TabOrder = 1
    Borders = False
    TBXStyleBackground = True
    ExplicitWidth = 890
    DesignSize = (
      852
      42)
    object SpTBXButton2: TSpTBXButton
      Left = 775
      Top = 10
      Width = 68
      Height = 21
      Action = Load
      Anchors = [akTop, akRight]
      TabOrder = 0
      Images = ResourcesForm.Images16x16
      ImageIndex = 318
      ExplicitLeft = 813
    end
    object Mods: TSpTBXComboBox
      Left = 111
      Top = 10
      Width = 658
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      OnChange = ModsChange
      ExplicitWidth = 696
    end
    object SpTBXLabel2: TSpTBXLabel
      Left = 7
      Top = 9
      Width = 98
      Height = 22
      Caption = 'Mod selection :'
      Images = ResourcesForm.Images16x16
      ImageIndex = 1181
    end
  end
  inherited Actions: TActionList
    object Load: TAction
      Category = 'Custom'
      Caption = 'Load'
      Enabled = False
      ImageIndex = 318
      OnExecute = LoadExecute
    end
    object Ok: TAction
      Category = 'Custom'
      Caption = 'Ok'
      ImageIndex = 1118
      OnExecute = OkExecute
    end
    object Cancel: TAction
      Category = 'Custom'
      Caption = 'Cancel'
      ImageIndex = 143
      OnExecute = CancelExecute
    end
    object Settings: TAction
      Category = 'Custom'
      Caption = 'Settings'
      ImageIndex = 498
      OnExecute = SettingsExecute
    end
  end
end
