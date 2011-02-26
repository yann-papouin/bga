inherited FSViewForm: TFSViewForm
  Caption = 'File system View'
  ClientHeight = 442
  ClientWidth = 745
  Position = poScreenCenter
  ExplicitWidth = 761
  ExplicitHeight = 480
  PixelsPerInch = 96
  TextHeight = 13
  inherited Container: TSpTBXPanel
    Top = 42
    Width = 745
    Height = 400
    ExplicitTop = 42
    ExplicitWidth = 622
    ExplicitHeight = 400
    inherited RFAList: TVirtualStringTree
      Width = 745
      Height = 329
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
      ExplicitWidth = 622
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
          Position = 4
          Width = 138
          WideText = 'Offset'
        end>
    end
    inherited SearchBar: TSpTBXPanel
      Top = 329
      Width = 745
      ExplicitTop = 329
      ExplicitWidth = 622
      inherited SearchEdit: TSpTBXEdit
        Width = 310
        ExplicitWidth = 187
      end
      inherited SearchProgressBar: TSpTBXProgressBar
        Left = 396
        ExplicitLeft = 273
      end
    end
    object Footer: TSpTBXPanel
      AlignWithMargins = True
      Left = 0
      Top = 364
      Width = 745
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
      ExplicitWidth = 622
      object ButtonOk: TSpTBXButton
        AlignWithMargins = True
        Left = 505
        Top = 3
        Width = 114
        Height = 27
        Action = Ok
        Align = alRight
        TabOrder = 0
        Images = ResourcesForm.Images16x16
        ImageIndex = 1118
        ExplicitLeft = 382
      end
      object ButtonCancel: TSpTBXButton
        AlignWithMargins = True
        Left = 625
        Top = 3
        Width = 114
        Height = 27
        Action = Cancel
        Align = alRight
        TabOrder = 1
        Images = ResourcesForm.Images16x16
        ImageIndex = 143
        ExplicitLeft = 502
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
        ImageIndex = 330
      end
    end
  end
  object Panel2: TSpTBXPanel [1]
    Left = 0
    Top = 0
    Width = 745
    Height = 42
    Align = alTop
    TabOrder = 1
    Borders = False
    TBXStyleBackground = True
    ExplicitTop = 9
    DesignSize = (
      745
      42)
    object SpTBXButton2: TSpTBXButton
      Left = 668
      Top = 10
      Width = 68
      Height = 21
      Action = Load
      Anchors = [akTop, akRight]
      TabOrder = 0
      Images = ResourcesForm.Images16x16
      ImageIndex = 318
    end
    object ModList: TSpTBXComboBox
      Left = 111
      Top = 10
      Width = 551
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      OnChange = ModListChange
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
      ImageIndex = 330
      OnExecute = SettingsExecute
    end
  end
end
