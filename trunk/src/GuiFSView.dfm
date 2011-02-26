inherited FSViewForm: TFSViewForm
  Caption = 'File system View'
  ClientHeight = 442
  ClientWidth = 622
  Position = poScreenCenter
  ExplicitWidth = 638
  ExplicitHeight = 480
  PixelsPerInch = 96
  TextHeight = 13
  inherited Container: TSpTBXPanel
    Top = 42
    Width = 622
    Height = 400
    ExplicitTop = 58
    ExplicitWidth = 622
    ExplicitHeight = 384
    inherited RFAList: TVirtualStringTree
      Width = 622
      Height = 329
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
      ExplicitWidth = 622
      ExplicitHeight = 349
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
          Width = 15
          WideText = 'Offset'
        end>
    end
    inherited SearchBar: TSpTBXPanel
      Top = 329
      Width = 622
      ExplicitTop = 349
      ExplicitWidth = 622
      inherited SearchEdit: TSpTBXEdit
        Width = 187
      end
      inherited SearchProgressBar: TSpTBXProgressBar
        Left = 273
        ExplicitLeft = 273
      end
    end
    object Footer: TSpTBXPanel
      AlignWithMargins = True
      Left = 0
      Top = 364
      Width = 622
      Height = 36
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      UseDockManager = True
      Padding.Left = 3
      Padding.Right = 3
      Padding.Bottom = 3
      TabOrder = 2
      Borders = False
      object ButtonOk: TSpTBXButton
        AlignWithMargins = True
        Left = 382
        Top = 3
        Width = 114
        Height = 27
        Action = Ok
        Align = alRight
        TabOrder = 0
        Images = ResourcesForm.Images16x16
        ImageIndex = 1118
        ExplicitLeft = 385
      end
      object ButtonCancel: TSpTBXButton
        AlignWithMargins = True
        Left = 502
        Top = 3
        Width = 114
        Height = 27
        Action = Cancel
        Align = alRight
        TabOrder = 1
        Images = ResourcesForm.Images16x16
        ImageIndex = 143
        ExplicitLeft = 505
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
        ExplicitLeft = 385
      end
    end
  end
  object Panel2: TPanel [1]
    Left = 0
    Top = 9
    Width = 622
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 25
    DesignSize = (
      622
      33)
    object SpTBXButton2: TSpTBXButton
      Left = 545
      Top = 6
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
      Top = 6
      Width = 428
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
    end
    object SpTBXLabel2: TSpTBXLabel
      Left = 7
      Top = 6
      Width = 98
      Height = 22
      Caption = 'Mod selection :'
      Images = ResourcesForm.Images16x16
      ImageIndex = 1181
    end
  end
  object TopDock: TSpTBXDock [2]
    Left = 0
    Top = 0
    Width = 622
    Height = 9
  end
  inherited Actions: TActionList
    object Load: TAction
      Category = 'Custom'
      Caption = 'Load'
      ImageIndex = 318
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
