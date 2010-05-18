inherited FSViewForm: TFSViewForm
  Caption = 'File system View'
  ClientHeight = 442
  ClientWidth = 622
  Position = poScreenCenter
  ExplicitWidth = 638
  ExplicitHeight = 480
  PixelsPerInch = 96
  TextHeight = 13
  inherited Container: TPanel
    Top = 58
    Width = 622
    Height = 384
    ExplicitTop = 58
    ExplicitWidth = 622
    ExplicitHeight = 384
    inherited RFAList: TVirtualStringTree
      Width = 622
      Height = 349
      ExplicitLeft = 0
      ExplicitTop = 0
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
      Top = 349
      Width = 622
      ExplicitLeft = 0
      ExplicitTop = 349
      ExplicitWidth = 622
      inherited Search: TSpTBXEdit
        Width = 535
        ExplicitWidth = 535
      end
    end
  end
  object Panel2: TPanel [1]
    Left = 0
    Top = 25
    Width = 622
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      622
      33)
    object SpTBXButton2: TSpTBXButton
      Left = 545
      Top = 6
      Width = 70
      Height = 21
      Action = Settings
      Anchors = [akTop, akRight]
      TabOrder = 0
      Images = ResourcesForm.Images16x16
      ImageIndex = 330
    end
    object FilesystemChoice: TSpTBXComboBox
      Left = 72
      Top = 6
      Width = 467
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = FilesystemChoiceChange
    end
    object SpTBXLabel2: TSpTBXLabel
      Left = 7
      Top = 8
      Width = 59
      Height = 19
      Caption = 'File system'
    end
  end
  object TopDock: TSpTBXDock [2]
    Left = 0
    Top = 0
    Width = 622
    Height = 25
    object tbMenuBar: TSpTBXToolbar
      Left = 0
      Top = 0
      ActivateParent = False
      CloseButton = False
      DefaultDock = TopDock
      DockMode = dmCannotFloatOrChangeDocks
      DockPos = 0
      DragHandleStyle = dhNone
      FullSize = True
      Images = ResourcesForm.Images16x16
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      TabOrder = 0
      Caption = 'Menu'
      Customizable = False
      MenuBar = True
      object mFile: TSpTBXSubmenuItem
        Caption = 'File system'
        object SpTBXItem14: TSpTBXItem
          Action = Add
        end
        object SpTBXItem2: TSpTBXItem
          Action = Import
        end
        object SpTBXSeparatorItem1: TSpTBXSeparatorItem
        end
        object SpTBXItem1: TSpTBXItem
          Action = Update
        end
      end
      object mEdit: TSpTBXSubmenuItem
        Caption = 'Edit'
        object SpTBXItem23: TSpTBXItem
          Action = Settings
        end
      end
    end
  end
  inherited Actions: TActionList
    object Settings: TAction
      Category = 'Custom'
      Caption = 'Settings'
      ImageIndex = 330
      OnExecute = SettingsExecute
    end
    object Add: TAction
      Category = 'Custom'
      Caption = 'Add'
      ImageIndex = 179
      OnExecute = AddExecute
    end
    object Import: TAction
      Category = 'Custom'
      Caption = 'Import'
      ImageIndex = 61
      OnExecute = ImportExecute
    end
    object Update: TAction
      Category = 'Custom'
      Caption = 'Update'
      ImageIndex = 94
      OnExecute = UpdateExecute
    end
  end
end
