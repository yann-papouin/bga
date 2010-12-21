inherited FSViewForm: TFSViewForm
  Caption = 'File system View'
  ClientHeight = 442
  ClientWidth = 622
  Position = poScreenCenter
  ExplicitWidth = 630
  ExplicitHeight = 469
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 23
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
      ItemHeight = 13
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
  object TopDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 622
    Height = 23
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
  object Actions: TActionList
    Images = ResourcesForm.Images16x16
    Left = 8
    Top = 64
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
