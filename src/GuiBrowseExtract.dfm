inherited BrowseExtractForm: TBrowseExtractForm
  Caption = 'Extract to folder'
  ClientHeight = 389
  ExplicitHeight = 427
  PixelsPerInch = 96
  TextHeight = 13
  inherited Background: TSpTBXPanel
    Height = 389
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 384
    ExplicitHeight = 389
    inherited Footer: TPanel
      Top = 346
      ExplicitTop = 346
    end
    inherited Recents: TSpTBXListBox
      Top = 136
      ExplicitTop = 136
    end
    inherited SpTBXLabel2: TSpTBXLabel
      Top = 108
      ExplicitTop = 108
    end
    object RecreateFullPath: TSpTBXCheckBox
      Left = 8
      Top = 67
      Width = 218
      Height = 21
      Caption = 'Recreate full path from root for each file'
      TabOrder = 5
      Checked = True
      State = cbChecked
    end
  end
  inherited Browse: TJvBrowseForFolderDialog
    Top = 216
  end
  inherited FormStorage: TJvFormStorage
    StoredProps.Strings = (
      'Folder.Text'
      'Recents.Items'
      'RecreateFullPath.Checked')
    Top = 248
  end
  inherited Actions: TActionList
    Top = 280
  end
end
