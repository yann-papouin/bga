inherited BrowseExtractForm: TBrowseExtractForm
  Caption = 'Extract to folder'
  ClientHeight = 389
  ExplicitWidth = 400
  ExplicitHeight = 427
  PixelsPerInch = 96
  TextHeight = 13
  inherited Recents: TSpTBXListBox
    Top = 136
    ExplicitTop = 136
  end
  inherited Footer: TPanel
    Top = 346
  end
  inherited SpTBXLabel2: TSpTBXLabel
    Top = 108
    ExplicitTop = 108
  end
  object RecreateFullPath: TSpTBXCheckBox [5]
    Left = 8
    Top = 67
    Width = 218
    Height = 21
    Caption = 'Recreate full path from root for each file'
    TabOrder = 5
    Checked = True
    State = cbChecked
  end
  inherited Browse: TJvBrowseForFolderDialog
    Top = 216
  end
  inherited FormStorage: TJvFormStorage
    StoredProps.Strings = ()
    Top = 248
  end
  inherited Actions: TActionList
    Top = 280
  end
end
