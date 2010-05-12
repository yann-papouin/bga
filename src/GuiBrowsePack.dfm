inherited BrowsePackForm: TBrowsePackForm
  Caption = 'Pack directory'
  ClientHeight = 362
  ExplicitHeight = 400
  PixelsPerInch = 96
  TextHeight = 13
  inherited Recents: TSpTBXListBox
    Top = 176
    Height = 124
    ExplicitTop = 176
    ExplicitHeight = 124
  end
  inherited Footer: TPanel
    Top = 319
    ExplicitTop = 319
  end
  inherited SpTBXLabel2: TSpTBXLabel
    Top = 148
    ExplicitTop = 148
  end
  object Base: TSpTBXEdit [5]
    Left = 8
    Top = 103
    Width = 368
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    Text = 'bf1942'
  end
  object UseBasePath: TSpTBXCheckBox [6]
    Left = 8
    Top = 76
    Width = 100
    Height = 21
    Caption = 'Use base path :'
    TabOrder = 6
    OnClick = UseBasePathClick
    Checked = True
    State = cbChecked
  end
  inherited Browse: TJvBrowseForFolderDialog
    Top = 184
  end
  inherited FormStorage: TJvFormStorage
    StoredProps.Strings = (
      'Base.Text'
      'UseBasePath.Checked'
      'Folder.Text'
      'Recents.Items')
    Top = 216
  end
  inherited Actions: TActionList
    Top = 248
  end
end
