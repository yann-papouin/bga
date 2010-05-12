object BrowseForm: TBrowseForm
  Left = 0
  Top = 0
  Caption = 'Browse for folder'
  ClientHeight = 366
  ClientWidth = 384
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnShow = FormShow
  DesignSize = (
    384
    366)
  PixelsPerInch = 96
  TextHeight = 13
  object Folder: TSpTBXButtonEdit
    AlignWithMargins = True
    Left = 8
    Top = 40
    Width = 368
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = FolderChange
    EditButton.Left = 345
    EditButton.Top = 0
    EditButton.Width = 19
    EditButton.Height = 17
    EditButton.Caption = '...'
    EditButton.Align = alRight
    EditButton.OnClick = EditFolder
  end
  object Recents: TSpTBXListBox
    AlignWithMargins = True
    Left = 8
    Top = 104
    Width = 368
    Height = 184
    Anchors = [akLeft, akTop, akRight, akBottom]
    IntegralHeight = True
    ItemHeight = 20
    TabOrder = 1
    OnClick = RecentsClick
    OnKeyDown = RecentsKeyDown
  end
  object Footer: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 323
    Width = 384
    Height = 43
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 2
    object ButtonOk: TSpTBXButton
      AlignWithMargins = True
      Left = 142
      Top = 8
      Width = 114
      Height = 27
      Action = Ok
      Align = alRight
      TabOrder = 0
      Images = RFAViewForm.ExplorerImg
      ImageIndex = 1118
    end
    object ButtonCancel: TSpTBXButton
      AlignWithMargins = True
      Left = 262
      Top = 8
      Width = 114
      Height = 27
      Action = Cancel
      Align = alRight
      TabOrder = 1
      Images = RFAViewForm.ExplorerImg
      ImageIndex = 143
    end
  end
  object SpTBXLabel1: TSpTBXLabel
    Left = 8
    Top = 12
    Width = 116
    Height = 22
    Caption = 'Current directory :'
    Images = RFAViewForm.ExplorerImg
    ImageIndex = 8
  end
  object SpTBXLabel2: TSpTBXLabel
    Left = 8
    Top = 76
    Width = 103
    Height = 22
    Caption = 'Recent folders :'
    Images = RFAViewForm.ExplorerImg
    ImageIndex = 165
  end
  object Browse: TJvBrowseForFolderDialog
    Left = 16
    Top = 112
  end
  object FormStorage: TJvFormStorage
    AppStorage = RFAViewForm.AppStorage
    AppStoragePath = 'BrowseCommon\'
    Options = [fpSize]
    StoredProps.Strings = (
      'Recents.Items'
      'Folder.Text')
    StoredValues = <>
    Left = 16
    Top = 144
  end
  object Actions: TActionList
    Images = RFAViewForm.ExplorerImg
    Left = 16
    Top = 176
    object Ok: TAction
      Caption = 'Ok'
      ImageIndex = 1118
      OnExecute = OkExecute
    end
    object Cancel: TAction
      Caption = 'Cancel'
      ImageIndex = 143
      OnExecute = CancelExecute
    end
  end
end
