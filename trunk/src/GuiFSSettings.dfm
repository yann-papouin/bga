object FSSettingsForm: TFSSettingsForm
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 548
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    433
    548)
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXLabel1: TSpTBXLabel
    Left = 8
    Top = 63
    Width = 127
    Height = 22
    Caption = 'Battlefield directory :'
    Images = ResourcesForm.Images16x16
    ImageIndex = 8
  end
  object BattlefieldDir: TSpTBXButtonEdit
    AlignWithMargins = True
    Left = 8
    Top = 91
    Width = 417
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = BattlefieldDirChange
    EditButton.Left = 394
    EditButton.Top = 0
    EditButton.Width = 19
    EditButton.Height = 17
    EditButton.Caption = '...'
    EditButton.Align = alRight
    EditButton.ExplicitLeft = 476
    ExplicitWidth = 499
  end
  object FilesystemName: TSpTBXEdit
    Left = 8
    Top = 36
    Width = 417
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    ExplicitWidth = 499
  end
  object SpTBXLabel2: TSpTBXLabel
    Left = 8
    Top = 8
    Width = 115
    Height = 22
    Caption = 'File system name :'
    Images = ResourcesForm.Images16x16
    ImageIndex = 522
  end
  object Mods: TSpTBXListBox
    AlignWithMargins = True
    Left = 8
    Top = 146
    Width = 417
    Height = 164
    Anchors = [akLeft, akTop, akRight]
    IntegralHeight = True
    ItemHeight = 20
    TabOrder = 4
    ExplicitWidth = 499
  end
  object SpTBXLabel3: TSpTBXLabel
    Left = 8
    Top = 118
    Width = 185
    Height = 22
    Caption = 'Select a mod for this file system :'
    Images = ResourcesForm.Images16x16
    ImageIndex = 1181
  end
  object SpTBXLabel4: TSpTBXLabel
    Left = 8
    Top = 316
    Width = 230
    Height = 22
    Caption = 'Ordered data paths for the selected mod :'
    Images = ResourcesForm.Images16x16
    ImageIndex = 124
  end
  object ModPath: TSpTBXListBox
    AlignWithMargins = True
    Left = 8
    Top = 344
    Width = 417
    Height = 158
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 20
    TabOrder = 7
  end
  object Footer: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 505
    Width = 433
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
    TabOrder = 8
    ExplicitTop = 323
    ExplicitWidth = 384
    object ButtonOk: TSpTBXButton
      AlignWithMargins = True
      Left = 191
      Top = 8
      Width = 114
      Height = 27
      Action = Ok
      Align = alRight
      TabOrder = 0
      Images = ResourcesForm.Images16x16
      ImageIndex = 1118
      ExplicitTop = 3
    end
    object ButtonCancel: TSpTBXButton
      AlignWithMargins = True
      Left = 311
      Top = 8
      Width = 114
      Height = 27
      Action = Cancel
      Align = alRight
      TabOrder = 1
      Images = ResourcesForm.Images16x16
      ImageIndex = 143
      ExplicitLeft = 262
    end
  end
  object Actions: TActionList
    Images = ResourcesForm.Images16x16
    Left = 128
    object AutoFill: TAction
      Caption = 'Auto fill'
      ImageIndex = 318
    end
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
