object FSEditForm: TFSEditForm
  Left = 0
  Top = 0
  Caption = 'Edit settings'
  ClientHeight = 461
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    617
    461)
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
    Width = 601
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = BattlefieldDirChange
    EditButton.Left = 578
    EditButton.Top = 0
    EditButton.Width = 19
    EditButton.Height = 17
    EditButton.Caption = '...'
    EditButton.Align = alRight
  end
  object FilesystemName: TSpTBXEdit
    Left = 8
    Top = 36
    Width = 601
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = FilesystemNameChange
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
  object Footer: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 418
    Width = 617
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
    TabOrder = 4
    object ButtonOk: TSpTBXButton
      AlignWithMargins = True
      Left = 375
      Top = 8
      Width = 114
      Height = 27
      Action = Ok
      Align = alRight
      TabOrder = 0
      Images = ResourcesForm.Images16x16
      ImageIndex = 1118
    end
    object ButtonCancel: TSpTBXButton
      AlignWithMargins = True
      Left = 495
      Top = 8
      Width = 114
      Height = 27
      Action = Cancel
      Align = alRight
      TabOrder = 1
      Images = ResourcesForm.Images16x16
      ImageIndex = 143
    end
  end
  object GridPanel1: TGridPanel
    Left = 8
    Top = 118
    Width = 601
    Height = 267
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = SpTBXLabel3
        Row = 0
      end
      item
        Column = 1
        Control = SpTBXLabel4
        Row = 0
      end
      item
        Column = 0
        Control = Mods
        Row = 1
      end
      item
        Column = 1
        Control = ModPath
        Row = 1
      end>
    ParentColor = True
    RowCollection = <
      item
        SizeStyle = ssAbsolute
        Value = 30.000000000000000000
      end
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 5
    object SpTBXLabel3: TSpTBXLabel
      Left = 0
      Top = 0
      Width = 184
      Height = 30
      Caption = 'Mods loaded for this file system :'
      Align = alLeft
      ParentColor = True
      Images = ResourcesForm.Images16x16
      ImageIndex = 1181
    end
    object SpTBXLabel4: TSpTBXLabel
      Left = 300
      Top = 0
      Width = 230
      Height = 30
      Caption = 'Ordered data paths for the selected mod :'
      Align = alLeft
      ParentColor = True
      Images = ResourcesForm.Images16x16
      ImageIndex = 124
    end
    object Mods: TSpTBXListBox
      AlignWithMargins = True
      Left = 3
      Top = 33
      Width = 294
      Height = 224
      Align = alClient
      IntegralHeight = True
      ItemHeight = 20
      TabOrder = 2
      OnClick = ModsClick
    end
    object ModPath: TSpTBXListBox
      AlignWithMargins = True
      Left = 303
      Top = 33
      Width = 295
      Height = 224
      Align = alClient
      IntegralHeight = True
      ItemHeight = 20
      TabOrder = 3
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
