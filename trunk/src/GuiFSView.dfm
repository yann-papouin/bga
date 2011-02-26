inherited FSViewForm: TFSViewForm
  Caption = 'Filesystem View'
  ClientHeight = 449
  ClientWidth = 511
  Position = poScreenCenter
  OnClose = FormClose
  OnDestroy = FormDestroy
  ExplicitWidth = 527
  ExplicitHeight = 487
  PixelsPerInch = 96
  TextHeight = 13
  object Background: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 511
    Height = 449
    Caption = 'Background'
    Align = alClient
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
    Borders = False
    TBXStyleBackground = True
    object Footer: TSpTBXPanel
      AlignWithMargins = True
      Left = 5
      Top = 411
      Width = 501
      Height = 33
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      UseDockManager = True
      TabOrder = 0
      Borders = False
      object ButtonOk: TSpTBXButton
        AlignWithMargins = True
        Left = 264
        Top = 3
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
        Left = 384
        Top = 3
        Width = 114
        Height = 27
        Action = Cancel
        Align = alRight
        TabOrder = 1
        Images = ResourcesForm.Images16x16
        ImageIndex = 143
      end
    end
  end
  object Actions: TActionList
    Images = ResourcesForm.Images16x16
    Left = 8
    Top = 16
    object Cancel: TAction
      Caption = 'Cancel'
      ImageIndex = 143
      OnExecute = CancelExecute
    end
    object Ok: TAction
      Caption = 'Ok'
      ImageIndex = 1118
      OnExecute = OkExecute
    end
  end
end
