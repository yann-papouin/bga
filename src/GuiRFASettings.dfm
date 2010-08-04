object RFASettingsForm: TRFASettingsForm
  AlignWithMargins = True
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 370
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Background: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 421
    Height = 370
    Caption = 'Background'
    Align = alClient
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
    Borders = False
    TBXStyleBackground = True
    object Footer: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 332
      Width = 411
      Height = 33
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object ButtonOk: TSpTBXButton
        AlignWithMargins = True
        Left = 174
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
        Left = 294
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
    object DoubleClickOption: TSpTBXRadioGroup
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 411
      Height = 94
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 10
      Caption = 'On double click'
      Align = alTop
      TabOrder = 1
      ItemIndex = 0
      Items.Strings = (
        'Preview the file with the internal viewer'
        'Open (Edit) the file with OS associated extension'
        'Open (Edit) the file with BGA associated extension (see below)')
    end
    object SpTBXGroupBox1: TSpTBXGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 109
      Width = 411
      Height = 215
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 10
      Caption = 'Extension editor association'
      Align = alTop
      Padding.Left = 10
      Padding.Top = 10
      Padding.Right = 10
      Padding.Bottom = 10
      TabOrder = 2
      object ExtList: TVirtualStringTree
        Left = 12
        Top = 25
        Width = 387
        Height = 152
        Align = alClient
        DragMode = dmAutomatic
        DragOperations = [doMove]
        EditDelay = 300
        Header.AutoSizeIndex = -1
        Header.DefaultHeight = 17
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Height = 24
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
        Images = ResourcesForm.Images16x16
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toFullVertGridLines, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
        OnDblClick = ExtListDblClick
        OnDrawText = ExtListDrawText
        OnEdited = ExtListEdited
        OnEditing = ExtListEditing
        OnFreeNode = ExtListFreeNode
        OnGetText = ExtListGetText
        OnGetNodeDataSize = ExtListGetNodeDataSize
        OnNewText = ExtListNewText
        ExplicitHeight = 155
        Columns = <
          item
            MinWidth = 80
            Position = 0
            Width = 80
            WideText = 'Extension'
          end
          item
            Position = 1
            Width = 303
            WideText = 'Path'
          end>
      end
      object SpTBXDock1: TSpTBXDock
        Left = 12
        Top = 177
        Width = 387
        Height = 26
        AllowDrag = False
        Position = dpBottom
        object SpTBXToolbar1: TSpTBXToolbar
          Left = 0
          Top = 0
          Align = alBottom
          DockPos = 40
          FullSize = True
          Images = ResourcesForm.Images16x16
          Resizable = False
          ShrinkMode = tbsmNone
          Stretch = True
          TabOrder = 0
          Caption = 'SpTBXToolbar1'
          object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
            CustomWidth = 158
          end
          object SpTBXItem1: TSpTBXItem
            Action = Add
            DisplayMode = nbdmImageAndText
          end
          object SpTBXItem2: TSpTBXItem
            Action = Remove
            DisplayMode = nbdmImageAndText
          end
          object SpTBXSeparatorItem1: TSpTBXSeparatorItem
          end
          object SpTBXItem4: TSpTBXItem
            Action = Edit
            DisplayMode = nbdmImageAndText
          end
          object SpTBXItem3: TSpTBXItem
            Action = Browse
            DisplayMode = nbdmImageAndText
          end
        end
      end
    end
  end
  object FormStorage: TJvFormStorage
    AppStorage = RFAViewForm.AppStorage
    AppStoragePath = 'Settings\'
    Options = []
    BeforeSavePlacement = FormStorageBeforeSavePlacement
    AfterRestorePlacement = FormStorageAfterRestorePlacement
    StoredProps.Strings = (
      'DoubleClickOption.ItemIndex')
    StoredValues = <>
    Left = 304
    Top = 8
  end
  object Actions: TActionList
    Images = ResourcesForm.Images16x16
    Left = 336
    Top = 8
    object Add: TAction
      Caption = 'Add'
      ImageIndex = 179
      OnExecute = AddExecute
    end
    object Remove: TAction
      Caption = 'Remove'
      ImageIndex = 471
      OnExecute = RemoveExecute
    end
    object ChoosePath: TAction
      Caption = 'ChoosePath'
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
    object Browse: TAction
      Caption = 'Browse'
      ImageIndex = 124
      OnExecute = BrowseExecute
    end
    object Edit: TAction
      Caption = 'Edit'
      ImageIndex = 51
      OnExecute = EditExecute
    end
  end
  object OpenDialog: TOpenDialog
    Left = 368
    Top = 8
  end
end
