object RFACommonForm: TRFACommonForm
  Left = 0
  Top = 0
  Caption = 'RFACommonForm'
  ClientHeight = 533
  ClientWidth = 933
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object RFAList: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 933
    Height = 533
    Align = alClient
    DragMode = dmAutomatic
    DragOperations = [doMove]
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 24
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.SelectionOptions = [toMultiSelect, toRightClickSelect]
    ExplicitLeft = -182
    ExplicitTop = -159
    ExplicitWidth = 817
    ExplicitHeight = 459
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
        Width = 326
        WideText = 'Offset'
      end>
  end
end
