inherited RFACommonForm: TRFACommonForm
  Caption = 'RFACommonForm'
  ClientHeight = 391
  ClientWidth = 698
  OnDestroy = FormDestroy
  ExplicitWidth = 714
  ExplicitHeight = 429
  PixelsPerInch = 96
  TextHeight = 13
  object Container: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 698
    Height = 391
    Align = alClient
    TabOrder = 0
    Borders = False
    TBXStyleBackground = True
    object RFAList: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 698
      Height = 356
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
      Images = ResourcesForm.Images16x16
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.SelectionOptions = [toMultiSelect, toRightClickSelect]
      OnCompareNodes = RFAListCompareNodes
      OnFreeNode = RFAListFreeNode
      OnGetText = RFAListGetText
      OnGetImageIndex = RFAListGetImageIndex
      OnGetNodeDataSize = RFAListGetNodeDataSize
      OnHeaderClick = RFAListHeaderClick
      OnKeyAction = RFAListKeyAction
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
          Width = 95
          WideText = 'Offset'
        end>
    end
    object SearchBar: TSpTBXPanel
      AlignWithMargins = True
      Left = 0
      Top = 356
      Width = 698
      Height = 35
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Caption = 'SpTBXPanel1'
      Align = alBottom
      Padding.Left = 7
      Padding.Top = 7
      Padding.Right = 7
      Padding.Bottom = 7
      TabOrder = 1
      Visible = False
      Borders = False
      TBXStyleBackground = True
      object SearchEdit: TSpTBXEdit
        Left = 80
        Top = 7
        Width = 263
        Height = 21
        Align = alClient
        TabOrder = 0
        Text = '*.*'
        OnChange = SearchEditChange
      end
      object SpTBXLabel1: TSpTBXLabel
        AlignWithMargins = True
        Left = 31
        Top = 10
        Width = 46
        Height = 15
        Caption = 'Search :'
        Align = alLeft
        Alignment = taRightJustify
      end
      object SpTBXButton1: TSpTBXButton
        Left = 7
        Top = 7
        Width = 21
        Height = 21
        Action = SearchStop
        Align = alLeft
        TabOrder = 2
        Flat = True
        Images = ResourcesForm.Images16x16
        ImageIndex = 98
      end
      object SearchProgressBar: TSpTBXProgressBar
        AlignWithMargins = True
        Left = 349
        Top = 9
        Width = 334
        Height = 17
        Margins.Left = 6
        Margins.Top = 2
        Margins.Right = 8
        Margins.Bottom = 2
        Color = clBtnFace
        Align = alRight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        CaptionGlow = gldNone
        CaptionType = pctNone
        Smooth = True
      end
    end
  end
  object Actions: TActionList
    Images = ResourcesForm.Images16x16
    Left = 8
    Top = 96
    object Preview: TAction
      Category = 'Common'
      Caption = 'Preview'
      ImageIndex = 65
      ShortCut = 16464
      OnExecute = PreviewExecute
    end
    object ExpandAll: TAction
      Category = 'Common'
      Caption = 'Expand all'
      ImageIndex = 206
      OnExecute = ExpandAllExecute
    end
    object CollapseAll: TAction
      Category = 'Common'
      Caption = 'Collapse all'
      ImageIndex = 205
      OnExecute = CollapseAllExecute
    end
    object ExpandSelected: TAction
      Category = 'Common'
      Caption = 'Expand'
      ImageIndex = 206
      OnExecute = ExpandSelectedExecute
    end
    object CollapseSelected: TAction
      Category = 'Common'
      Caption = 'Collapse'
      ImageIndex = 205
      OnExecute = CollapseSelectedExecute
    end
    object SearchStart: TAction
      Category = 'Common'
      Caption = 'Find'
      ImageIndex = 22
      ShortCut = 16454
      OnExecute = SearchStartExecute
    end
    object SearchStop: TAction
      Category = 'Common'
      ImageIndex = 98
      OnExecute = SearchStopExecute
    end
  end
  object UpdateVCL: TTimer
    Interval = 50
    OnTimer = UpdateVCLTimer
    Left = 8
    Top = 64
  end
end
