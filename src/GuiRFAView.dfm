inherited RFAViewForm: TRFAViewForm
  Caption = 'RFA View'
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 949
  ExplicitHeight = 571
  PixelsPerInch = 96
  TextHeight = 13
  inherited Container: TSpTBXPanel
    Top = 25
    Height = 451
    ExplicitTop = 25
    ExplicitHeight = 451
    inherited RFAList: TVirtualStringTree
      Height = 416
      PopupMenu = ViewerPopup
      OnBeforeCellPaint = RFAListBeforeCellPaint
      OnDblClick = RFAListDblClick
      OnDragOver = RFAListDragOver
      OnDragDrop = RFAListDragDrop
      OnEdited = RFAListEdited
      OnNewText = RFAListNewText
      OnNodeMoved = RFAListNodeMoved
      OnStartDrag = RFAListStartDrag
      OnStateChange = RFAListStateChange
      ExplicitHeight = 416
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
    inherited SearchBar: TSpTBXPanel
      Top = 416
      ExplicitTop = 416
    end
    object Theme: TSpTBXEdit
      Left = 8
      Top = 264
      Width = 69
      Height = 21
      TabOrder = 2
      Visible = False
    end
  end
  object TopDock: TSpTBXDock [1]
    Left = 0
    Top = 0
    Width = 933
    Height = 25
    object tbMenuBar: TSpTBXToolbar
      Left = 0
      Top = 0
      ActivateParent = False
      CloseButton = False
      DefaultDock = TopDock
      DockMode = dmCannotFloatOrChangeDocks
      DockPos = 0
      DragHandleStyle = dhNone
      FullSize = True
      Images = ResourcesForm.Images16x16
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      TabOrder = 0
      Caption = 'Menu'
      Customizable = False
      MenuBar = True
      object mFile: TSpTBXSubmenuItem
        Caption = 'File'
        object SpTBXItem14: TSpTBXItem
          Action = New
        end
        object SpTBXItem2: TSpTBXItem
          Action = Open
        end
        object RecentMenu: TSpTBXSubmenuItem
          Action = Recent
          object SpTBXItem4: TSpTBXItem
            OnClick = OpenRecentClick
          end
        end
        object SpTBXSeparatorItem1: TSpTBXSeparatorItem
        end
        object SpTBXItem5: TSpTBXItem
          Action = Save
        end
        object SpTBXItem15: TSpTBXItem
          Action = Defrag
        end
        object SpTBXItem6: TSpTBXItem
          Action = SaveAs
        end
        object SpTBXSeparatorItem2: TSpTBXSeparatorItem
        end
        object SpTBXItem7: TSpTBXItem
          Action = Quit
        end
      end
      object mEdit: TSpTBXSubmenuItem
        Caption = 'Edit'
        object SpTBXItem12: TSpTBXItem
          Action = SearchStart
        end
        object SpTBXSeparatorItem4: TSpTBXSeparatorItem
        end
        object SpTBXItem16: TSpTBXItem
          Action = ExpandAll
        end
        object SpTBXItem18: TSpTBXItem
          Action = CollapseAll
        end
        object SpTBXSeparatorItem9: TSpTBXSeparatorItem
        end
        object SpTBXItem1: TSpTBXItem
          Action = Settings
        end
        object SpTBXItem23: TSpTBXItem
          Action = Filesystem
        end
      end
      object SpTBXSubmenuItem1: TSpTBXSubmenuItem
        Caption = 'Preview'
        object SpTBXItem13: TSpTBXItem
          Action = PreviewRAW
        end
      end
      object SpTBXSubmenuItem3: TSpTBXSubmenuItem
        Caption = 'Extract'
        object SpTBXItem11: TSpTBXItem
          Action = ExtractAll
        end
        object SpTBXItem10: TSpTBXItem
          Action = ExtractSelected
        end
        object SpTBXSeparatorItem3: TSpTBXSeparatorItem
        end
        object SpTBXItem9: TSpTBXItem
          Action = ExtractModFolder
        end
      end
      object SpTBXSubmenuItem2: TSpTBXSubmenuItem
        Caption = 'Pack'
        object SpTBXItem8: TSpTBXItem
          Action = PackDirectory
        end
        object UseCompression: TSpTBXItem
          Caption = 'Use compression'
          AutoCheck = True
          Checked = True
        end
      end
      object SpTBXSubmenuItem5: TSpTBXSubmenuItem
        Caption = 'Theme'
        object SkinGroup: TSpTBXSkinGroupItem
          OnSkinChange = SkinGroupSkinChange
        end
      end
      object mHelp: TSpTBXSubmenuItem
        Caption = 'Help'
        object SpTBXItem3: TSpTBXItem
          Action = About
        end
      end
      object SpTBXItem17: TSpTBXItem
        Action = NewVersionAvailable
      end
    end
  end
  object StatusBar: TSpTBXStatusBar [2]
    Left = 0
    Top = 508
    Width = 933
    Height = 25
    object ArchiveSize: TSpTBXLabelItem
      Caption = 'ArchiveSize'
    end
    object SpTBXSeparatorItem7: TSpTBXSeparatorItem
    end
    object Fragmentation: TSpTBXLabelItem
      Caption = 'Fragmentation'
    end
    object SpTBXSeparatorItem8: TSpTBXSeparatorItem
    end
    object ArchiveFileCount: TSpTBXLabelItem
      Caption = 'ArchiveFileCount'
    end
    object SpTBXSeparatorItem10: TSpTBXSeparatorItem
    end
    object SelectionText: TSpTBXLabelItem
      Caption = 'SelectionText'
    end
  end
  object ProgressPanel: TSpTBXPanel [3]
    Left = 0
    Top = 476
    Width = 933
    Height = 32
    Caption = 'LoadBar'
    Align = alBottom
    TabOrder = 1
    Visible = False
    Borders = False
    TBXStyleBackground = True
    object SubProgressBar: TSpTBXProgressBar
      AlignWithMargins = True
      Left = 671
      Top = 7
      Width = 168
      Height = 18
      Margins.Top = 7
      Margins.Right = 8
      Margins.Bottom = 7
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
    object TotalProgressBar: TSpTBXProgressBar
      AlignWithMargins = True
      Left = 67
      Top = 7
      Width = 593
      Height = 18
      Margins.Top = 7
      Margins.Right = 8
      Margins.Bottom = 7
      Color = clBtnFace
      Align = alClient
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
    object TotalProgressLabel: TSpTBXLabel
      AlignWithMargins = True
      Left = 8
      Top = 3
      Width = 53
      Height = 26
      Margins.Left = 8
      Caption = 'Progress'
      Align = alLeft
    end
    object SpTBXButton2: TSpTBXButton
      AlignWithMargins = True
      Left = 850
      Top = 4
      Width = 75
      Height = 24
      Margins.Top = 4
      Margins.Right = 8
      Margins.Bottom = 4
      Action = Cancel
      Align = alRight
      TabOrder = 3
      Images = ResourcesForm.Images16x16
      ImageIndex = 85
    end
  end
  inherited Actions: TActionList
    object Open: TAction [5]
      Category = 'Custom'
      Caption = 'Open ...'
      ImageIndex = 8
      ShortCut = 16463
      OnExecute = OpenExecute
    end
    object Save: TAction [6]
      Category = 'Custom'
      Caption = 'Save'
      ImageIndex = 1179
      ShortCut = 16467
      OnExecute = SaveExecute
    end
    object SaveAs: TAction [7]
      Category = 'Custom'
      Caption = 'Save as ...'
      ImageIndex = 16
      OnExecute = SaveAsExecute
    end
    object Quit: TAction [8]
      Category = 'Custom'
      Caption = 'Exit'
      ImageIndex = 1
      ShortCut = 16465
      OnExecute = QuitExecute
    end
    object Recent: TAction [9]
      Category = 'Custom'
      Caption = 'Recent'
      ImageIndex = 9
      OnExecute = RecentExecute
    end
    object PackDirectory: TAction [10]
      Category = 'Custom'
      Caption = 'Directory...'
      ImageIndex = 109
      OnExecute = PackDirectoryExecute
    end
    object ExtractModFolder: TAction [11]
      Category = 'Custom'
      Caption = 'Mod folder'
      ImageIndex = 124
    end
    object About: TAction [12]
      Category = 'Custom'
      Caption = 'About'
      ImageIndex = 95
      ShortCut = 112
      OnExecute = AboutExecute
    end
    object ApplicationRun: TAction [13]
      Category = 'Custom'
      OnExecute = ApplicationRunExecute
    end
    object Settings: TAction [14]
      Category = 'Custom'
      Caption = 'Settings ...'
      ImageIndex = 13
      ShortCut = 123
    end
    object PreviewRAW: TAction [15]
      Category = 'Custom'
      Caption = 'RAW map'
      ImageIndex = 172
      ShortCut = 16461
      OnExecute = PreviewRAWExecute
    end
    object Defrag: TAction [16]
      Category = 'Custom'
      Caption = 'Save (Defrag)'
      ImageIndex = 15
      ShortCut = 49235
      OnExecute = DefragExecute
    end
    object New: TAction [17]
      Category = 'Custom'
      Caption = 'New'
      ImageIndex = 7
      ShortCut = 16462
      OnExecute = NewExecute
    end
    object NewVersionAvailable: TAction [18]
      Category = 'Custom'
      Caption = 'New version available !'
      Enabled = False
      Visible = False
      OnExecute = NewVersionAvailableExecute
    end
    object Cancel: TAction [19]
      Category = 'Custom'
      Caption = 'Cancel'
      Enabled = False
      ImageIndex = 85
      OnExecute = CancelExecute
    end
    object NewFolder: TAction [20]
      Category = 'Custom'
      Caption = 'Create a new folder'
      ImageIndex = 28
      OnExecute = NewFolderExecute
    end
    object ExtractAll: TAction
      Category = 'Common'
      Caption = 'All'
      ImageIndex = 1041
      OnExecute = ExtractAllExecute
    end
    object ExtractSelected: TAction
      Category = 'Common'
      Caption = 'Selected'
      ImageIndex = 796
      OnExecute = ExtractSelectedExecute
    end
    object Filesystem: TAction
      Category = 'Custom'
      Caption = 'File system ...'
      ImageIndex = 522
      Visible = False
      OnExecute = FilesystemExecute
    end
  end
  object DropFileSource: TDropFileSource
    DragTypes = [dtMove]
    OnDrop = DropFileSourceDrop
    OnAfterDrop = DropFileSourceAfterDrop
    OnGetData = DropFileSourceGetData
    Left = 8
    Top = 256
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.rfa'
    Filter = 'Battlefield 1942 game archive|*.rfa'
    Title = 'Open'
    Left = 8
    Top = 224
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.rfa'
    Filter = 'Battlefield 1942 game archive|*.rfa'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save'
    Left = 40
    Top = 224
  end
  object AppInstances: TJvAppInstances
    Active = False
    AutoActivate = False
    OnCmdLineReceived = AppInstancesCmdLineReceived
    Left = 40
    Top = 192
  end
  object RecentList: TJvMruList
    SubKeyUnicode = 'Software\Battlefield 1942\BGA\Recent'
    MaxItems = 20
    OnEnumText = RecentListEnumText
    Active = False
    Left = 8
    Top = 192
  end
  object ViewerPopup: TSpTBXPopupMenu
    Images = ResourcesForm.Images16x16
    Left = 40
    Top = 160
    object SpTBXItem22: TSpTBXItem
      Action = NewFolder
    end
    object SpTBXSubmenuItem4: TSpTBXSubmenuItem
      Caption = 'Edit with ...'
      Enabled = False
      ImageIndex = 791
    end
    object SpTBXSeparatorItem5: TSpTBXSeparatorItem
    end
    object SpTBXItem21: TSpTBXItem
      Action = ExpandSelected
    end
    object SpTBXItem20: TSpTBXItem
      Action = CollapseSelected
    end
    object SpTBXSeparatorItem6: TSpTBXSeparatorItem
    end
    object SpTBXItem19: TSpTBXItem
      Action = Preview
    end
  end
  object AppStorage: TJvAppRegistryStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    Root = 'Software\Battlefield 1942\BGA'
    SubStorages = <>
    Left = 40
    Top = 128
  end
  object FormStorage: TJvFormStorage
    AppStorage = AppStorage
    AppStoragePath = '%FORM_NAME%\'
    StoredProps.Strings = (
      'NewVersionAvailable.Visible'
      'Theme.Text'
      'UseCompression.Checked')
    StoredValues = <>
    Left = 8
    Top = 128
  end
  object Sync: TTimer
    Enabled = False
    Interval = 10
    OnTimer = SyncTimer
    Left = 8
    Top = 160
  end
end
