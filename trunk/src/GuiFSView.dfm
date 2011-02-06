inherited FSViewForm: TFSViewForm
  Caption = 'File system View'
  ClientHeight = 455
  ClientWidth = 511
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitWidth = 527
  ExplicitHeight = 493
  PixelsPerInch = 96
  TextHeight = 13
  object Background: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 511
    Height = 455
    Caption = 'Background'
    Align = alClient
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
    Borders = False
    TBXStyleBackground = True
    object SpTBXGroupBox1: TSpTBXGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 501
      Height = 308
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 10
      Caption = 'Select a file system'
      Align = alTop
      Padding.Left = 10
      Padding.Top = 10
      Padding.Right = 10
      Padding.Bottom = 10
      TabOrder = 0
      object SpTBXDock1: TSpTBXDock
        Left = 12
        Top = 270
        Width = 477
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
          object SpTBXItem2: TSpTBXItem
            Action = Active
            DisplayMode = nbdmImageAndText
          end
          object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
            CustomWidth = 176
          end
          object SpTBXItem3: TSpTBXItem
            Action = Add
            DisplayMode = nbdmImageAndText
          end
          object SpTBXItem4: TSpTBXItem
            Action = Remove
            DisplayMode = nbdmImageAndText
          end
          object SpTBXSeparatorItem2: TSpTBXSeparatorItem
          end
          object SpTBXItem5: TSpTBXItem
            Action = Edit
            DisplayMode = nbdmImageAndText
          end
          object SpTBXItem1: TSpTBXItem
            Action = Update
            DisplayMode = nbdmImageAndText
          end
        end
      end
      object FilesystemList: TVirtualStringTree
        Left = 12
        Top = 25
        Width = 477
        Height = 245
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
        TabOrder = 1
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toFullVertGridLines, toUseBlendedSelection, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
        OnBeforeCellPaint = FilesystemListBeforeCellPaint
        OnDblClick = FilesystemListDblClick
        OnDrawText = FilesystemListDrawText
        OnFreeNode = FilesystemListFreeNode
        OnGetText = FilesystemListGetText
        OnGetNodeDataSize = FilesystemListGetNodeDataSize
        OnStateChange = FilesystemListStateChange
        Columns = <
          item
            MinWidth = 200
            Position = 0
            Width = 200
            WideText = 'Name'
          end
          item
            Position = 1
            Width = 273
            WideText = 'Path'
          end>
      end
    end
    object Footer: TSpTBXPanel
      AlignWithMargins = True
      Left = 5
      Top = 417
      Width = 501
      Height = 33
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      UseDockManager = True
      TabOrder = 1
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
      object SyncStatus: TSpTBXLabel
        Left = 0
        Top = 8
        Width = 34
        Height = 22
        Caption = '--'
        Images = ResourcesForm.Images16x16
        ImageIndex = 312
      end
    end
    object SyncStatusGroup: TSpTBXGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 323
      Width = 501
      Height = 78
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 10
      Caption = 'Syncing status'
      Align = alTop
      Padding.Left = 10
      Padding.Top = 10
      Padding.Right = 10
      Padding.Bottom = 10
      TabOrder = 2
      Visible = False
      DesignSize = (
        501
        78)
      object SyncAnimScan: TJvGIFAnimator
        Left = 13
        Top = 24
        Width = 16
        Height = 16
        Center = True
        Image.Data = {
          A705000047494638396110001000E60000FFFFFFB50000FCFAFADE9292DC8888
          F9EEEEEEC8C8F3D8D8E29E9EB50000DD8E8EEDC6C6C33232CC5252F8ECECE4A6
          A6DF9494FAF2F2DA8282BD1C1CE8B2B2E4A4A4E19A9AFBF4F4FCF8F8E5A8A8F0
          CECEC84444B80C0CD06060F4DEDEF7E8E8EBBCBCC12A2AC43636C94646F1D4D4
          EDC4C4C63C3CCB4E4EC63E3EC94848F0D0D0EFCACACE5A5AE8B4B4F8EAEAD570
          70E6ACACE9B8B8EABABAD05E5ECF5C5CD16262DB8686F5E0E0D97E7EFDFCFCE3
          A2A2BB1818C12C2CCD5656B60404CB5050EFCCCCC53838CE5858BC1A1AF9F0F0
          F2D6D6E19C9CF5E2E2F7E6E6C74040E5AAAABF2424BE2020E7AEAED57272CA4A
          4AC33434D36A6AF1D2D2ECC0C0BF2626D87C7CC74242CD5454B70A0AB70808E7
          B0B0F6E4E4CA4C4CFBF6F6E9B6B6D26666D26868F4DCDCE2A0A0BA1212BE2222
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000021FF0B4E45545343415045322E3003
          0100000021FE1A43726561746564207769746820616A61786C6F61642E696E66
          6F0021F90409080000002C000000001000100000076880008283202226258389
          000B121A00210909288A82060C091B1E97092982118904911C160B23272A0018
          199F82070D131D1F94188A0E1405948A02080FB994039115BE002488A10916C3
          C500050A1017C3D1D2A9BEB4891119D689AAAC83DD2424DED12588D3D1810021
          F90409080000002C000000001000100000076B800082830734353783898A3309
          092F393A3D1939832B2F1A172F8D38103B093B30832C090D322E123605218D09
          35832D29310589153C3E3C32832E30B38A303F208AC2C3002524C482242582C6
          C800CACED1D28318C4D5891119D7891819118ADFC9C782E1C425CBD3C8810021
          F90409080000002C00000000100010000007688000828302080F838889000309
          09150025122A891182048D1640410926458311191800050A1017383E8D4688A2
          891E424334478A8A0E1444B4B4202226259024B900218D28BFC10C8D29C1830B
          232793CBB9AD8AD382A0D58218199588DC0024C0D6CB25BED1D1810021F90409
          080000002C000000001000100000076780008283848586822525878311832424
          00440B4E5284111918850F490951859985484A4F538B851F3005A68507343537
          0025908B3309092FB1B3872FB638AB822E1236AAAB46500F9F86104B094C308C
          988521B60935968E90154C0954328B89824D51DEBF86810021F9040908000000
          2C0000000010001000000768800082838485868225258783118324248C851119
          188618198D8495879B8B9E9F8202080F88908B030909150025A68704A916A000
          050A1017A00B4E45859D832B565856BC82939520222625553EA946849921A928
          475759425B870CA92900475A1F8B0B23272A86810021F90409080000002C0000
          000010001000000767800082838485868225258783118324248C851119188618
          198D8495879B8B9E9F8725909E248A00A29FA5A08B16500F865D205D83461309
          21198444532C1A835C09C129073435370F490960831943093C3233C12F1F5E5F
          0B844D5132002FC138AB2E12360586810021F90409080000002C000000001000
          1000000767800082838485868225258783118324248C851119188618198D8495
          879B8B9E2022268A9E82210909289E248A0CA729AA8A0B23272AA4842B4E52B6
          0006411C4F61968436A71C1602080F82979900615C59330E03A71582CD825B5A
          480004A716BB050A101786810021F90409080000002C00000000100010000007
          688000828384858202623D4A82252586824663094C190024248311845C099D4F
          8518199A8219643E2232861885303F318F8407343537B084339D2FB6248E2F9D
          38BB8E2E123605B685113202C78325422B5DAA852C090DAF84A1A382535F5344
          969882DA8F8DCCE681003B}
        Threaded = False
      end
      object SyncAnimStore: TJvGIFAnimator
        Left = 13
        Top = 24
        Width = 16
        Height = 16
        Center = True
        Image.Data = {
          4603000047494638396110001000C40000FFFFFF383838F9F9F96363639E9E9E
          3C3C3C555555DEDEDEBABABA494949939393878787E9E9E9AEAEAED2D2D27070
          707B7B7B00000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000021FF0B4E45545343415045322E3003
          0100000021FE1A43726561746564207769746820616A61786C6F61642E696E66
          6F0021F904090A0000002C000000001000100000055020208E64699EA530146C
          212A8460AAAD9814C6911A3580F006D988805B8D043C02698550911085018985
          A28EAC292C80495AF8A22462F0BA4892903884A835261D7889356B203C1314F2
          791DC5EF8F420021F904090A0000002C000000001000100000056820208A47D2
          8C682A1AC5E0A8A84024457DC0C0C1D6C502088C9480656814120CC1C0204011
          0A4CC080F0635147830242B4E5B6503598203CAA35556451B62BE822BEA36794
          30A816AEA2A18F416B10CF2341000B3C503730073C090480300E5906382A0D09
          8723210021F904090A0000002C000000001000100000056020208E23F39028DA
          14482A0AC45014092DA48731EF33810A3A43CB314BB4488482E106482E9824D9
          B179000810BED1CC05D86A0B2E81574440DC0E5900A230480916852C308E42D4
          0A8E6B104AE6ED0C553F357703047C246B0D5C290F0C8B210021F904090A0000
          002C000000001000100000055220208E64699E802094023114C540A883431E06
          AC17C66B8B829C012142E4603F0081B702BE90234582387A4251588063BB6DA2
          760B1222A120E9C2A3608160860D5546A61972DC190EA570EB29F366FF802100
          21F904090A0000002C000000001000100000056020208E64699EA8400C453110
          C2C9186D5D18C7B918488CD086D82807108A04340220E7282C18A64621614800
          58539808816D35A4B501624448B40600AAB961622C0A8E258070331663C423B0
          27F80DEC220734363750262A582F80288C8D24210021F904090A0000002C0000
          00001000100000055F20208E64699EA84814C54008A6403400C2B2C641224991
          08B09AA160083E6E850549302488188D1E4B37B20D482C07C1892D74832A8283
          451A1410A4C32DD160A88860C002F91031890818B097408F0E433768082F3104
          66055C298B8C2621003B}
        Threaded = False
      end
      object SyncStatusAction: TSpTBXLabel
        Left = 33
        Top = 22
        Width = 18
        Height = 19
        Caption = '---'
      end
      object SyncProgressBar: TSpTBXProgressBar
        AlignWithMargins = True
        Left = 208
        Top = 24
        Width = 275
        Height = 17
        Margins.Left = 6
        Margins.Top = 2
        Margins.Right = 8
        Margins.Bottom = 2
        Color = clBtnFace
        Anchors = [akLeft, akTop, akRight]
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
      object SyncStatusArchiveName: TSpTBXLabel
        Left = 13
        Top = 46
        Width = 475
        Height = 22
        Caption = '---'
        Anchors = [akLeft, akTop, akRight]
        Wrapping = twPathEllipsis
        Images = ResourcesForm.Images16x16
        ImageIndex = 832
      end
    end
  end
  object Actions: TActionList
    Images = ResourcesForm.Images16x16
    Left = 88
    Top = 64
    object Add: TAction
      Caption = 'Add'
      ImageIndex = 179
      OnExecute = AddExecute
    end
    object Import: TAction
      Caption = 'Import'
      ImageIndex = 61
      OnExecute = ImportExecute
    end
    object Update: TAction
      Caption = 'Update'
      ImageIndex = 94
      OnExecute = UpdateExecute
    end
    object Remove: TAction
      Caption = 'Remove'
      ImageIndex = 471
      OnExecute = RemoveExecute
    end
    object Edit: TAction
      Caption = 'Edit'
      ImageIndex = 51
      OnExecute = EditExecute
    end
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
    object Init: TAction
      Caption = 'Init'
      OnExecute = InitExecute
    end
    object Active: TAction
      Caption = 'Active'
      ImageIndex = 495
      OnExecute = ActiveExecute
    end
    object Delete: TAction
      Caption = 'Delete'
      ImageIndex = 268
    end
  end
  object FormStorage: TJvFormStorage
    AppStorage = TemporaryAppStorage
    AppStoragePath = 'Filesystems\'
    Options = []
    BeforeSavePlacement = FormStorageBeforeSavePlacement
    AfterRestorePlacement = FormStorageAfterRestorePlacement
    StoredValues = <>
    Left = 24
    Top = 64
  end
  object TemporaryAppStorage: TJvAppRegistryStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    Root = 'Software\Battlefield 1942\BGA'
    SubStorages = <>
    Left = 56
    Top = 64
  end
  object DataSource: TDataSource
    DataSet = Dataset
    Left = 88
    Top = 96
  end
  object Database: TSqlitePassDatabase
    DatabaseType = dbtSqlitePass
    DatatypeOptions.BooleanStorage = asInteger
    DatatypeOptions.DateFormat = 'YYYY-MM-DD'
    DatatypeOptions.DateStorage = asInteger
    DatatypeOptions.DateTimeFormat = 'YYYY-MM-DD hh:mm:ss.zzz'
    DatatypeOptions.DateTimeStorage = dtsDateTime
    DatatypeOptions.DecimalSeparator = '.'
    DatatypeOptions.DefaultFieldType = ftUnknown
    DatatypeOptions.DetectionMode = dmTypeName
    DatatypeOptions.LoadOptions = [loDefaultProperties, loCustomProperties, loTranslationRules, loCustomFieldDefs]
    DatatypeOptions.SaveOptions = [soCustomProperties, soTranslationRules, soCustomFieldDefs]
    DatatypeOptions.UnicodeEncoding = ueUTF16
    DatatypeOptions.TimeFormat = 'hh:mm:ss'
    DatatypeOptions.TimeStorage = asInteger
    DatatypeOptions.pCustomFieldDefs = ()
    DatatypeOptions.pTranslationsRules = ()
    Options.ApplyMode = [amOverwriteDatabaseFileSettings, amAutoVacuum, amCacheSize, amCaseSensitiveLike, amCountChanges, amDefaultCacheSize, amFullColumnNames, amForeignKeys, amJournalMode, amLockingMode, amRecursiveTriggers, amSecureDelete, amSynchronous, amTemporaryStorage]
    Options.AutoVacuum = avNone
    Options.CacheSize = 2000
    Options.CaseSensitiveLike = False
    Options.CountChanges = False
    Options.DefaultCacheSize = 2000
    Options.Encoding = UTF8
    Options.ForeignKeys = False
    Options.FullColumnNames = False
    Options.JournalMode = jmDelete
    Options.JournalSizeLimit = -1
    Options.LockingMode = lmNormal
    Options.LogErrors = True
    Options.MaxPageCount = 2147483647
    Options.PageSize = 1024
    Options.QuoteStyle = qsDoubleQuote
    Options.RecursiveTriggers = False
    Options.SecureDelete = False
    Options.Synchronous = syncNormal
    Options.TemporaryStorage = tsDefault
    QueryTimeout = 0
    ShowSystemObjects = False
    VersionInfo.Component = '0.55'
    VersionInfo.Schema = -1
    VersionInfo.Package = '0.55'
    VersionInfo.SqliteLibraryNumber = 0
    VersionInfo.UserTag = -1
    Left = 24
    Top = 96
  end
  object Dataset: TSqlitePassDataset
    CalcDisplayedRecordsOnly = False
    Database = Database
    MasterSourceAutoActivate = True
    FilterMode = fmSQLDirect
    FilterRecordLowerLimit = 0
    FilterRecordUpperLimit = 0
    Indexed = True
    LocateSmartRefresh = False
    LookUpCache = False
    LookUpDisplayedRecordsOnly = False
    LookUpSmartRefresh = False
    Sorted = False
    RecordsCacheCapacity = 100
    DatabaseAutoActivate = True
    VersionInfo.Component = '0.55'
    VersionInfo.Package = '0.55'
    ParamCheck = False
    WriteMode = wmDirect
    Left = 56
    Top = 96
    pParams = ()
  end
  object SubDataset: TSqlitePassDataset
    CalcDisplayedRecordsOnly = False
    Database = Database
    MasterSourceAutoActivate = True
    FilterMode = fmSQLDirect
    FilterRecordLowerLimit = 0
    FilterRecordUpperLimit = 0
    Indexed = True
    LocateSmartRefresh = False
    LookUpCache = False
    LookUpDisplayedRecordsOnly = False
    LookUpSmartRefresh = False
    Sorted = False
    RecordsCacheCapacity = 100
    DatabaseAutoActivate = True
    VersionInfo.Component = '0.55'
    VersionInfo.Package = '0.55'
    ParamCheck = False
    WriteMode = wmDirect
    Left = 56
    Top = 128
    pParams = ()
  end
  object Sync: TTimer
    Enabled = False
    Interval = 100
    OnTimer = SyncTimer
    Left = 24
    Top = 128
  end
  object SyncDataset: TSqlitePassDataset
    CalcDisplayedRecordsOnly = False
    Database = Database
    MasterSourceAutoActivate = True
    FilterMode = fmSQLDirect
    FilterRecordLowerLimit = 0
    FilterRecordUpperLimit = 0
    Indexed = True
    LocateSmartRefresh = False
    LookUpCache = False
    LookUpDisplayedRecordsOnly = False
    LookUpSmartRefresh = False
    Sorted = False
    RecordsCacheCapacity = 100
    DatabaseAutoActivate = True
    VersionInfo.Component = '0.55'
    VersionInfo.Package = '0.55'
    ParamCheck = False
    WriteMode = wmDirect
    Left = 56
    Top = 160
    pParams = ()
  end
end
