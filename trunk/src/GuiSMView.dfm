object SMViewForm: TSMViewForm
  Left = 0
  Top = 0
  Caption = 'SM View'
  ClientHeight = 513
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 222
    Top = 0
    Width = 562
    Height = 479
    Camera = Camera
    Buffer.BackgroundColor = 4737096
    Buffer.FaceCulling = False
    FieldOfView = 138.809280395507800000
    Align = alClient
    TabOrder = 0
  end
  object MeshList: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 217
    Height = 479
    Align = alLeft
    Ctl3D = True
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
    Header.MainColumn = -1
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    Images = ResourcesForm.Images16x16
    ParentCtl3D = False
    TabOrder = 1
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toMultiSelect, toRightClickSelect]
    OnChange = MeshListChange
    OnFreeNode = MeshListFreeNode
    OnGetText = MeshListGetText
    OnGetImageIndex = MeshListGetImageIndex
    OnGetNodeDataSize = MeshListGetNodeDataSize
    Columns = <>
  end
  object Splitter: TSpTBXSplitter
    Left = 217
    Top = 0
    Height = 479
    Cursor = crSizeWE
    MinSize = 200
  end
  object Panel1: TSpTBXPanel
    Left = 0
    Top = 479
    Width = 784
    Height = 34
    Caption = ' '
    Color = clBtnFace
    Align = alBottom
    UseDockManager = True
    TabOrder = 3
    Visible = False
    Borders = False
    TBXStyleBackground = True
    ExplicitLeft = 120
    ExplicitTop = 495
    object Label1: TSpTBXLabel
      Left = 8
      Top = 6
      Width = 214
      Height = 19
      Caption = 'Need a real battlefield mesh viewer ?'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = True
      ParentFont = False
    end
    object Label2: TSpTBXLabel
      Left = 228
      Top = 6
      Width = 397
      Height = 19
      Caption = 
        'Download Remdul'#39's BfMeshView on http://www.buijs-leur.nl/bfstuff' +
        '/bfmeshview/ '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentColor = True
      ParentFont = False
      LinkText = 'http://www.buijs-leur.nl/bfstuff/bfmeshview/'
    end
  end
  object Navigation: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = Viewer
    ZoomSpeed = 1.049999952316284000
    FormCaption = 'SM View - %FPS'
    Options = [snoInvertMouseWheel, snoMouseWheelHandled]
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 8
    Top = 40
  end
  object Scene: TGLScene
    Left = 8
    Top = 8
    object Light: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {9695153F9695153F9695153F0000803F}
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {029F1F3FBEBEBE3E999F1F3F0000803F}
    end
    object Camera: TGLCamera
      DepthOfView = 10000.000000000000000000
      FocalLength = 90.000000000000000000
      TargetObject = CameraTarget
      Position.Coordinates = {F9D61343F9D61343F9D613430000803F}
      object CamLight: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {8D8C0C3F8D8C0C3F8D8C0C3F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object CameraTarget: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object FreeMesh: TGLFreeForm
      Material.Texture.Border = 0
    end
    object Grid: TGLXYZGrid
      AntiAliased = True
      LineColor.Color = {FBFAFA3EFBFAFA3EFBFAFA3E0000803F}
      XSamplingScale.Min = -100.000000000000000000
      XSamplingScale.Max = 100.000000000000000000
      XSamplingScale.Step = 5.000000000000000000
      YSamplingScale.Step = 1.000000000000000000
      ZSamplingScale.Min = -100.000000000000000000
      ZSamplingScale.Max = 100.000000000000000000
      ZSamplingScale.Step = 5.000000000000000000
      Parts = [gpX, gpZ]
    end
  end
end
