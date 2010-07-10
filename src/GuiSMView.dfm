inherited SMViewForm: TSMViewForm
  Caption = 'SM View'
  ClientHeight = 513
  ClientWidth = 784
  ExplicitWidth = 800
  ExplicitHeight = 551
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 222
    Top = 0
    Width = 562
    Height = 513
    Camera = Camera
    Buffer.BackgroundColor = 4737096
    Buffer.FaceCulling = False
    FieldOfView = 141.330383300781300000
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 479
  end
  object MeshList: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 217
    Height = 513
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
    ExplicitHeight = 479
    Columns = <>
  end
  object Splitter: TSpTBXSplitter
    Left = 217
    Top = 0
    Height = 513
    Cursor = crSizeWE
    MinSize = 200
    ExplicitHeight = 479
  end
  object Navigation: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = Viewer
    ZoomSpeed = 1.049999952316284000
    FormCaption = 'SM View - %FPS'
    Options = [snoMouseWheelHandled]
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
