inherited RAWViewForm: TRAWViewForm
  ActiveControl = Viewer
  Caption = 'RAW View'
  ClientHeight = 572
  ClientWidth = 1049
  OnDestroy = FormDestroy
  ExplicitWidth = 1065
  ExplicitHeight = 610
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 739
    Height = 547
    Camera = Camera
    PostRender = ViewerPostRender
    Buffer.BackgroundColor = clSkyBlue
    Buffer.FaceCulling = False
    FieldOfView = 143.570602416992200000
    Align = alClient
    OnDblClick = ViewerDblClick
    OnMouseDown = ViewerMouseDown
    TabOrder = 0
  end
  object Inspector: TJvInspector
    Left = 744
    Top = 0
    Width = 305
    Height = 547
    Align = alRight
    Divider = 120
    ItemHeight = 16
    TabStop = True
    TabOrder = 1
  end
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 547
    Width = 1049
    Height = 25
    object XLabel: TSpTBXLabelItem
      Caption = 'X'
    end
    object TBSeparatorItem2: TTBSeparatorItem
    end
    object YLabel: TSpTBXLabelItem
      Caption = 'Y'
    end
    object TBSeparatorItem1: TTBSeparatorItem
    end
    object ZLabel: TSpTBXLabelItem
      Caption = 'Z'
    end
  end
  object SpTBXSplitter1: TSpTBXSplitter
    Left = 739
    Top = 0
    Height = 547
    Cursor = crSizeWE
    Align = alRight
  end
  object Scene: TGLScene
    Left = 8
    Top = 8
    object Root: TGLDummyCube
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {0000008000000000000080BF00000000}
      CubeSize = 1.000000000000000000
      object CameraTarget: TGLDummyCube
        CubeSize = 1.000000000000000000
      end
      object WaterPlane: TGLPlane
        Material.FrontProperties.Diffuse.Color = {00000000FDFC7C3ECDCC4C3F4A0C023E}
        Material.FrontProperties.Emission.Color = {EBEAEA3ED7D6D63EDFDEDE3E0000803F}
        Material.BlendingMode = bmModulate
        Up.Coordinates = {000000000000803F0000008000000000}
        Height = 16.000000000000000000
        Width = 16.000000000000000000
      end
      object TerrainRenderer: TGLTerrainRenderer
        HeightDataSource = BattlefieldHDS
        TilesPerTexture = 1.000000000000000000
      end
    end
    object Camera: TGLCamera
      DepthOfView = 1200.000000000000000000
      FocalLength = 90.000000000000000000
      TargetObject = CameraTarget
      Position.Coordinates = {000000000000A0410000A0400000803F}
      object CamLight: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object Navigation: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = Viewer
    ZoomSpeed = 1.049999952316284000
    FormCaption = 'RAW View - %FPS'
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
    OnMouseMove = ViewerMouseMove
    Left = 8
    Top = 40
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Left = 8
    Top = 73
  end
  object BattlefieldHDS: TGLCustomHDS
    MaxPoolSize = 0
    OnStartPreparingData = BattlefieldHDSStartPreparingData
    Left = 8
    Top = 104
  end
end
