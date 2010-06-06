inherited RAWViewForm: TRAWViewForm
  ActiveControl = Viewer
  Caption = 'RAW View'
  ClientHeight = 572
  ClientWidth = 851
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 647
    Height = 547
    Camera = Camera
    PostRender = ViewerPostRender
    Buffer.BackgroundColor = clSkyBlue
    Buffer.FaceCulling = False
    FieldOfView = 143.570602416992200000
    Align = alClient
    TabOrder = 0
  end
  object Inspector: TJvInspector
    Left = 647
    Top = 0
    Width = 204
    Height = 547
    Align = alRight
    ItemHeight = 16
    TabStop = True
    TabOrder = 1
  end
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 547
    Width = 851
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
  object Scene: TGLScene
    Left = 8
    Top = 8
    object Camera: TGLCamera
      DepthOfView = 10000.000000000000000000
      FocalLength = 90.000000000000000000
      TargetObject = CameraTarget
      Position.Coordinates = {7EBAEE40C3686440C36864400000803F}
      object CamLight: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object DummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {029F1F3FBEBEBE3E999F1F3F0000803F}
    end
    object HeightField: TGLHeightField
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Material.Texture.Border = 0
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      XSamplingScale.Max = 16.000000000000000000
      XSamplingScale.Step = 1.000000000000000000
      YSamplingScale.Max = 16.000000000000000000
      YSamplingScale.Step = 1.000000000000000000
      ColorMode = hfcmDiffuse
      object WaterPlane: TGLPlane
        Material.FrontProperties.Diffuse.Color = {00000000FDFC7C3ECDCC4C3F4A0C023E}
        Material.FrontProperties.Emission.Color = {EBEAEA3ED7D6D63EDFDEDE3E0000803F}
        Material.BlendingMode = bmModulate
        Material.Texture.Border = 0
        Up.Coordinates = {000000000000803F0000008000000000}
        Height = 16.000000000000000000
        Width = 16.000000000000000000
        NoZWrite = False
      end
      object CameraTarget: TGLDummyCube
        CubeSize = 1.000000000000000000
      end
    end
  end
  object Navigation: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = Viewer
    ZoomSpeed = 1.049999952316284000
    FormCaption = 'RAW View - %FPS'
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
    OnMouseMove = ViewerMouseMove
    Left = 8
    Top = 40
  end
end
