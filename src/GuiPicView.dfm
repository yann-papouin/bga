inherited PICViewForm: TPICViewForm
  Caption = 'Picture View'
  ClientHeight = 496
  ClientWidth = 522
  OnClose = FormClose
  OnMouseWheel = FormMouseWheel
  ExplicitWidth = 538
  ExplicitHeight = 534
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 522
    Height = 496
    Camera = GLCamera
    Buffer.BackgroundColor = 3815994
    Buffer.AmbientColor.Color = {0000803F0000803F0000803F0000803F}
    FieldOfView = 164.840972900390600000
    Align = alClient
    OnDblClick = ViewerDblClick
    OnMouseDown = ViewerMouseDown
    OnMouseMove = ViewerMouseMove
    OnMouseUp = ViewerMouseUp
    TabOrder = 0
  end
  object GLScene: TGLScene
    Left = 8
    Top = 64
    object GLZoomPlane: TGLPlane
      Material.FrontProperties.Diffuse.Color = {00000000000000000000000046B6F33E}
      Material.BlendingMode = bmTransparency
      Material.Texture.Border = 0
      Position.Coordinates = {0000000000000000CDCCCC3D0000803F}
      Height = 25.000000000000000000
      Width = 55.000000000000000000
      NoZWrite = False
      object GLZoom: TGLAbsoluteHUDText
        BitmapFont = WindowsBitmapFont
        Text = 'Zoom'
        Alignment = taCenter
        Layout = tlCenter
      end
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 33.000000000000000000
      CameraStyle = csOrtho2D
      Position.Coordinates = {000080C3000080C3000000000000803F}
    end
    object Plane: TGLPlane
      Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
      Material.Texture.MinFilter = miLinear
      Material.Texture.Border = 0
      Height = 64.000000000000000000
      Width = 64.000000000000000000
      NoZWrite = False
    end
    object GLFilename: TGLHUDText
      Position.Coordinates = {0000204100002041000000000000803F}
      BitmapFont = WindowsBitmapFont
      Text = 'GLFilename'
    end
    object GLResolution: TGLHUDText
      Position.Coordinates = {000020410000F041000000000000803F}
      BitmapFont = WindowsBitmapFont
      Text = 'GLResolution'
    end
  end
  object WindowsBitmapFont: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 8
    Top = 96
  end
  object Cadencer: TGLCadencer
    Scene = GLScene
    Enabled = False
    SleepLength = 15
    OnProgress = CadencerProgress
    Left = 8
    Top = 128
  end
end
