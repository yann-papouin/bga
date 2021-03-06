inherited RAWViewForm: TRAWViewForm
  Caption = 'RAW View'
  ClientHeight = 572
  ClientWidth = 1049
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyPress = FormKeyPress
  ExplicitWidth = 1057
  ExplicitHeight = 599
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1049
    Height = 549
    Camera = Camera
    PostRender = ViewerPostRender
    Buffer.BackgroundColor = clWhite
    Buffer.FaceCulling = False
    FieldOfView = 143.694595336914100000
    Align = alClient
    OnDblClick = ViewerDblClick
    OnMouseDown = ViewerMouseDown
    TabOrder = 0
  end
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 549
    Width = 1049
    Height = 23
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
    object CameraTarget: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Camera: TGLCamera
        DepthOfView = 512.000000000000000000
        FocalLength = 90.000000000000000000
        TargetObject = CameraTarget
        Position.Coordinates = {00000000000020410000A0400000803F}
        object CamLight: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object Root: TGLDummyCube
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000803F000000000000008000000000}
      CubeSize = 1.000000000000000000
      object WaterPlane: TGLPlane
        Material.FrontProperties.Diffuse.Color = {CDCC4C3EC1C0C03DC3C2423F17D92E3F}
        Material.FrontProperties.Emission.Color = {C5C4443EC5C4443EB5B4343E0000803F}
        Material.FrontProperties.Shininess = 5
        Material.FrontProperties.Specular.Color = {9796963E9D9C1C3FCFCECE3E0000803F}
        Material.BlendingMode = bmTransparency
        Up.Coordinates = {000000000000803F0000008000000000}
        Height = 16.000000000000000000
        Width = 16.000000000000000000
      end
      object TerrainRenderer: TGLTerrainRenderer
        Material.BackProperties.Ambient.Color = {CDCC4C3ED5D4543FCDCC4C3E0000803F}
        Material.BackProperties.Diffuse.Color = {CDCC4C3F8180003FCDCC4C3F0000803F}
        Material.BackProperties.Emission.Color = {00000000A5A4243FE2E1613F0000803F}
        Material.BackProperties.Shininess = 128
        Material.BackProperties.Specular.Color = {B7B6363F00000000F4F3733F0000803F}
        Material.FrontProperties.Ambient.Color = {F7F6F63EA7A6A63ED3D2D23E0000803F}
        Material.FrontProperties.Diffuse.Color = {00000000FDFC7C3E9594143F0000803F}
        Material.FrontProperties.Emission.Color = {EBEAEA3ED7D6D63EF3F2F23E0000803F}
        Material.FrontProperties.Shininess = 8
        Material.Texture.Image.Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000C80000
          00C8100200000072AAE58A000000097048597300000048000000480046C96B3E
          0000000976704167000000C8000000C800EB4A7C96000007B04944415478DAED
          DDB16D2B571445514DE24A5C811B72E4AA7EE482EC0A5C891319E2248202729F
          C47AC05F6B124111B17971C06CAEF7B78FE71C7F3F9E3F1ECFFDF7097E7B3C3F
          1ECFFDF709B4EAB4EACE6C75BB0C56E1B03AAD3AAD56062B71589D569D562B83
          9538AC4EAB4EAB95C14A1C56A755A7D5CA60250EABD3AAD36A65B01287D569D5
          69B5325889C3EAB4EAB45A19ACC461755A755AAD0C56E2B03AAD3AAD56062B71
          589D569D562B839538AC4EAB4EAB95C14A1C56A755A7D5CA60250EABD3AAD36A
          65B01287D569D569B5325889C3EAB4EAB45A19ACC461755A755AAD0C56E2B03A
          AD3AAD56062B71589D569D562B839538AC4EAB4EAB95C14A1C56A755A7D5CA60
          250EABD3AAD36A65B01287D569D569B5325889C3EAB4EAB45A5D7FBD7D3C27F8
          E5F1FCF378FE7C3CF7DFF7FFBFCBBF8FE7D7C7F3FBE3B9FFBEFFAF95565AFD9F
          AE7376DD57A895565A3D777DF707F8CA8FE44EAB4EABEECC5637839538AC4EAB
          4EAB95C14A1C56A755A7D5CA60250EABD3AAD36A65B01287D569D569B5325889
          C3EAB4EAB45A19ACC461755A755AAD0C56E2B03AAD3AAD56062B71589D569D56
          2B839538AC4EAB4EAB95C14A1C56A755A7D5CA60250EABD3AAD36A65B01287D5
          69D569B5325889C3EAB4EAB45A19ACC461755A755AAD0C56E2B03AAD3AAD5606
          2B71589D569D562B839538AC4EAB4EAB95C14A1C56A755A7D5CA60250EABD3AA
          D36A65B01287D569D569B5325889C3EAB4EAB45A19ACC461755A755AADBC97F0
          05EF8FD34AAB83DE4BE8CDCFCF9DF9156AA5D5CFD3EAB3EBFDEDE339871FC99D
          569D56DD99AD6E062B71589D569D562B839538AC4EAB4EAB95C14A1C56A755A7
          D5CA60250EABD3AAD36A65B01287D569D569B5325889C3EAB4EAB45A19ACC461
          755A755AAD0C56E2B03AAD3AAD56062B71589D569D562B839538AC4EAB4EAB95
          C14A1C56A755A7D5CA60250EABD3AAD36A65B01287D569D569B5325889C3EAB4
          EAB45A19ACC461755A755AAD0C56E2B03AAD3AAD56062B71589D569D562B8395
          38AC4EAB4EAB95C14A1C56A755A7D5CA60250EABD3AAD36A65B01287D569D569
          B5325889C3EAB4EAB45A792FE10BDE1FA7955607BD97F09C5DF7156AA59556CF
          5DDFFD01BEF223B9D3AAD3AA3BB3D5CD60250EABD3AAD36A65B01287D569D569
          B5325889C3EAB4EAB45A19ACC461755A755AAD0C56E2B03AAD3AAD56062B7158
          9D569D562B839538AC4EAB4EAB95C14A1C56A755A7D5CA60250EABD3AAD36A65
          B01287D569D569B5325889C3EAB4EAB45A19ACC461755A755AAD0C56E2B03AAD
          3AAD56062B71589D569D562B839538AC4EAB4EAB95C14A1C56A755A7D5CA6025
          0EABD3AAD36A65B01287D569D569B5325889C3EAB4EAB45A19ACC461755A755A
          AD0C56E2B03AAD3AAD56062B71589D569D562BEF257CC1FBE3B4D2EAA0F7127A
          F3F373677E855A69F5F3B4FAEC7A7FFB78CEE14772A755A7557766AB9BC14A1C
          56A755A7D5CA60250EABD3AAD36A65B01287D569D569B5325889C3EAB4EAB45A
          19ACC461755A755AAD0C56E2B03AAD3AAD56062B71589D569D562B839538AC4E
          AB4EAB95C14A1C56A755A7D5CA60250EABD3AAD36A65B01287D569D569B53258
          89C3EAB4EAB45A19ACC461755A755AAD0C56E2B03AAD3AAD56062B71589D569D
          562B839538AC4EAB4EAB95C14A1C56A755A7D5CA60250EABD3AAD36A65B01287
          D569D569B5325889C3EAB4EAB45A19ACC461755A755AAD0C56E2B03AAD3AAD56
          DE4BF882F7C769A5D541EF253C67D77D855A69A5D573D7777F80AFFC48EEB4EA
          B4EACE6C75335889C3EAB4EAB45A19ACC461755A755AAD0C56E2B03AAD3AAD56
          062B71589D569D562B839538AC4EAB4EAB95C14A1C56A755A7D5CA60250EABD3
          AAD36A65B01287D569D569B5325889C3EAB4EAB45A19ACC461755A755AAD0C56
          E2B03AAD3AAD56062B71589D569D562B839538AC4EAB4EAB95C14A1C56A755A7
          D5CA60250EABD3AAD36A65B01287D569D569B5325889C3EAB4EAB45A19ACC461
          755A755AAD0C56E2B03AAD3AAD56062B71589D569D562B839538AC4EAB4EAB95
          C14A1C56A755A7D5CA7B095FF0FE38ADB43AE8BD84DEFCFCDC995FA1565AFD3C
          AD3EBBDEDF3E9E73F891DC69D569D59DD9EA66B01287D569D569B5325889C3EA
          B4EAB45A19ACC461755A755AAD0C56E2B03AAD3AAD56062B71589D569D562B83
          9538AC4EAB4EAB95C14A1C56A755A7D5CA60250EABD3AAD36A65B01287D569D5
          69B5325889C3EAB4EAB45A19ACC461755A755AAD0C56E2B03AAD3AAD56062B71
          589D569D562B839538AC4EAB4EAB95C14A1C56A755A7D5CA60250EABD3AAD36A
          65B01287D569D569B5325889C3EAB4EAB45A19ACC461755A755AAD0C56E2B03A
          AD3AAD56062B71589D569D562B839538AC4EAB4EAB95F712BEE0FD715A6975D0
          7B09CFD9755FA1565A69F5DCF5DD1FE02B3F923BAD3AADBA335BDD0C56E2B03A
          AD3AAD56062B71589D569D562B839538AC4EAB4EAB95C14A1C56A755A7D5CA60
          250EABD3AAD36A65B01287D569D569B5325889C3EAB4EAB45A19ACC461755A75
          5AAD0C56E2B03AAD3AAD56062B71589D569D562B839538AC4EAB4EAB95C14A1C
          56A755A7D5CA60250EABD3AAD36A65B01287D569D569B5325889C3EAB4EAB45A
          19ACC461755A755AAD0C56E2B03AAD3AAD56062B71589D569D562B839538AC4E
          AB4EAB95C14A1C56A755A7D5CA60250EABD3AAD36A65B01287D569D569B53258
          89C3EAB4EAB45AFD075258EF23917BADB00000000049454E44AE426082}
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        HeightDataSource = BattlefieldHDS
        TilesPerTexture = 1.000000000000000000
        MaterialLibrary = GLMaterialLibrary
        QualityDistance = 128.000000000000000000
        QualityStyle = hrsTesselated
        MaxCLODTriangles = 128000
      end
    end
    object GLInfos: TGLHUDText
      Position.Coordinates = {0000204100002041000000000000803F}
      BitmapFont = WindowsBitmapFont
      Text = 'GLInfos'
      ModulateColor.Color = {B1A8A83EB1A8A83EB1A8A83E0000803F}
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
  object WindowsBitmapFont: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 8
    Top = 136
  end
  object Cadencer: TGLCadencer
    Scene = Scene
    Enabled = False
    SleepLength = 10
    OnProgress = CadencerProgress
    Left = 8
    Top = 168
  end
end
