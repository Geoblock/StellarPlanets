object FormStarPlanets: TFormStarPlanets
  Left = 412
  Top = 123
  Cursor = crHelp
  Caption = 'Stellar Planets'
  ClientHeight = 547
  ClientWidth = 769
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  TextHeight = 13
  object SceneViewer: TGLSceneViewer
    Left = 142
    Top = 0
    Width = 627
    Height = 528
    Cursor = crCross
    Camera = Camera
    BeforeRender = SceneViewerBeforeRender
    Buffer.BackgroundColor = clBlack
    FieldOfView = 138.680328369140600000
    PenAsTouch = False
    Align = alClient
    OnDblClick = SceneViewerDblClick
    OnMouseDown = SceneViewerMouseDown
    OnMouseMove = SceneViewerMouseMove
    TabOrder = 0
  end
  object PanelTree: TPanel
    Left = 0
    Top = 0
    Width = 142
    Height = 528
    Align = alLeft
    TabOrder = 1
    object TreeView: TTreeView
      Left = 1
      Top = 1
      Width = 140
      Height = 526
      Align = alClient
      AutoExpand = True
      HideSelection = False
      Indent = 19
      TabOrder = 0
      OnClick = TreeViewClick
      Items.NodeData = {
        0319000000240000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        00000000000103530075006E002C0000000100000000000000FFFFFFFFFFFFFF
        FF00000000000000000000000001074D00650072006300750072007900280000
        000200000000000000FFFFFFFFFFFFFFFF000000000000000000000000010556
        0065006E0075007300280000000300000000000000FFFFFFFFFFFFFFFF000000
        000000000001000000010545006100720074006800260000001F000000000000
        00FFFFFFFFFFFFFFFF00000000000000000000000001044D006F006F006E0026
        0000000400000000000000FFFFFFFFFFFFFFFF00000000000000000200000001
        044D006100720073002A000000290000000000000001000000FFFFFFFF000000
        0000000000000000000106500068006F0062006F0073002A0000002A00000000
        00000001000000FFFFFFFF00000000000000000000000001064400650069006D
        006F0073002C0000000500000000000000FFFFFFFFFFFFFFFF00000000000000
        000500000001074A007500700069007400650072002200000033000000000000
        00FFFFFFFFFFFFFFFF000000000000000000000000010249006F002A00000034
        00000000000000FFFFFFFFFFFFFFFF0000000000000000000000000106450075
        0072006F00700061002E0000003500000000000000FFFFFFFFFFFFFFFF000000
        0000000000000000000108470061006E0079006D006500640065002E00000036
        00000000000000FFFFFFFFFFFFFFFF0000000000000000000000000108430061
        006C006C006900730074006F002E000000370000000000000001000000FFFFFF
        FF000000000000000000000000010841006D0061006C0074006800650061002A
        0000000600000000000000FFFFFFFFFFFFFFFF00000000000000000C00000001
        06530061007400750072006E00300000003D00000000000000FFFFFFFFFFFFFF
        FF000000000000000000000000010945006E00630065006C0061006400750073
        00280000003E00000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        00010554006900740061006E00280000003F00000000000000FFFFFFFFFFFFFF
        FF00000000000000000000000001054D0069006D00610073002A000000400000
        0000000000FFFFFFFFFFFFFFFF00000000000000000000000001065400650074
        00680079007300280000004100000000000000FFFFFFFFFFFFFFFF0000000000
        000000000000000105440069006F006E006500260000004200000000000000FF
        FFFFFFFFFFFFFF000000000000000000000000010452006800650061002C0000
        004300000000000000FFFFFFFFFFFFFFFF000000000000000000000000010749
        0061007000650074007500730028000000440000000000000001000000FFFFFF
        FF00000000000000000000000001054A0061006E00750073002E000000450000
        000000000001000000FFFFFFFF00000000000000000000000001084800790070
        006500720069006F006E0032000000620200000000000001000000FFFFFFFF00
        0000000000000000000000010A500072006F006D006500740068006500750073
        002A000000630200000000000001000000FFFFFFFF0000000000000000000000
        000106500068006F006500620065002C000000000000000000000001000000FF
        FFFFFF0000000000000000000000000107500061006E0064006F00720061002A
        0000000700000000000000FFFFFFFFFFFFFFFF00000000000000000500000001
        065500720061006E00750073002C0000004700000000000000FFFFFFFFFFFFFF
        FF00000000000000000000000001074D006900720061006E0064006100280000
        004800000000000000FFFFFFFFFFFFFFFF000000000000000000000000010541
        007200690065006C002C0000004900000000000000FFFFFFFFFFFFFFFF000000
        000000000000000000010755006D0062007200690065006C002C0000004A0000
        0000000000FFFFFFFFFFFFFFFF00000000000000000000000001075400690074
        0061006E00690061002A0000004B00000000000000FFFFFFFFFFFFFFFF000000
        00000000000000000001064F006200650072006F006E002C0000000800000000
        000000FFFFFFFFFFFFFFFF00000000000000000400000001074E006500700074
        0075006E0065002C000000000000000000000001000000FFFFFFFF0000000000
        0000000000000001074C006100720069007300730061002A0000000000000000
        000000FFFFFFFFFFFFFFFF00000000000000000000000001064E006500720065
        00690064002A0000005100000000000000FFFFFFFFFFFFFFFF00000000000000
        0000000000010654007200690074006F006E002C000000580000000000000001
        000000FFFFFFFF0000000000000000000000000107500072006F007400650075
        007300280000000900000000000000FFFFFFFFFFFFFFFF000000000000000001
        000000010550006C00750074006F002A0000005B00000000000000FFFFFFFFFF
        FFFFFF000000000000000000000000010643006800610072006F006E00280000
        000A00000000000000FFFFFFFFFFFFFFFF000000000000000000000000010543
        006500720065007300440000000B00000000000000FFFFFFFFFFFFFFFF000000
        000000000000000000011345007200690073005F002800640077006100720066
        005F0070006C0061006E006500740029002A0000000C00000000000000FFFFFF
        FFFFFFFFFF00000000000000000000000001064800610075006D006500610028
        0000000D0000000000000001000000FFFFFFFF00000000000000000000000001
        05560065007300740061002C000000F30000000000000001000000FFFFFFFF00
        000000000000000000000001073200340033005F004900640061002E000000B1
        0100000000000001000000FFFFFFFF0000000000000000000000000108340033
        0033005F00450072006F00730032000000B70300000000000001000000FFFFFF
        FF000000000000000000000000010A3900350031005F00470061007300700072
        0061003C000000540600000000000001000000FFFFFFFF000000000000000000
        000000010F31003600320030005F00470065006F00670072006100700068006F
        0073003C000000120700000000000001000000FFFFFFFF000000000000000000
        000000010F31003800310030005F004500700069006D00650074006800650075
        00730030000000CE0700000000000001000000FFFFFFFF000000000000000000
        000000010931003900390038005F004B00590032003600360000000F08000000
        00000001000000FFFFFFFF000000000000000000000000010C32003000360033
        005F004200610063006300680075007300380000005310000000000000010000
        00FFFFFFFF000000000000000000000000010D34003100370039005F0054006F
        0075007400610074006900730038000000A11200000000000001000000FFFFFF
        FF000000000000000000000000010D34003700360039005F0043006100730074
        0061006C006900610036000000591900000000000001000000FFFFFFFF000000
        000000000000000000010C36003400380039005F0047006F006C00650076006B
        006100340000000961010000000000FFFFFFFFFFFFFFFF000000000000000000
        000000010B390030003300370037005F005300650064006E006100}
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 528
    Width = 769
    Height = 19
    Panels = <>
  end
  object Scene: TGLScene
    ObjectsSorting = osNone
    Left = 208
    Top = 39
    object AstroSkyDome: TGLSkyDome
      Bands = <
        item
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 15.000000000000000000
        end
        item
          StartAngle = 15.000000000000000000
          StopAngle = 90.000000000000000000
          StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}
          Stacks = 4
        end>
      Stars = <>
      object ConstelLines: TGLLines
        Direction.Coordinates = {0000803F000000000000008000000000}
        Scale.Coordinates = {00A00C4600A00C4600A00C4600000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        Visible = False
        AntiAliased = True
        LineColor.Color = {E3A51B3FE3A51B3F0000803F00000000}
        LineWidth = 2.000000000000000000
        Nodes = <>
        NodesAspect = lnaInvisible
        SplineMode = lsmSegments
        Options = []
      end
      object ConstelBoundaries: TGLLines
        Nodes = <>
        SplineMode = lsmSegments
        Options = []
      end
    end
    object PlanetSkyDome: TGLEarthSkyDome
      Visible = False
      Bands = <>
      Stars = <>
      SunElevation = 75.000000000000000000
      Turbidity = 15.000000000000000000
      ExtendedOptions = []
      Slices = 48
      Stacks = 24
    end
    object dcStar: TGLDummyCube
      ObjectsSorting = osNone
      CubeSize = 1.000000000000000000
      object LightStar: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {00D0044600D00446000000000000803F}
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
        object LensStar: TGLLensFlare
          Seed = 1465
          FlareIsNotOccluded = True
        end
      end
      object CameraControler: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = dcStar
        Position.Coordinates = {0000803F000000000000803F0000803F}
        Direction.Coordinates = {0000803F000000000000008000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
      end
      object Camera: TGLCamera
        DepthOfView = 200000.000000000000000000
        FocalLength = 99.545860290527340000
        NearPlaneBias = 0.100000001490116100
        TargetObject = dcStar
        CameraStyle = csInfinitePerspective
        Position.Coordinates = {0000803F000000000000803F0000803F}
        Direction.Coordinates = {0000803F000000000000008000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
      end
      object dcMoon: TGLDummyCube
        Up.Coordinates = {FC9D7FB10000803F0000000000000000}
        CubeSize = 1.000000000000000000
        object Moon: TGLSphere
          Material.MaterialLibrary = MatLib
          Material.LibMaterialName = 'moon'
          Direction.Coordinates = {D947AABE616D713F0000000000000000}
          Position.Coordinates = {CDCCECC100000000000000000000803F}
          Scale.Coordinates = {713D8A3E713D8A3E713D8A3E00000000}
          TurnAngle = -170.000000000000000000
          Up.Coordinates = {00000000000000000000803F00000000}
          Radius = 0.500000000000000000
          Slices = 32
          Stacks = 32
        end
      end
      object Planetoid: TGLFreeForm
        Material.MaterialLibrary = MatLib
        Direction.Coordinates = {00000000000080BF0000000000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        Visible = False
        MaterialLibrary = MatLib
      end
      object Planet: TGLSphere
        Material.LibMaterialName = 'earthDay'
        Direction.Coordinates = {000000000000803F0000000000000000}
        TurnAngle = -150.000000000000000000
        Up.Coordinates = {00000000000000800000803F00000000}
        Radius = 0.500000000000000000
        Slices = 64
        Stacks = 64
        object PlanetRingDn: TGLDisk
          Material.Texture.Disabled = False
          Direction.Coordinates = {000000000000803F2EBD3BB300000000}
          PitchAngle = 90.000000000000000000
          Up.Coordinates = {000000002EBD3BB3000080BF00000000}
          Visible = False
          InnerRadius = 0.600000023841857900
          OuterRadius = 0.899999976158142100
          Slices = 64
          SweepAngle = 360.000000000000000000
        end
        object PlanetRingUp: TGLDisk
          Material.Texture.Disabled = False
          Direction.Coordinates = {000000000000803F2EBD3BB300000000}
          PitchAngle = 90.000000000000000000
          Up.Coordinates = {000000002EBD3BB3000080BF00000000}
          Visible = False
          NormalDirection = ndInside
          InnerRadius = 0.600000023841857900
          OuterRadius = 0.899999976158142100
          Slices = 64
          SweepAngle = 360.000000000000000000
        end
        object PlanetCore: TGLDisk
          Material.Texture.Disabled = False
          Direction.Coordinates = {0000803F2EBD3BB3583DAF2600000000}
          PitchAngle = 90.000000000000000000
          Position.Coordinates = {0AD723BC00000000000000000000803F}
          TurnAngle = 90.000000000000000000
          Up.Coordinates = {000000002EBD3BB3000080BF00000000}
          NormalDirection = ndInside
          OuterRadius = 0.499000012874603300
          Slices = 64
          SweepAngle = 360.000000000000000000
        end
        object Atmosphere: TGLAtmosphere
          Sun = LightStar
          Visible = False
        end
      end
      object DirectOpenGL: TGLDirectOpenGL
        UseBuildList = False
        OnRender = DirectOpenGLRender
        Blend = False
      end
    end
  end
  object Cadencer: TGLCadencer
    Scene = Scene
    MaxDeltaTime = 0.050000000000000000
    OnProgress = CadencerProgress
    Left = 282
    Top = 43
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 509
    Top = 140
  end
  object MatLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'earthDay'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureFormat = tfLuminance
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
      end>
    Left = 212
    Top = 125
  end
  object TexCombiner: TGLTexCombineShader
    Combiners.Strings = (
      'Tex0:=Tex0;'
      'Tex1:=InterPolate(Tex0, Tex1, PrimaryColor);'
      '')
    DesignTimeEnabled = False
    Left = 298
    Top = 130
  end
  object MainMenu: TMainMenu
    Left = 501
    Top = 56
    object Open1: TMenuItem
      Caption = 'File'
      object miOpenFile: TMenuItem
        Caption = 'Open...'
        OnClick = miOpenFileClick
      end
      object Saveas1: TMenuItem
        Caption = 'Save as...'
        OnClick = Saveas1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object N1: TMenuItem
      Caption = 'View'
      object miShowHidePlanet: TMenuItem
        Caption = 'Hide Planet'
        OnClick = miShowHidePlanetClick
      end
      object miInnerCore: TMenuItem
        Caption = 'Inner Core '
        OnClick = miInnerCoreClick
      end
      object miPlanetSkyDome: TMenuItem
        Caption = 'Planet Skydome'
        OnClick = miPlanetSkyDomeClick
      end
      object NightLights1: TMenuItem
        Caption = 'Night Lights'
      end
    end
    object miSystem: TMenuItem
      Caption = 'Stars'
      object miSolarSys: TMenuItem
        Caption = 'Solar System...'
        OnClick = miSolarSysClick
      end
      object miKepler22: TMenuItem
        Caption = 'Kepler-22...'
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object miConstelLines: TMenuItem
        Caption = 'Constellation Lines'
        OnClick = miConstelLinesClick
      end
      object miConstelBoundaries: TMenuItem
        Caption = 'Constellation Boundaries'
        OnClick = miConstelBoundariesClick
      end
      object Projection1: TMenuItem
        Caption = 'Projection...'
        OnClick = Projection1Click
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object miShowHideTV: TMenuItem
        Caption = 'Hide TreeView'
        OnClick = miShowHideTVClick
      end
      object miClearTreeView: TMenuItem
        Caption = 'Clear TreeView'
        Enabled = False
        OnClick = miClearTreeViewClick
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object miWikiHelp: TMenuItem
        Caption = 'Wikipedia...'
        Hint = 'Information'
        ShortCut = 112
        OnClick = miWikiHelpClick
      end
      object miGoogleEarth: TMenuItem
        Caption = 'Google Earth...'
        OnClick = miGoogleEarthClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = 'About...'
        OnClick = About1Click
      end
    end
  end
  object OpenDialog: TOpenDialog
    Left = 524
    Top = 297
  end
  object SaveDialog: TSaveDialog
    Left = 271
    Top = 327
  end
  object ImageList1: TImageList
    Left = 286
    Top = 493
  end
  object ImageCollection1: TImageCollection
    Images = <>
    Left = 386
    Top = 490
  end
end
