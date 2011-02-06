object MainForm: TMainForm
  Left = 0
  Top = 0
  ActiveControl = Log
  Caption = 'MainForm'
  ClientHeight = 407
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Log: TMemo
    Left = 0
    Top = 0
    Width = 365
    Height = 296
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitHeight = 407
  end
  object Panel1: TPanel
    Left = 0
    Top = 296
    Width = 365
    Height = 111
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      365
      111)
    object Code: TSpinEdit
      Left = 8
      Top = 6
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 100
    end
    object Start: TButton
      Left = 135
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 1
      OnClick = StartClick
    end
    object Dbg: TMemo
      Left = 8
      Top = 34
      Width = 345
      Height = 71
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 2
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 312
    Top = 8
  end
end
