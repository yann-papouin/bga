object WaitForm: TWaitForm
  Left = 0
  Top = 0
  Cursor = crHourGlass
  BorderIcons = []
  BorderStyle = bsSingle
  ClientHeight = 96
  ClientWidth = 299
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  GlassFrame.SheetOfGlass = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    299
    96)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelText: TLabel
    Left = 8
    Top = 16
    Width = 283
    Height = 33
    Cursor = crHourGlass
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'DEFAULT'
    Transparent = True
    Layout = tlCenter
    WordWrap = True
    ExplicitTop = 24
    ExplicitWidth = 354
  end
  object Panel: TSpTBXPanel
    Left = 8
    Top = 8
    Width = 34
    Height = 41
    Cursor = crHourGlass
    TabOrder = 0
    Visible = False
    TBXStyleBackground = True
  end
  object WaitBar: TProgressBar
    Left = 56
    Top = 53
    Width = 186
    Height = 24
    Cursor = crHourGlass
    Anchors = [akLeft, akTop, akRight, akBottom]
    DoubleBuffered = False
    Max = 13
    ParentDoubleBuffered = False
    Smooth = True
    TabOrder = 1
  end
  object AsyncTimer1: TAsyncTimer
    Interval = 1
    Left = 16
    Top = 8
  end
end
