object RFASettingsForm: TRFASettingsForm
  AlignWithMargins = True
  Left = 0
  Top = 0
  Caption = 'RFASettingsForm'
  ClientHeight = 491
  ClientWidth = 589
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 10
  Padding.Top = 10
  Padding.Right = 10
  Padding.Bottom = 10
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXGroupBox1: TSpTBXGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 569
    Height = 321
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Caption = 'Extension editor association'
    Align = alTop
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 0
    object ValueListEditor1: TValueListEditor
      Left = 12
      Top = 25
      Width = 545
      Height = 284
      Align = alClient
      Strings.Strings = (
        '.con='
        '.dds='
        '.txt='
        '.tga='
        '.raw=')
      TabOrder = 0
      TitleCaptions.Strings = (
        'Extension'
        'Application')
      ColWidths = (
        150
        389)
    end
  end
end
