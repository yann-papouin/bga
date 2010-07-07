object RFASettingsForm: TRFASettingsForm
  AlignWithMargins = True
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 491
  ClientWidth = 421
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
    Width = 401
    Height = 215
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 10
    Caption = 'Extension editor association'
    Align = alTop
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 0
    ExplicitWidth = 569
    object ValueListEditor1: TValueListEditor
      Left = 12
      Top = 25
      Width = 377
      Height = 178
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
      ExplicitWidth = 361
      ColWidths = (
        150
        221)
    end
  end
  object SpTBXGroupBox2: TSpTBXRadioGroup
    AlignWithMargins = True
    Left = 10
    Top = 235
    Width = 401
    Height = 94
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 10
    Caption = 'On double click'
    Align = alTop
    TabOrder = 1
    ItemIndex = 0
    Items.Strings = (
      'Open/Edit the file with OS associated extension'
      'Open/Edit the file with BGA associated extension'
      'Preview the file with the internal viewer')
    ExplicitLeft = 22
    ExplicitTop = 402
  end
  object FormStorage: TJvFormStorage
    AppStorage = RFAViewForm.AppStorage
    AppStoragePath = '%FORM_NAME%\'
    Options = []
    StoredValues = <>
    Left = 304
    Top = 8
  end
end
