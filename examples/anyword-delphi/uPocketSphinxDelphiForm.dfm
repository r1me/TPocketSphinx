object PocketSphinxDelphiForm: TPocketSphinxDelphiForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  ClientHeight = 228
  ClientWidth = 383
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
  object lbStatus: TLabel
    Left = 13
    Top = 16
    Width = 35
    Height = 13
    Caption = 'Status:'
    Color = clBtnFace
    ParentColor = False
  end
  object cbClearOnHypothesis: TCheckBox
    Left = 13
    Top = 194
    Width = 169
    Height = 17
    Caption = 'Clear text on every hypothesis'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object memHypothesis: TMemo
    Left = 13
    Top = 43
    Width = 356
    Height = 134
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
