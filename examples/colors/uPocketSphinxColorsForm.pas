unit uPocketSphinxColorsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, uPocketSphinx, uPocketSphinxDefaultAudioSource;

type

  { TPocketSphinxColorForm }

  TPocketSphinxColorForm = class(TForm)
    lbHypothesis: TLabel;
    lbStatus: TLabel;
    spColorBox: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    PocketSphinx: TPocketSphinx;

    procedure OnPocketSphinxStateChange(Sender: TObject; AState: TPocketSphinxState);
    procedure OnPocketSphinxHypothesis(Sender: TObject; AScore: Integer; AHypothesis: String);
  end;

var
  PocketSphinxColorForm: TPocketSphinxColorForm;

implementation

{$R *.lfm}

{ TPocketSphinxColorForm }

procedure TPocketSphinxColorForm.FormCreate(Sender: TObject);
begin
  PocketSphinx := TPocketSphinx.Create;

  PocketSphinx.OnStateChange := @OnPocketSphinxStateChange;
  PocketSphinx.OnHypothesis := @OnPocketSphinxHypothesis;

  PocketSphinx.AcousticModelPath := 'en-us/en-us';
  PocketSphinx.Threshold := -3000;

  PocketSphinx.Init;
  if PocketSphinx.State = rsInitialized then
  begin
    if PocketSphinx.LoadDictionary('colors/colors.dic') then
    begin
      PocketSphinx.AddGrammarSearchFile('colors', 'colors/colors.gram');
      PocketSphinx.ActiveSearch := 'colors';
    end;

    PocketSphinx.AudioSource := TAudioSourceDefaultDevice.Create;

    PocketSphinx.Active := True;
  end;
end;

procedure TPocketSphinxColorForm.FormDestroy(Sender: TObject);
begin
  PocketSphinx.Active := False;
  PocketSphinx.Free;
end;

procedure TPocketSphinxColorForm.OnPocketSphinxStateChange(Sender: TObject;
  AState: TPocketSphinxState);
begin
  case AState of
    rsNotInitialized: lbStatus.Caption := 'Status: Not Initialized';
    rsInitialized: lbStatus.Caption := 'Status: Initialized';
    rsReady: lbStatus.Caption := 'Status: Ready';
    rsListening: lbStatus.Caption := 'Status: Listening...';
    rsAnalyze: lbStatus.Caption := 'Status: Analyzing...';
    rsError: lbStatus.Caption := 'Status: Error - ' + (Sender as TPocketSphinx).LastErrorMsg;
  end;
end;

procedure TPocketSphinxColorForm.OnPocketSphinxHypothesis(Sender: TObject;
  AScore: Integer; AHypothesis: String);
begin
  lbHypothesis.Caption := Format('Recent hypothesis: %s with score %d', [AHypothesis, AScore]);

  if AHypothesis = 'black' then
    spColorBox.Brush.Color := clBlack
  else if AHypothesis = 'blue' then
    spColorBox.Brush.Color := clBlue
  else if AHypothesis = 'green' then
    spColorBox.Brush.Color := clGreen
  else if AHypothesis = 'red' then
    spColorBox.Brush.Color := clRed
  else if AHypothesis = 'yellow' then
    spColorBox.Brush.Color := clYellow
  else if AHypothesis = 'white' then
    spColorBox.Brush.Color := clWhite;
end;

end.

