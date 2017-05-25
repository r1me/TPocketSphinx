unit uPocketSphinxAnyWordForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, uPocketSphinx, uPocketSphinxDefaultAudioSource;

type

  { TPocketSphinxAnyWordForm }

  TPocketSphinxAnyWordForm = class(TForm)
    cbClearOnHypothesis: TCheckBox;
    lbStatus: TLabel;
    memHypothesis: TMemo;
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
  PocketSphinxAnyWordForm: TPocketSphinxAnyWordForm;

implementation

{$R *.lfm}

{ TPocketSphinxAnyWordForm }

procedure TPocketSphinxAnyWordForm.FormCreate(Sender: TObject);
begin
  PocketSphinx := TPocketSphinx.Create;

  PocketSphinx.OnStateChange := @OnPocketSphinxStateChange;
  PocketSphinx.OnHypothesis := @OnPocketSphinxHypothesis;

  PocketSphinx.AcousticModelPath := 'en-us/en-us';
  PocketSphinx.Threshold := 0;

  PocketSphinx.Init;
  if PocketSphinx.State = rsInitialized then
  begin
    if PocketSphinx.LoadDictionary('en-us/cmudict-en-us.dict') then
    begin
      PocketSphinx.AddNgramSearch('ngram', 'en-us/en-us.lm.bin');
      PocketSphinx.ActiveSearch := 'ngram';
    end;

    PocketSphinx.AudioSource := TAudioSourceDefaultDevice.Create;

    PocketSphinx.Active := True;
  end;
end;

procedure TPocketSphinxAnyWordForm.FormDestroy(Sender: TObject);
begin
  PocketSphinx.Active := False;
  PocketSphinx.Free;
end;

procedure TPocketSphinxAnyWordForm.OnPocketSphinxStateChange(Sender: TObject;
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

procedure TPocketSphinxAnyWordForm.OnPocketSphinxHypothesis(Sender: TObject;
  AScore: Integer; AHypothesis: String);
begin
  if cbClearOnHypothesis.Checked then
    memHypothesis.Clear;

  memHypothesis.Lines.Add(AHypothesis);
end;

end.

