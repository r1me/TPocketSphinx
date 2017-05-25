unit uPocketSphinxDelphiForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  uPocketSphinx, uPocketSphinxDefaultAudioSource;

type
  TPocketSphinxDelphiForm = class(TForm)
    lbStatus: TLabel;
    cbClearOnHypothesis: TCheckBox;
    memHypothesis: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    PocketSphinx: TPocketSphinx;

    procedure OnPocketSphinxStateChange(Sender: TObject; AState: TPocketSphinxState);
    procedure OnPocketSphinxHypothesis(Sender: TObject; AScore: Integer; AHypothesis: String);
  end;

var
  PocketSphinxDelphiForm: TPocketSphinxDelphiForm;

implementation

{$R *.dfm}

{ TPocketSphinxAnyWordForm }

procedure TPocketSphinxDelphiForm.FormCreate(Sender: TObject);
begin
  PocketSphinx := TPocketSphinx.Create;

  PocketSphinx.OnStateChange := OnPocketSphinxStateChange;
  PocketSphinx.OnHypothesis := OnPocketSphinxHypothesis;

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

procedure TPocketSphinxDelphiForm.FormDestroy(Sender: TObject);
begin
  PocketSphinx.Active := False;
  PocketSphinx.Free;  
end;

procedure TPocketSphinxDelphiForm.OnPocketSphinxStateChange(Sender: TObject;
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

procedure TPocketSphinxDelphiForm.OnPocketSphinxHypothesis(Sender: TObject;
  AScore: Integer; AHypothesis: String);
begin
  if cbClearOnHypothesis.Checked then
    memHypothesis.Clear;

  memHypothesis.Lines.Add(AHypothesis);
end;  

end.
