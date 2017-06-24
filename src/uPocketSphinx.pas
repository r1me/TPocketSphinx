unit uPocketSphinx;

{
TPocketSphinx
Copyright (c) 2017 Damian Woroch, http://r1me.pl

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE. 
}

interface
uses
  Classes, SysUtils, SyncObjs, pocketsphinx, ps_search, cmd_ln;

type
  TPocketSphinxState = (
    rsNotInitialized, // Initial state
    rsInitialized, // Indicates both decoder and audio device initialized successfully
    rsReady, // Waiting for audio input
    rsListening, // "In-speech"
    rsAnalyze, // Recognizer is analyzing provided audio data
    rsError // Error state, LastErrorMsg property stores the error message
  );
  
type
  TOnPocketSphinxStateChange = procedure(Sender: TObject; AState: TPocketSphinxState) of object;
  TOnPocketSphinxHypothesis = procedure(Sender: TObject; AScore: Integer; AHypothesis: String) of object;

type
  TPocketSphinxAudioSource = class
  protected
    FReady: Boolean;
  public
    property Ready: Boolean read FReady;
    function GetData(buffer: Pointer; nmax: Cardinal): Integer; virtual; abstract;
  end;

type

  { TPocketSphinx }

  TPocketSphinx = class(TThread)
  protected
    FDecoder: pps_decoder_t;
    FConfig: pcmd_ln_t;

    procedure Execute; override;
  private
    FLastErrorMsg: String;

    FState: TPocketSphinxState;
    FActive: Boolean;
    FThreshold: Integer;

    FAcousticModelPath: String;

    FAudioSource: TPocketSphinxAudioSource;

    FRecentHypothesis: String;
    FHypothesisScore: Integer;

    FOnStateChange: TOnPocketSphinxStateChange;
    FOnHypothesis: TOnPocketSphinxHypothesis;

    procedure SetState(ANewState: TPocketSphinxState);
    procedure SetFatalError(AMsg: String); inline;

    procedure SendState;
    procedure SendHypothesis;

    procedure SetActive(ANewValue: Boolean);

    procedure SetActiveSearch(SearchName: String);
    function GetActiveSearch: String;
    procedure AssignAudioSource(AudioSource: TPocketSphinxAudioSource);
  public
    property LastErrorMsg: String read FLastErrorMsg;

    property State: TPocketSphinxState read FState write SetState;
    property Active: Boolean read FActive write SetActive;
    property ActiveSearch: String read GetActiveSearch write SetActiveSearch;
    property Threshold: Integer read FThreshold write FThreshold;
    property AudioSource: TPocketSphinxAudioSource read FAudioSource write AssignAudioSource;

    property AcousticModelPath: String read FAcousticModelPath write FAcousticModelPath;

    property OnStateChange: TOnPocketSphinxStateChange read FOnStateChange write FOnStateChange;
    property OnHypothesis: TOnPocketSphinxHypothesis read FOnHypothesis write FOnHypothesis;

    function AddKeyphraseSearch(SearchName: String; KeyPhrase: String): Boolean;
    function AddKeyphraseSearchFile(SearchName: String; FileName: String): Boolean;
    function AddGrammarSearch(SearchName: String; JSGFString: String): Boolean;
    function AddGrammarSearchFile(SearchName: String; FileName: String): Boolean;
    function AddNgramSearch(SearchName: String; FileName: String): Boolean;
    function AddAllphoneSearch(SearchName: String; FileName: String): Boolean;

    function LoadDictionary(FileName: String): Boolean;

    procedure Init;

    constructor Create;
    destructor Destroy; override;
  end;

const
  DEFAULT_RECO_THRESHOLD = -2500;

implementation

{ TPocketSphinx }

constructor TPocketSphinx.Create;
begin
  inherited Create(True);
  FLastErrorMsg := '';
  FThreshold := DEFAULT_RECO_THRESHOLD;

  State := rsNotInitialized;
end;

destructor TPocketSphinx.Destroy;
begin
  if Assigned(FDecoder) then
    ps_free(FDecoder);
  if Assigned(FConfig) then
    cmd_ln_free_r(FConfig);
  if Assigned(FAudioSource) then
    FAudioSource.Free;

  inherited Destroy;
end;

procedure TPocketSphinx.SetFatalError(AMsg: String);
begin
  FLastErrorMsg := AMsg;
  State := rsError;
end;

procedure TPocketSphinx.SetActive(ANewValue: Boolean);
begin
  FActive := ANewValue;
  if FActive and Suspended then Start;
end;

procedure TPocketSphinx.SetState(ANewState: TPocketSphinxState);
begin
  if FState <> ANewState then
  begin
    FState := ANewState;
    Queue({$IFDEF FPC}@{$ENDIF}SendState);
  end;
end;

function TPocketSphinx.GetActiveSearch: String;
begin
  Result := String(UTF8Decode(ps_get_search(FDecoder)));
end;

procedure TPocketSphinx.SetActiveSearch(SearchName: String);
begin
  ps_set_search(FDecoder, PUTF8Char(UTF8Encode(SearchName)));
end;

procedure TPocketSphinx.SendState;
begin
  if Assigned(FOnStateChange) then
    OnStateChange(Self, FState);
end;

procedure TPocketSphinx.SendHypothesis;
begin
  if Assigned(FOnHypothesis) then
    OnHypothesis(Self, FHypothesisScore, FRecentHypothesis);
end;

function TPocketSphinx.AddKeyphraseSearch(SearchName: String;
  KeyPhrase: String): Boolean;
begin
  Result := ps_set_keyphrase(FDecoder, PUTF8Char(UTF8Encode(SearchName)), PUTF8Char(UTF8Encode(KeyPhrase))) = 0;
end;

function TPocketSphinx.AddKeyphraseSearchFile(SearchName: String;
  FileName: String): Boolean;
begin
  Result := ps_set_kws(FDecoder, PUTF8Char(UTF8Encode(SearchName)), PUTF8Char(UTF8Encode(FileName))) = 0;
end;

function TPocketSphinx.AddGrammarSearch(SearchName: String;
  JSGFString: String): Boolean;
begin
  Result := ps_set_jsgf_string(FDecoder, PUTF8Char(UTF8Encode(SearchName)), PUTF8Char(UTF8Encode(JSGFString))) = 0;
end;

function TPocketSphinx.AddGrammarSearchFile(SearchName: String;
  FileName: String): Boolean;
begin
  Result := ps_set_jsgf_file(FDecoder, PUTF8Char(UTF8Encode(SearchName)), PUTF8Char(UTF8Encode(FileName))) = 0;
end;

function TPocketSphinx.AddNgramSearch(SearchName: String; FileName: String
  ): Boolean;
begin
  Result := ps_set_lm_file(FDecoder, PUTF8Char(UTF8Encode(SearchName)), PUTF8Char(UTF8Encode(FileName))) = 0;
end;

function TPocketSphinx.AddAllphoneSearch(SearchName: String; FileName: String
  ): Boolean;
begin
  Result := ps_set_allphone_file(FDecoder, PUTF8Char(UTF8Encode(SearchName)), PUTF8Char(UTF8Encode(FileName))) = 0;
end;

function TPocketSphinx.LoadDictionary(FileName: String): Boolean;
begin
  Result := ps_load_dict(FDecoder, PUTF8Char(UTF8Encode(FileName)), nil, nil) = 0;
end;

procedure TPocketSphinx.Execute;
var
  utt_started, in_speech: Boolean;
  nframes: Integer;
  adbuf: array[0..2048-1] of SmallInt;
  hyp: PUTF8Char;
  score: Integer;
  delayev: TSimpleEvent;
begin
  if (ps_start_utt(FDecoder) < 0) then
    SetFatalError('Failed to start utterance');

  utt_started := False;
  State := rsReady;
  in_speech := False;
  nframes := 0;

  delayev := TSimpleEvent.Create;
  try
    while not Terminated do
    begin
      if FActive then
      begin
        if Assigned(FAudioSource) then
          if FAudioSource.Ready then
            nframes := FAudioSource.GetData(@adbuf[0], 2048);

        if nframes > 0 then
        begin
          ps_process_raw(FDecoder, @adbuf[0], nframes, False, False);
          in_speech := ps_get_in_speech(FDecoder);
        end else
          if nframes < 0 then Break;

        if (in_speech and (not utt_started)) then
        begin
          utt_started := True;
          State := rsListening;
        end;

        if ((not in_speech) and utt_started) then
        begin
          {* speech -> silence transition, time to start new utterance  *}
          ps_end_utt(FDecoder);

          State := rsAnalyze;
          if (FThreshold <> 0) then
          begin
            hyp := ps_get_hyp(FDecoder, score);
            if score < FThreshold then
              hyp := nil;
          end else
            hyp := ps_get_hyp_final(FDecoder, score);

          if (hyp <> nil) then
          begin
            FRecentHypothesis := String(UTF8Decode(hyp));
            FHypothesisScore := score;

            Synchronize({$IFDEF FPC}@{$ENDIF}SendHypothesis);
          end;

          if (ps_start_utt(FDecoder) < 0) then
            SetFatalError('Failed to start utterance');
          utt_started := False;
          State := rsReady;
        end;
      end;
      delayev.WaitFor(20);
    end;
  finally
    delayev.Free;
  end;
end;

procedure TPocketSphinx.Init;
begin
  FConfig := cmd_ln_init(nil, ps_args(), True
    {$IFDEF Debug}, PUTF8Char(UTF8Encode('-logfn')), PUTF8Char(UTF8Encode('ps.log')){$ENDIF},
    PUTF8Char(UTF8Encode('-hmm')), PUTF8Char(UTF8Encode(FAcousticModelPath)),
    nil);

  if not Assigned(FConfig) then
  begin
    SetFatalError('Failed to create configuration');
    Exit;
  end;

  ps_default_search_args(FConfig);
  FDecoder := ps_init(FConfig);
  if not Assigned(FDecoder) then
  begin
    SetFatalError('Failed to initialize decoder');
    Exit;
  end;

  State := rsInitialized;
end;

procedure TPocketSphinx.AssignAudioSource(AudioSource: TPocketSphinxAudioSource
  );
begin
  if Assigned(FAudioSource) then
    FAudioSource.Free;

  FAudioSource := AudioSource;
end;

end.
