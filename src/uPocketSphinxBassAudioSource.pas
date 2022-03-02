unit uPocketSphinxBassAudioSource;

{ TPocketSphinx
Copyright (c) 2017 Damian Woroch, http://r1me.pl }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, uPocketSphinx, bass;

type

  { TBassAudioSource }

  TBassAudioSource = class(TPocketSphinxAudioSource)
  private
    FBassInitialized: Boolean;
    FRecognizer: TPocketSphinx;
    FChan: HRECORD;
    FInputSourceIdx: Integer;
    FInputSources: TStringList;
    function InitBASS(ADeviceIdx: Integer): Boolean;
    procedure FreeBASS;
    procedure ListInputSources;
    procedure SetInputSourceIdx(ANewIdx: Integer);
  public
    property InputSources: TStringList read FInputSources;
    property InputSourceIdx: Integer read FInputSourceIdx write SetInputSourceIdx;

    function GetData(buffer: Pointer; nmax: Cardinal): Integer; override;

    constructor Create(ARecognizer: TPocketSphinx; ADeviceIdx: Integer);
    destructor Destroy; override;
  end;

function GetMicrophoneDeviceIdx: Integer;

implementation

function HIWORD(l : longint) : WORD;
begin
  HIWORD:=WORD(((DWORD(l)) shr 16) and $FFFF);
end;

{ TBassAudioSource }

function GetMicrophoneDeviceIdx: Integer;
var
  i: Integer;
  info: BASS_DEVICEINFO;
begin
  Result := -1;
  i := 0;
  while BASS_RecordGetDeviceInfo(i, info) do
  begin
    if ((info.flags and BASS_DEVICE_ENABLED) = BASS_DEVICE_ENABLED) and
       ((info.flags and BASS_DEVICE_TYPE_MASK) = BASS_DEVICE_TYPE_MICROPHONE) then
    begin
      Result := i;
      Break;
    end;

    Inc(i);
  end;
end;

function TBassAudioSource.InitBASS(ADeviceIdx: Integer): Boolean;
begin
  Result := False;

  // Check if correct version of BASS.DLL was loaded
  if (HIWORD(BASS_GetVersion) <> BASSVERSION) then
    Exit;

  // Initialize audio device - 16000hz, mono, 16 bits
  Result := BASS_RecordInit(ADeviceIdx) and
    BASS_Init(ADeviceIdx, 16000, BASS_DEVICE_MONO or BASS_DEVICE_FREQ, GetCurrentThreadID, nil);

  FBassInitialized := Result;
end;

procedure TBassAudioSource.FreeBASS;
begin
  BASS_Stop;
  BASS_RecordFree;
  BASS_Free;
  FBassInitialized := False;
end;

procedure TBassAudioSource.ListInputSources;
var
  i: Integer;
  inName: PAnsiChar;
  level: Single;
begin
  i := 0;
  inName := BASS_RecordGetInputName(i);
  while inName <> nil do
  begin
    FInputSources.Add(StrPas(inName));
    if (BASS_RecordGetInput(i, level) and BASS_INPUT_OFF) = 0 then
      FInputSourceIdx := i;
    Inc(i);
    inName := BASS_RecordGetInputName(i);
  end;
end;

procedure TBassAudioSource.SetInputSourceIdx(ANewIdx: Integer);
var
  i: Integer;
  r: Boolean;
begin
  FInputSourceIdx := ANewIdx;

  r := True;
  i := 0;
  while r do
  begin
    r := BASS_RecordSetInput(i, BASS_INPUT_OFF, -1);
    Inc(i);
  end;
  BASS_RecordSetInput(FInputSourceIdx, BASS_INPUT_ON, -1);
end;

function TBassAudioSource.GetData(buffer: Pointer; nmax: Cardinal): Integer;
begin
  Result := Integer(BASS_ChannelGetData(FChan, buffer, nmax * SizeOf(Int16)));

  if Result < 0 then
  begin
    if BASS_ErrorGetCode = BASS_ERROR_ENDED then
      Result := 0;
  end else
    Result := Result div 2;
end;

constructor TBassAudioSource.Create(ARecognizer: TPocketSphinx; ADeviceIdx: Integer);
begin
  FReady := False;
  FRecognizer := ARecognizer;
  FInputSourceIdx := -1;
  FInputSources := TStringList.Create;

  if InitBASS(ADeviceIdx) then
  begin
    ListInputSources;
    FChan := BASS_RecordStart(16000, 1, 0, nil, nil);
    FReady := FChan <> 0;
  end;
end;

destructor TBassAudioSource.Destroy;
begin
  FreeBASS;
  FInputSources.Free;
  inherited Destroy;
end;

end.

