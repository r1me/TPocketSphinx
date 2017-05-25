unit uPocketSphinxDefaultAudioSource;

{ TPocketSphinx
Copyright (c) 2017 Damian Woroch, http://r1me.pl }

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, uPocketSphinx, ad;

type

  { TAudioSourceDefaultDevice }

  TAudioSourceDefaultDevice = class(TPocketSphinxAudioSource)
  private
    FAudioDevice: pad_rec_t;
    FSampleRate: Integer;
  public
    function GetData(buffer: Pointer; nmax: Cardinal): Integer; override;

    constructor Create(ADeviceName: AnsiString = '');
    destructor Destroy; override;
  end;

implementation

{ TAudioSourceDefaultDevice }

function TAudioSourceDefaultDevice.GetData(buffer: Pointer; nmax: Cardinal): Integer;
begin
  Result := ad_read(FAudioDevice, buffer, nmax);
end;

constructor TAudioSourceDefaultDevice.Create(ADeviceName: AnsiString = '');
begin
  FReady := False;
  FSampleRate := DEFAULT_SAMPLES_PER_SEC;

  if ADeviceName <> '' then
    FAudioDevice := ad_open_dev(PAnsiChar(ADeviceName), FSampleRate)
  else
    FAudioDevice := ad_open_dev(nil, FSampleRate);

  if Assigned(FAudioDevice) then
    if (ad_start_rec(FAudioDevice) = 0) then
      FReady := True;
end;

destructor TAudioSourceDefaultDevice.Destroy;
begin
  if Assigned(FAudioDevice) then
    ad_close(FAudioDevice);
  inherited Destroy;
end;

end.

