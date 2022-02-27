unit ad;

{ TPocketSphinx
Copyright (c) 2017 Damian Woroch, http://r1me.pl }

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  dynlibs;

const
{$IFDEF Linux}
  sphinxadlib = 'libsphinxad.so.3';
{$ENDIF}
{$IFDEF Windows}
  sphinxadlib = 'sphinxbase.dll';
{$ENDIF}

{$IFDEF FPC}
type
  PUTF8Char = PAnsiChar;
{$ENDIF}

const
  DEFAULT_SAMPLES_PER_SEC = 16000;

{* Return codes *}
const
  AD_OK	= 0;
  AD_EOF = -1;
  AD_ERR_GEN = -1;
  AD_ERR_NOT_OPEN = -2;
  AD_ERR_WAVE = -3;

type
  ad_rec_s = record
    sps: Integer;		//**< Samples/sec */
    bps: Integer;		//**< Bytes/sample */
  end;
  ad_rec_t = ad_rec_s;
  pad_rec_t = ^ad_rec_t;

type
  {**
  * Open a specific audio device for recording.
  *
  * The device is opened in non-blocking mode and placed in idle state.
  *
  * @return pointer to read-only ad_rec_t structure if successful, NULL
  * otherwise.  The return value to be used as the first argument to
  * other recording functions.
  *}
  Tad_open_dev = function (const dev: PUTF8Char; samples_per_sec: Integer): pad_rec_t; cdecl;

  {**
  * Open the default audio device with a given sampling rate.
  *}
  Tad_open_sps = function (samples_per_sec: Integer): pad_rec_t; cdecl;

  {**
  * Open the default audio device.
  *}
  Tad_open = function : pad_rec_t; cdecl;

  {* Start audio recording.  Return value: 0 if successful, <0 otherwise *}
  Tad_start_rec = function (rec: pad_rec_t): Integer; cdecl;

  {* Stop audio recording.  Return value: 0 if successful, <0 otherwise *}
  Tad_stop_rec = function (rec: pad_rec_t): Integer; cdecl;

  {* Close the recording device.  Return value: 0 if successful, <0 otherwise *}
  Tad_close = function(rec: pad_rec_t): Integer; cdecl;

  {*
  * Read next block of audio samples while recording; read upto max samples into buf.
  * Return value: # samples actually read (could be 0 since non-blocking); -1 if not
  * recording and no more samples remaining to be read from most recent recording.
  *}
  Tad_read = function (rec: pad_rec_t; buf: Pointer; max: Integer): Integer; cdecl;

var
  Lib: TLibHandle;
  ad_open_dev: Tad_open_dev;
  ad_open_sps: Tad_open_sps;
  ad_open: Tad_open;
  ad_start_rec: Tad_start_rec;
  ad_stop_rec: Tad_stop_rec;
  ad_close: Tad_close;
  ad_read: Tad_read;

implementation

procedure LoadLibrary;
begin
  Lib := DynLibs.LoadLibrary(sphinxadlib);
  if Lib <> DynLibs.NilHandle then
  begin
    ad_open_dev := GetProcAddress(Lib, 'ad_open_dev');
    ad_open_sps := GetProcAddress(Lib, 'ad_open_sps');
    ad_open := GetProcAddress(Lib, 'ad_open');
    ad_start_rec := GetProcAddress(Lib, 'ad_start_rec');
    ad_stop_rec := GetProcAddress(Lib, 'ad_stop_rec');
    ad_close := GetProcAddress(Lib, 'ad_close');
    ad_read := GetProcAddress(Lib, 'ad_read');
  end;
end;

initialization
  LoadLibrary;

end.
