unit ad;

{ TPocketSphinx
Copyright (c) 2017 Damian Woroch, http://r1me.pl }

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

const
{$IFDEF Linux}
  sphinxadlib = 'libsphinxad.so.3';
{$ENDIF}
{$IFDEF Windows}
  sphinxadlib = 'sphinxbase.dll';
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

{**
 * Open a specific audio device for recording.
 *
 * The device is opened in non-blocking mode and placed in idle state.
 *
 * @return pointer to read-only ad_rec_t structure if successful, NULL
 * otherwise.  The return value to be used as the first argument to
 * other recording functions.
 *}
function ad_open_dev(const dev: PAnsiChar; samples_per_sec: Integer): pad_rec_t; cdecl; external sphinxadlib;

{**
 * Open the default audio device with a given sampling rate.
 *}
function ad_open_sps(samples_per_sec: Integer): pad_rec_t; cdecl; external sphinxadlib;

{**
 * Open the default audio device.
 *}
function ad_open: pad_rec_t; cdecl; external sphinxadlib;

{* Start audio recording.  Return value: 0 if successful, <0 otherwise *}
function ad_start_rec(rec: pad_rec_t): Integer; cdecl; external sphinxadlib;

{* Stop audio recording.  Return value: 0 if successful, <0 otherwise *}
function ad_stop_rec(rec: pad_rec_t): Integer; cdecl; external sphinxadlib;

{* Close the recording device.  Return value: 0 if successful, <0 otherwise *}
function ad_close(rec: pad_rec_t): Integer; cdecl; external sphinxadlib;

{*
 * Read next block of audio samples while recording; read upto max samples into buf.
 * Return value: # samples actually read (could be 0 since non-blocking); -1 if not
 * recording and no more samples remaining to be read from most recent recording.
 *}
function ad_read(rec: pad_rec_t; buf: Pointer; max: Integer): Integer; cdecl; external sphinxadlib;

implementation

end.
