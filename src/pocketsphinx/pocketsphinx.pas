unit pocketsphinx;

{ TPocketSphinx
Copyright (c) 2017 Damian Woroch, http://r1me.pl }

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface
uses
  cmd_ln;

const
{$IFDEF Linux}
  pocketsphinxlib = 'libpocketsphinx.so.3';
{$ENDIF}
{$IFDEF Windows}
  pocketsphinxlib = 'pocketsphinx.dll';
{$ENDIF}

{$IFDEF FPC}
type
  PUTF8Char = PAnsiChar;
{$ENDIF}

type
  pps_decoder_t = Pointer;

type
  plogmath_t = Pointer;
  pfe_t = Pointer;
  pfeat_t = Pointer;
  pps_mllr_t = Pointer;
  pmfcc_t = Pointer;
  pps_lattice_t = Pointer;
  pps_seg_t = Pointer;
  pps_nbest_t = Pointer;

{**
 * Sets default grammar and language model if they are not set explicitly and
 * are present in the default search path.
 *}
procedure ps_default_search_args(pcmd_ln_t: pcmd_ln_t); cdecl; external pocketsphinxlib;

{**
 * Initialize the decoder from a configuration object.
 *
 * @note The decoder retains ownership of the pointer
 * <code>config</code>, so if you are not going to use it
 * elsewere, you can free it.
 *
 * @param config a command-line structure, as created by
 * cmd_ln_parse_r() or cmd_ln_parse_file_r().
 *}
function ps_init(config: pcmd_ln_t): pps_decoder_t; cdecl; external pocketsphinxlib;

{**
 * Reinitialize the decoder with updated configuration.
 *
 * This function allows you to switch the acoustic model, dictionary,
 * or other configuration without creating an entirely new decoding
 * object.
 *
 * @note The decoder retains ownership of the pointer
 * <code>config</code>, so you must not attempt to free it manually.
 * If you wish to reuse it elsewhere, call cmd_ln_retain() on it.
 *
 * @param ps Decoder.
 * @param config An optional new configuration to use.  If this is
 *               NULL, the previous configuration will be reloaded,
 *               with any changes applied.
 * @return 0 for success, <0 for failure.
 *}
function ps_reinit(ps: pps_decoder_t; config: pcmd_ln_t): Integer; cdecl; external pocketsphinxlib;

{**
 * Returns the argument definitions used in ps_init().
 *
 * This is here to avoid exporting global data, which is problematic
 * on Win32 and Symbian (and possibly other platforms).
 *}
function ps_args: parg_t; cdecl external pocketsphinxlib;

{**
 * Retain a pointer to the decoder.
 *
 * This increments the reference count on the decoder, allowing it to
 * be shared between multiple parent objects.  In general you will not
 * need to use this function, ever.  It is mainly here for the
 * convenience of scripting language bindings.
 *
 * @return pointer to retained decoder.
 *}
function ps_retain(ps: pps_decoder_t): pps_decoder_t; cdecl; external pocketsphinxlib;

{**
 * Finalize the decoder.
 *
 * This releases all resources associated with the decoder, including
 * any language models or grammars which have been added to it, and
 * the initial configuration object passed to ps_init().
 *
 * @param ps Decoder to be freed.
 * @return New reference count (0 if freed).
 *}
function ps_free(ps: pps_decoder_t): Integer; cdecl; external pocketsphinxlib;

{**
 * Get the configuration object for this decoder.
 *
 * @return The configuration object for this decoder.  The decoder
 *         retains ownership of this pointer, so you should not
 *         attempt to free it manually.  Use cmd_ln_retain() if you
 *         wish to reuse it elsewhere.
 *}
function ps_get_config(ps: pps_decoder_t): pcmd_ln_t; cdecl; external pocketsphinxlib;

{**
 * Get the log-math computation object for this decoder.
 *
 * @return The log-math object for this decoder.  The decoder retains
 *         ownership of this pointer, so you should not attempt to
 *         free it manually.  Use logmath_retain() if you wish to
 *         reuse it elsewhere.
 *}
function ps_get_logmath(ps: pps_decoder_t): plogmath_t; cdecl external pocketsphinxlib;

{**
 * Get the feature extraction object for this decoder.
 *
 * @return The feature extraction object for this decoder.  The
 *         decoder retains ownership of this pointer, so you should
 *         not attempt to free it manually.  Use fe_retain() if you
 *         wish to reuse it elsewhere.
 *}
function ps_get_fe(ps: pps_decoder_t): pfe_t; cdecl external pocketsphinxlib;

{**
 * Get the dynamic feature computation object for this decoder.
 *
 * @return The dynamic feature computation object for this decoder.  The
 *         decoder retains ownership of this pointer, so you should
 *         not attempt to free it manually.  Use feat_retain() if you
 *         wish to reuse it elsewhere.
 *}
function ps_get_feat(ps: pps_decoder_t): pfeat_t; cdecl external pocketsphinxlib;

{**
 * Adapt current acoustic model using a linear transform.
 *
 * @param mllr The new transform to use, or NULL to update the existing
 *              transform.  The decoder retains ownership of this pointer,
 *              so you should not attempt to free it manually.  Use
 *              ps_mllr_retain() if you wish to reuse it
 *              elsewhere.
 * @return The updated transform object for this decoder, or
 *         NULL on failure.
 *}
function ps_update_mllr(ps: pps_decoder_t; mllr: pps_mllr_t): pps_mllr_t; cdecl external pocketsphinxlib;

{**
 * Reload the pronunciation dictionary from a file.
 *
 * This function replaces the current pronunciation dictionary with
 * the one stored in dictfile.  This also causes the active search
 * module(s) to be reinitialized, in the same manner as calling
 * ps_add_word() with update=TRUE.
 *
 * @param dictfile Path to dictionary file to load.
 * @param fdictfile Path to filler dictionary to load, or NULL to keep
 *                  the existing filler dictionary.
 * @param format Format of the dictionary file, or NULL to determine
 *               automatically (currently unused,should be NULL)
 *}
function ps_load_dict(ps: pps_decoder_t;
                      const dictfile, fdictfile, format: PUTF8Char): Integer; cdecl; external pocketsphinxlib;

{**
 * Dump the current pronunciation dictionary to a file.
 *
 * This function dumps the current pronunciation dictionary to a tex
 *
 * @param dictfile Path to file where dictionary will be written.
 * @param format Format of the dictionary file, or NULL for the
 *               default (text) format (currently unused, should be NULL)
 *}
function ps_save_dict(ps: pps_decoder_t;
                      const dictfile, format: PUTF8Char): Integer; cdecl; external pocketsphinxlib;

{**
 * Add a word to the pronunciation dictionary.
 *
 * This function adds a word to the pronunciation dictionary and the
 * current language model (but, obviously, not to the current FSG if
 * FSG mode is enabled).  If the word is already present in one or the
 * other, it does whatever is necessary to ensure that the word can be
 * recognized.
 *
 * @param word Word string to add.
 * @param phones Whitespace-separated list of phoneme strings
 *               describing pronunciation of <code>word</code>.
 * @param update If TRUE, update the search module (whichever one is
 *               currently active) to recognize the newly added word.
 *               If adding multiple words, it is more efficient to
 *               pass FALSE here in all but the last word.
 * @return The internal ID (>= 0) of the newly added word, or <0 on
 *         failure.
 *}
function ps_add_word(ps: pps_decoder_t;
                     const word, phones: PUTF8Char;
                     update: Integer): Integer; cdecl; external pocketsphinxlib;

{**
 * Lookup for the word in the dictionary and return phone transcription
 * for it.
 *
 * @param ps Pocketsphinx decoder
 * @param word Word to look for
 *
 * @return Whitespace-spearated phone string describing the pronunciation of the <code>word</code>
 *         or NULL if word is not present in the dictionary. The string is
 *         allocated and must be freed by the user.
 *}
function ps_lookup_word(ps: pps_decoder_t; const word: PUTF8Char): PUTF8Char; cdecl; external pocketsphinxlib;

{**
 * Decode a raw audio stream.
 *
 * No headers are recognized in this files.  The configuration
 * parameters <tt>-samprate</tt> and <tt>-input_endian</tt> are used
 * to determine the sampling rate and endianness of the stream,
 * respectively.  Audio is always assumed to be 16-bit signed PCM.
 *
 * @param ps Decoder.
 * @param rawfh Previously opened file stream.
 * @param maxsamps Maximum number of samples to read from rawfh, or -1
 *                 to read until end-of-file.
 * @return Number of samples of audio.
 *}
function ps_decode_raw(ps: pps_decoder_t; rawfh: Cardinal; maxsamps: Integer): Integer; cdecl; external pocketsphinxlib;

{**
 * Decode a senone score dump file.
 *
 * @param ps Decoder
 * @param fh Previously opened file handle positioned at start of file.
 * @return Number of frames read.
 *}
function ps_decode_senscr(ps: pps_decoder_t; senfh: THandle): Integer; cdecl external pocketsphinxlib;

{**
 * Start processing of the stream of speech. Channel parameters like
 * noise-level are maintained for the stream and reused among utterances.
 * Times returned in segment iterators are also stream-wide.
 *
 * @return 0 for success, <0 on error.
 *}
function ps_start_stream(ps: pps_decoder_t): Integer; cdecl; external pocketsphinxlib;

{**
 * Start utterance processing.
 *
 * This function should be called before any utterance data is passed
 * to the decoder.  It marks the start of a new utterance and
 * reinitializes internal data structures.
 *
 * @param ps Decoder to be started.
 * @return 0 for success, <0 on error.
 *}
function ps_start_utt(ps: pps_decoder_t): Integer; cdecl; external pocketsphinxlib;

{**
 * Decode raw audio data.
 *
 * @param ps Decoder.
 * @param no_search If non-zero, perform feature extraction but don't
 *                  do any recognition yet.  This may be necessary if
 *                  your processor has trouble doing recognition in
 *                  real-time.
 * @param full_utt If non-zero, this block of data is a full utterance
 *                 worth of data.  This may allow the recognizer to
 *                 produce more accurate results.
 * @return Number of frames of data searched, or <0 for error.
 *}
function ps_process_raw(ps: pps_decoder_t;
                        const data: Pointer;
                        n_samples: NativeInt;
                        no_search: LongBool;
                        full_utt: LongBool): Integer; cdecl; external pocketsphinxlib;

{**
 * Decode acoustic feature data.
 *
 * @param ps Decoder.
 * @param no_search If non-zero, perform feature extraction but don't
 *                  do any recognition yet.  This may be necessary if
 *                  your processor has trouble doing recognition in
 *                  real-time.
 * @param full_utt If non-zero, this block of data is a full utterance
 *                 worth of data.  This may allow the recognizer to
 *                 produce more accurate results.
 * @return Number of frames of data searched, or <0 for error.
 *}
function ps_process_cep(ps: pps_decoder_t;
                        data: pmfcc_t;
                        n_frames: Integer;
                        no_search: Integer;
                        full_utt: Integer): Integer; cdecl external pocketsphinxlib;

{**
 * Get the number of frames of data searched.
 *
 * Note that there is a delay between this and the number of frames of
 * audio which have been input to the system.  This is due to the fact
 * that acoustic features are computed using a sliding window of
 * audio, and dynamic features are computed over a sliding window of
 * acoustic features.
 *
 * @param ps Decoder.
 * @return Number of frames of speech data which have been recognized
 * so far.
 *}
function ps_get_n_frames(ps: pps_decoder_t): Integer; cdecl; external pocketsphinxlib;

{**
 * End utterance processing.
 *
 * @param ps Decoder.
 * @return 0 for success, <0 on error
 *}
function ps_end_utt(ps: pps_decoder_t): Integer; cdecl; external pocketsphinxlib;

{**
 * Get hypothesis string and path score.
 *
 * @param ps Decoder.
 * @param out_best_score Output: path score corresponding to returned string.
 * @return String containing best hypothesis at this point in
 *         decoding.  NULL if no hypothesis is available.
 *}
function ps_get_hyp(ps: pps_decoder_t; out out_best_score: Integer): PUTF8Char; cdecl; external pocketsphinxlib;

{**
 * Get hypothesis string and final flag.
 *
 * @param ps Decoder.
 * @param out_is_best_score Output: if hypothesis is reached final state in the grammar.
 * @return String containing best hypothesis at this point in
 *         decoding.  NULL if no hypothesis is available.
 *}
function ps_get_hyp_final(ps: pps_decoder_t; out out_is_final: Integer): PUTF8Char; cdecl; external pocketsphinxlib;

{**
 * Get posterior probability.
 *
 * @note Unless the -bestpath option is enabled, this function will
 * always return zero (corresponding to a posterior probability of
 * 1.0).  Even if -bestpath is enabled, it will also return zero when
 * called on a partial result.  Ongoing research into effective
 * confidence annotation for partial hypotheses may result in these
 * restrictions being lifted in future versions.
 *
 * @param ps Decoder.
 * @return Posterior probability of the best hypothesis.
 *}
function ps_get_prob(ps: pps_decoder_t): Integer; cdecl external pocketsphinxlib;

{**
 * Get word lattice.
 *
 * There isn't much you can do with this so far, a public API will
 * appear in the future.
 *
 * @param ps Decoder.
 * @return Word lattice object containing all hypotheses so far.  NULL
 *         if no hypotheses are available.  This pointer is owned by
 *         the decoder and you should not attempt to free it manually.
 *         It is only valid until the next utterance, unless you use
 *         ps_lattice_retain() to retain it.
 *}
function ps_get_lattice(ps: pps_decoder_t): pps_lattice_t; cdecl external pocketsphinxlib;

{**
 * Get an iterator over the word segmentation for the best hypothesis.
 *
 * @param ps Decoder.
 * @return Iterator over the best hypothesis at this point in
 *         decoding.  NULL if no hypothesis is available.
 *}
function ps_seg_iter(ps: pps_decoder_t): pps_seg_t; cdecl external pocketsphinxlib;

{**
 * Get the next segment in a word segmentation.
 *
 * @param seg Segment iterator.
 * @return Updated iterator with the next segment.  NULL at end of
 *         utterance (the iterator will be freed in this case).
 *}
function ps_seg_next(seg: pps_seg_t): pps_seg_t; cdecl external pocketsphinxlib;

{**
 * Get word string from a segmentation iterator.
 *
 * @param seg Segment iterator.
 * @return Read-only string giving string name of this segment.  This
 * is only valid until the next call to ps_seg_next().
 *}
function ps_seg_word(seg: pps_seg_t): PUTF8Char; cdecl external pocketsphinxlib;

{**
 * Get inclusive start and end frames from a segmentation iterator.
 *
 * @note These frame numbers are inclusive, i.e. the end frame refers
 * to the last frame in which the given word or other segment was
 * active.  Therefore, the actual duration is *out_ef - *out_sf + 1.
 *
 * @param seg Segment iterator.
 * @param out_sf Output: First frame index in segment.
 * @param out_sf Output: Last frame index in segment.
 *}
procedure ps_seg_frames(seg: pps_seg_t; var out_sf: Integer; var out_ef: Integer); cdecl external pocketsphinxlib;

{**
 * Get language, acoustic, and posterior probabilities from a
 * segmentation iterator.
 *
 * @note Unless the -bestpath option is enabled, this function will
 * always return zero (corresponding to a posterior probability of
 * 1.0).  Even if -bestpath is enabled, it will also return zero when
 * called on a partial result.  Ongoing research into effective
 * confidence annotation for partial hypotheses may result in these
 * restrictions being lifted in future versions.
 *
 * @param out_ascr Output: acoustic model score for this segment.
 * @param out_lscr Output: language model score for this segment.
 * @param out_lback Output: language model backoff mode for this
 *                  segment (i.e. the number of words used in
 *                  calculating lscr).  This field is, of course, only
 *                  meaningful for N-Gram models.
 * @return Log posterior probability of current segment.  Log is
 *         expressed in the log-base used in the decoder.  To convert
 *         to linear floating-point, use logmath_exp(ps_get_logmath(),
 *         pprob).
 *}
function ps_seg_prob(seg: pps_seg_t;
                     var out_ascr: Integer;
                     var out_lscr: Integer;
                     var out_lback: Integer): Integer; cdecl external pocketsphinxlib;

{**
 * Finish iterating over a word segmentation early, freeing resources.
 *}
procedure ps_seg_free(seg: pps_seg_t); cdecl external pocketsphinxlib;

{**
 * Get an iterator over the best hypotheses. The function may also
 * return a NULL which means that there is no hypothesis available for this
 * utterance.
 *
 * @param ps Decoder.
 * @return Iterator over N-best hypotheses or NULL if no hypothesis is available
 *}
function ps_nbest(ps: pps_decoder_t): pps_nbest_t; cdecl external pocketsphinxlib;

{**
 * Move an N-best list iterator forward.
 *
 * @param nbest N-best iterator.
 * @return Updated N-best iterator, or NULL if no more hypotheses are
 *         available (iterator is freed ni this case).
 *}
function ps_nbest_next(nbest: pps_nbest_t): pps_nbest_t; cdecl external pocketsphinxlib;

{**
 * Get the hypothesis string from an N-best list iterator.
 *
 * @param nbest N-best iterator.
 * @param out_score Output: Path score for this hypothesis.
 * @return String containing next best hypothesis.
 *}
function ps_nbest_hyp(nbest: pps_nbest_t; var out_score: Integer): PUTF8Char; cdecl external pocketsphinxlib;

{**
 * Get the word segmentation from an N-best list iterator.
 *
 * @param nbest N-best iterator.
 * @param out_score Output: Path score for this hypothesis.
 * @return Iterator over the next best hypothesis.
 *}
function ps_nbest_seg(nbest: pps_nbest_t): pps_seg_t; cdecl external pocketsphinxlib;

{**
 * Finish N-best search early, releasing resources.
 *
 * @param nbest N-best iterator.
 *}
procedure ps_nbest_free(nbest: pps_nbest_t); cdecl external pocketsphinxlib;

{**
 * Get performance information for the current utterance.
 *
 * @param ps Decoder.
 * @param out_nspeech Output: Number of seconds of speech.
 * @param out_ncpu    Output: Number of seconds of CPU time used.
 * @param out_nwall   Output: Number of seconds of wall time used.
 *}
procedure ps_get_utt_time(ps: pps_decoder_t;
                          var out_nspeech: double;
                          var out_ncpu: double;
                          var out_nwall: double); cdecl external pocketsphinxlib;

{**
 * Get overall performance information.
 *
 * @param ps Decoder.
 * @param out_nspeech Output: Number of seconds of speech.
 * @param out_ncpu    Output: Number of seconds of CPU time used.
 * @param out_nwall   Output: Number of seconds of wall time used.
 *}
procedure ps_get_all_time(ps: pps_decoder_t;
                          var out_nspeech: double;
                          var out_ncpu: double;
                          var out_nwall: double); cdecl external pocketsphinxlib;

{**
 * Checks if the last feed audio buffer contained speech
 *
 * @param ps Decoder.
 * @return 1 if last buffer contained speech, 0 - otherwise
 *}
function ps_get_in_speech(ps: pps_decoder_t): ByteBool; cdecl external pocketsphinxlib;

{**
 * Sets the limit of the raw audio data to store in decoder
 * to retrieve it later on ps_get_rawdata.
 *
 * @param ps Decoder
 * @param size bytes of the utterance to store
 *}
procedure ps_set_rawdata_size(ps: pps_decoder_t; size: Integer); cdecl external pocketsphinxlib;

{**
 * Retrieves the raw data collected during utterance decoding.
 *
 * @param ps Decoder
 * @param buffer preallocated buffer to store the data, must be within the limit
 * set before
 * @param size size of the data collected in samples (not bytes).
 *}
procedure ps_get_rawdata(ps: pps_decoder_t; buffer: Pointer; var size: Integer); cdecl external pocketsphinxlib;

implementation

end.
