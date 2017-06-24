unit ps_search;

{ TPocketSphinx
Copyright (c) 2017 Damian Woroch, http://r1me.pl }

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface
uses
  pocketsphinx;

type
  pps_search_iter_t = Pointer;
  pngram_model_t = Pointer;
  pfsg_model_t = Pointer;

{**
 * Actives search with the provided name.
 *
 * Activates search with the provided name. The search must be added before
 * using either ps_set_fsg(), ps_set_lm() or ps_set_kws().
 *
 * @return 0 on success, -1 on failure
 *}
function ps_set_search(ps: pps_decoder_t; name: PUTF8Char): Integer; cdecl; external pocketsphinxlib;

{**
 * Returns name of curent search in decoder
 *
 * @see ps_set_search
 *}
function ps_get_search(ps: pps_decoder_t): PUTF8Char; cdecl; external pocketsphinxlib;

{**
 * Unsets the search and releases related resources.
 *
 * Unsets the search previously added with
 * using either ps_set_fsg(), ps_set_lm() or ps_set_kws().
 *
 * @see ps_set_fsg
 * @see ps_set_lm
 * @see ps_set_kws
 *}
function ps_unset_search(ps: pps_decoder_t; name: PUTF8Char): Integer; cdecl; external pocketsphinxlib;

{**
 * Returns iterator over current searches
 *
 * @see ps_set_search
 *}
function ps_search_iter(ps: pps_decoder_t): pps_search_iter_t; cdecl; external pocketsphinxlib;

{**
 * Updates search iterator to point to the next position.
 *
 * This function automatically frees the iterator object upon reaching
 * the final entry.
 * @see ps_set_search
 *}
function ps_search_iter_next(itor: pps_search_iter_t): pps_search_iter_t; cdecl; external pocketsphinxlib;

{**
 * Retrieves the name of the search the iterator points to.
 *
 * @see ps_set_search
 *}
function ps_search_iter_val(itor: pps_search_iter_t): PUTF8Char; cdecl; external pocketsphinxlib;

{**
 * Delete an unfinished search iterator
 *
 * @see ps_set_search
 *}
procedure ps_search_iter_free(itor: pps_search_iter_t); cdecl; external pocketsphinxlib;

{**
 * Get the language model set object for this decoder.
 *
 * If N-Gram decoding is not enabled, this will return NULL.  You will
 * need to enable it using ps_set_lmset().
 *
 * @return The language model set object for this decoder.  The
 *         decoder retains ownership of this pointer, so you should
 *         not attempt to free it manually.  Use ngram_model_retain()
 *         if you wish to reuse it elsewhere.
 *}
function ps_get_lm(ps: pps_decoder_t; name: PUTF8Char): pngram_model_t; cdecl; external pocketsphinxlib;

{**
 * Adds new search based on N-gram language model.
 *
 * Associates N-gram search with the provided name. The search can be activated
 * using ps_set_search().
 *
 * @see ps_set_search.
 *}
function ps_set_lm(ps: pps_decoder_t; name: PUTF8Char; lm: pngram_model_t): Integer; cdecl; external pocketsphinxlib;

{**
 * Adds new search based on N-gram language model.
 *
 * Convenient method to load N-gram model and create a search.
 *
 * @see ps_set_lm
 *}
function ps_set_lm_file(ps: pps_decoder_t; name: PUTF8Char; path: PUTF8Char): Integer; cdecl; external pocketsphinxlib;

{**
 * Get the finite-state grammar set object for this decoder.
 *
 * If FSG decoding is not enabled, this returns NULL.  Call
 * ps_set_fsgset() to enable it.
 *
 * @return The current FSG set object for this decoder, or
 *         NULL if none is available.
 *}
function ps_get_fsg(ps: pps_decoder_t; name: PUTF8Char): pfsg_model_t; cdecl; external pocketsphinxlib;

{**
 * Adds new search based on finite state grammar.
 *
 * Associates FSG search with the provided name. The search can be activated
 * using ps_set_search().
 *
 * @see ps_set_search
 *}
function ps_set_fsg(ps: pps_decoder_t; name: PUTF8Char; fsg: pfsg_model_t): Integer; cdecl; external pocketsphinxlib;

{**
 * Adds new search using JSGF model.
 *
 * Convenient method to load JSGF model and create a search.
 *
 * @see ps_set_fsg
 *}
function ps_set_jsgf_file(ps: pps_decoder_t; name: PUTF8Char; path: PUTF8Char): Integer; cdecl; external pocketsphinxlib;

{**
 * Adds new search using JSGF model.
 *
 * Convenience method to parse JSGF model from string and create a search.
 *
 * @see ps_set_fsg
 *}
function ps_set_jsgf_string(ps: pps_decoder_t; name: PUTF8Char; jsgf_string: PUTF8Char): Integer; cdecl; external pocketsphinxlib;

{**
 * Get the current Key phrase to spot
 *
 * If KWS is not enabled, this returns NULL. Call
 * ps_update_kws() to enable it.
 *
 * @return The current keyphrase to spot
 *}
function ps_get_kws(ps: pps_decoder_t; name: PUTF8Char): PUTF8Char; cdecl; external pocketsphinxlib;

{**
 * Adds keywords from a file to spotting
 *
 * Associates KWS search with the provided name. The search can be activated
 * using ps_set_search().
 *
 * @see ps_set_search
 *}
function ps_set_kws(ps: pps_decoder_t; name: PUTF8Char; keyfile: PUTF8Char): Integer; cdecl; external pocketsphinxlib;

{**
 * Adds new keyword to spot
 *
 * Associates KWS search with the provided name. The search can be activated
 * using ps_set_search().
 *
 * @see ps_set_search
 *}
function ps_set_keyphrase(ps: pps_decoder_t; name: PUTF8Char; keyphrase: PUTF8Char): Integer; cdecl; external pocketsphinxlib;

{**
 * Adds new search based on phone N-gram language model.
 *
 * Associates N-gram search with the provided name. The search can be activated
 * using ps_set_search().
 *
 * @see ps_set_search.
 *}
function ps_set_allphone(ps: pps_decoder_t; name: PUTF8Char; lm: pngram_model_t): Integer; cdecl; external pocketsphinxlib;

{**
 * Adds new search based on phone N-gram language model.
 *
 * Convenient method to load N-gram model and create a search.
 *
 * @see ps_set_allphone
 *}
function ps_set_allphone_file(ps: pps_decoder_t; name: PUTF8Char; path: PUTF8Char): Integer; cdecl; external pocketsphinxlib;

implementation

end.
