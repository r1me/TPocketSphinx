unit ps_search;

{ TPocketSphinx
Copyright (c) 2017 Damian Woroch, http://r1me.pl }

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface
uses
  pocketsphinx, dynlibs;

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
  Tps_set_search = function(ps: pps_decoder_t; name: PUTF8Char): Integer; cdecl;

{**
 * Returns name of curent search in decoder
 *
 * @see ps_set_search
 *}
  Tps_get_search = function(ps: pps_decoder_t): PUTF8Char; cdecl;

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
  Tps_unset_search = function (ps: pps_decoder_t; name: PUTF8Char): Integer; cdecl;

{**
 * Returns iterator over current searches
 *
 * @see ps_set_search
 *}
  Tps_search_iter = function (ps: pps_decoder_t): pps_search_iter_t; cdecl;

{**
 * Updates search iterator to point to the next position.
 *
 * This function automatically frees the iterator object upon reaching
 * the final entry.
 * @see ps_set_search
 *}
  Tps_search_iter_next = function (itor: pps_search_iter_t): pps_search_iter_t; cdecl;

{**
 * Retrieves the name of the search the iterator points to.
 *
 * @see ps_set_search
 *}
  Tps_search_iter_val = function (itor: pps_search_iter_t): PUTF8Char; cdecl;

{**
 * Delete an unfinished search iterator
 *
 * @see ps_set_search
 *}
  Tps_search_iter_free = procedure (itor: pps_search_iter_t); cdecl;

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
  Tps_get_lm = function (ps: pps_decoder_t; name: PUTF8Char): pngram_model_t; cdecl;

{**
 * Adds new search based on N-gram language model.
 *
 * Associates N-gram search with the provided name. The search can be activated
 * using ps_set_search().
 *
 * @see ps_set_search.
 *}
  Tps_set_lm = function (ps: pps_decoder_t; name: PUTF8Char; lm: pngram_model_t): Integer; cdecl;

{**
 * Adds new search based on N-gram language model.
 *
 * Convenient method to load N-gram model and create a search.
 *
 * @see ps_set_lm
 *}
  Tps_set_lm_file = function (ps: pps_decoder_t; name: PUTF8Char; path: PUTF8Char): Integer; cdecl;

{**
 * Get the finite-state grammar set object for this decoder.
 *
 * If FSG decoding is not enabled, this returns NULL.  Call
 * ps_set_fsgset() to enable it.
 *
 * @return The current FSG set object for this decoder, or
 *         NULL if none is available.
 *}
  Tps_get_fsg = function (ps: pps_decoder_t; name: PUTF8Char): pfsg_model_t; cdecl;

{**
 * Adds new search based on finite state grammar.
 *
 * Associates FSG search with the provided name. The search can be activated
 * using ps_set_search().
 *
 * @see ps_set_search
 *}
  Tps_set_fsg = function (ps: pps_decoder_t; name: PUTF8Char; fsg: pfsg_model_t): Integer; cdecl;

{**
 * Adds new search using JSGF model.
 *
 * Convenient method to load JSGF model and create a search.
 *
 * @see ps_set_fsg
 *}
  Tps_set_jsgf_file = function (ps: pps_decoder_t; name: PUTF8Char; path: PUTF8Char): Integer; cdecl;

{**
 * Adds new search using JSGF model.
 *
 * Convenience method to parse JSGF model from string and create a search.
 *
 * @see ps_set_fsg
 *}
  Tps_set_jsgf_string = function (ps: pps_decoder_t; name: PUTF8Char; jsgf_string: PUTF8Char): Integer; cdecl;

{**
 * Get the current Key phrase to spot
 *
 * If KWS is not enabled, this returns NULL. Call
 * ps_update_kws() to enable it.
 *
 * @return The current keyphrase to spot
 *}
  Tps_get_kws = function (ps: pps_decoder_t; name: PUTF8Char): PUTF8Char; cdecl;

{**
 * Adds keywords from a file to spotting
 *
 * Associates KWS search with the provided name. The search can be activated
 * using ps_set_search().
 *
 * @see ps_set_search
 *}
  Tps_set_kws = function (ps: pps_decoder_t; name: PUTF8Char; keyfile: PUTF8Char): Integer; cdecl;

{**
 * Adds new keyword to spot
 *
 * Associates KWS search with the provided name. The search can be activated
 * using ps_set_search().
 *
 * @see ps_set_search
 *}
  Tps_set_keyphrase = function (ps: pps_decoder_t; name: PUTF8Char; keyphrase: PUTF8Char): Integer; cdecl;

{**
 * Adds new search based on phone N-gram language model.
 *
 * Associates N-gram search with the provided name. The search can be activated
 * using ps_set_search().
 *
 * @see ps_set_search.
 *}
  Tps_set_allphone = function (ps: pps_decoder_t; name: PUTF8Char; lm: pngram_model_t): Integer; cdecl;

{**
 * Adds new search based on phone N-gram language model.
 *
 * Convenient method to load N-gram model and create a search.
 *
 * @see ps_set_allphone
 *}
  Tps_set_allphone_file = function (ps: pps_decoder_t; name: PUTF8Char; path: PUTF8Char): Integer; cdecl;

var
  Lib: TLibHandle;
  ps_set_search: Tps_set_search;
  ps_get_search: Tps_get_search;
  ps_unset_search: Tps_unset_search;
  ps_search_iter: Tps_search_iter;
  ps_search_iter_next: Tps_search_iter_next;
  ps_search_iter_val: Tps_search_iter_val;
  ps_search_iter_free: Tps_search_iter_free;
  ps_get_lm: Tps_get_lm;
  ps_set_lm: Tps_set_lm;
  ps_set_lm_file: Tps_set_lm_file;
  ps_get_fsg: Tps_get_fsg;
  ps_set_fsg: Tps_set_fsg;
  ps_set_jsgf_file: Tps_set_jsgf_file;
  ps_set_jsgf_string: Tps_set_jsgf_string;
  ps_get_kws: Tps_get_kws;
  ps_set_kws: Tps_set_kws;
  ps_set_keyphrase: Tps_set_keyphrase;
  ps_set_allphone: Tps_set_allphone;
  ps_set_allphone_file: Tps_set_allphone_file;

implementation

procedure LoadLibrary;
begin
  Lib := DynLibs.LoadLibrary(pocketsphinxlib);
  if Lib <> DynLibs.NilHandle then
  begin
    ps_set_search := GetProcAddress(Lib, 'ps_set_search');
    ps_get_search := GetProcAddress(Lib, 'ps_get_search');
    ps_unset_search := GetProcAddress(Lib, 'ps_unset_search');
    ps_search_iter := GetProcAddress(Lib, 'ps_search_iter');
    ps_search_iter_next := GetProcAddress(Lib, 'ps_search_iter_next');
    ps_search_iter_val := GetProcAddress(Lib, 'ps_search_iter_val');
    ps_search_iter_free := GetProcAddress(Lib, 'ps_search_iter_free');
    ps_get_lm := GetProcAddress(Lib, 'ps_get_lm');
    ps_set_lm := GetProcAddress(Lib, 'ps_set_lm');
    ps_set_lm_file := GetProcAddress(Lib, 'ps_set_lm_file');
    ps_get_fsg := GetProcAddress(Lib, 'ps_get_fsg');
    ps_set_fsg := GetProcAddress(Lib, 'ps_set_fsg');
    ps_set_jsgf_file := GetProcAddress(Lib, 'ps_set_jsgf_file');
    ps_set_jsgf_string := GetProcAddress(Lib, 'ps_set_jsgf_string');
    ps_get_kws := GetProcAddress(Lib, 'ps_get_kws');
    ps_set_kws := GetProcAddress(Lib, 'ps_set_kws');
    ps_set_keyphrase := GetProcAddress(Lib, 'ps_set_keyphrase');
    ps_set_allphone := GetProcAddress(Lib, 'ps_set_allphone');
    ps_set_allphone_file := GetProcAddress(Lib, 'ps_set_allphone_file');
  end;
end;

initialization
  LoadLibrary;

end.
