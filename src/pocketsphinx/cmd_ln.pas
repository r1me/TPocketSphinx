unit cmd_ln;

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
  sphinxbaselib = 'libsphinxbase.so.3';
{$ENDIF}
{$IFDEF Windows}
  sphinxbaselib = 'sphinxbase.dll';
{$ENDIF}

type
  parg_t = Pointer;
  pcmd_ln_t = Pointer;

{**
 * Create a cmd_ln_t from NULL-terminated list of arguments.
 *
 * This function creates a cmd_ln_t from a NULL-terminated list of
 * argument strings.  For example, to create the equivalent of passing
 * "-hmm foodir -dsratio 2 -lm bar.lm" on the command-line:
 *
 *  config = cmd_ln_init(NULL, defs, TRUE, "-hmm", "foodir", "-dsratio", "2",
 *                       "-lm", "bar.lm", NULL);
 *
 * Note that for simplicity, <strong>all</strong> arguments are passed
 * as strings, regardless of the actual underlying type.
 *
 * @param inout_cmdln Previous command-line to update, or NULL to create a new one.
 * @param defn Array of argument name definitions, or NULL to allow any arguments.
 * @param strict Whether to fail on duplicate or unknown arguments.
 * @return A cmd_ln_t* containing the results of command line parsing, or NULL on failure.
 *}
  Tcmd_ln_init = function(inout_cmdln: pcmd_ln_t; defn: parg_t; bfailunk: Boolean): pcmd_ln_t; cdecl varargs;

{**
 * Release a command-line argument set and all associated strings.
 *
 * @return new reference count (0 if freed completely)
 *}
  Tcmd_ln_free_r = function(cmdln: pcmd_ln_t): Integer; cdecl;

var
  Lib: TLibHandle;
  cmd_ln_init: Tcmd_ln_init;
  cmd_ln_free_r: Tcmd_ln_free_r;

implementation

procedure LoadLibrary;
begin
  Lib := DynLibs.LoadLibrary(sphinxbaselib);
  if Lib <> DynLibs.NilHandle then
  begin
    cmd_ln_init := GetProcAddress(Lib, 'cmd_ln_init');
    cmd_ln_free_r := GetProcAddress(Lib, 'cmd_ln_free_r');
  end;
end;

initialization
  LoadLibrary;

end.

