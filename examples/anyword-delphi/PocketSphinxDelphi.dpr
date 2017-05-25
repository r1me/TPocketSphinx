program PocketSphinxDelphi;

uses
  Forms,
  uPocketSphinxDelphiForm in 'uPocketSphinxDelphiForm.pas' {PocketSphinxDelphiForm},
  uPocketSphinx in '..\..\src\uPocketSphinx.pas',
  ad in '..\..\src\pocketsphinx\ad.pas',
  cmd_ln in '..\..\src\pocketsphinx\cmd_ln.pas',
  pocketsphinx in '..\..\src\pocketsphinx\pocketsphinx.pas',
  ps_search in '..\..\src\pocketsphinx\ps_search.pas',
  uPocketSphinxDefaultAudioSource in '..\..\src\uPocketSphinxDefaultAudioSource.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPocketSphinxDelphiForm, PocketSphinxDelphiForm);
  Application.Run;
end.
