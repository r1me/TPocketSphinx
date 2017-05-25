program PocketSphinxAnyWord;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uPocketSphinxAnyWordForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TPocketSphinxAnyWordForm, PocketSphinxAnyWordForm);
  Application.Run;
end.

